// gpu_ttm_example.c - GPU TTM驱动示例
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/pci.h>
#include <drm/drm_device.h>
#include <drm/drm_file.h>
#include <drm/drm_managed.h>
#include <drm/drm_gem.h>
#include <drm/ttm/ttm_bo_driver.h>
#include <drm/ttm/ttm_placement.h>
#include <drm/ttm/ttm_range_manager.h>
#include <drm/ttm/ttm_pool.h>

/* GPU设备私有数据结构 */
struct gpu_example_device {
    struct drm_device ddev;               // DRM设备
    struct ttm_device bdev;               // TTM设备
    struct pci_dev *pdev;                 // PCI设备
    
    /* GPU硬件资源 */
    phys_addr_t vram_base;                // VRAM物理基址（PCI BAR0）
    resource_size_t vram_size;            // VRAM大小
    void __iomem *vram_mem;               // VRAM映射（WC属性）
    
    /* TTM相关 */
    struct ttm_pool pool;                 // 内存池
    u64 gpu_max_bo_order;                 // GPU最佳连续内存阶数
};

/* GPU缓冲区对象 */
struct gpu_example_bo {
    struct ttm_buffer_object tbo;         // TTM缓冲区对象
    struct gpu_example_device *gdev;      // 所属GPU设备
    
    /* GPU特定字段 */
    uint64_t gpu_addr;                    // GPU虚拟地址
    uint32_t gpu_flags;                   // GPU访问标志
};

/* 放置域定义 */
static const struct ttm_place vram_placement = {
    .mem_type = TTM_PL_VRAM,              // VRAM内存类型
    .flags = TTM_PL_FLAG_WC |             // 写组合（显存最佳实践）
             TTM_PL_FLAG_CONTIGUOUS,      // 连续物理内存（GPU常需）
};

static const struct ttm_place sys_placement = {
    .mem_type = TTM_PL_SYSTEM,            // 系统内存（后备存储）
    .flags = TTM_PL_FLAG_CACHED,          // 完全缓存
};

/* 组合放置（首选VRAM，繁忙时退到系统内存） */
static struct ttm_placement gpu_placement = {
    .num_placement = 1,
    .placement = &vram_placement,
    .num_busy_placement = 1,
    .busy_placement = &sys_placement,
};

/* 1. TTM后端函数 - 处理TT对象（Translation Table） */
static struct ttm_tt *gpu_ttm_tt_create(struct ttm_buffer_object *bo,
                                        uint32_t page_flags)
{
    struct ttm_tt *tt;
    
    tt = kzalloc(sizeof(*tt), GFP_KERNEL);
    if (!tt)
        return NULL;
    
    /* 初始化转换表（使用系统内存池） */
    if (ttm_tt_init(tt, bo, page_flags, ttm_cached, 0)) {
        kfree(tt);
        return NULL;
    }
    
    return tt;
}

static int gpu_ttm_tt_populate(struct ttm_device *bdev,
                               struct ttm_tt *ttm,
                               struct ttm_operation_ctx *ctx)
{
    /* 使用TTM池分配物理页面 */
    return ttm_pool_alloc(&bdev->pool, ttm, ctx);
}

static void gpu_ttm_tt_unpopulate(struct ttm_device *bdev,
                                  struct ttm_tt *ttm)
{
    ttm_pool_free(&bdev->pool, ttm);
}

static void gpu_ttm_tt_destroy(struct ttm_device *bdev,
                               struct ttm_tt *ttm)
{
    ttm_tt_fini(ttm);
    kfree(ttm);
}

/* 2. IO内存管理 - 处理VRAM的CPU访问 */
static int gpu_ttm_io_mem_reserve(struct ttm_device *bdev,
                                  struct ttm_resource *mem)
{
    struct gpu_example_device *gdev = 
        container_of(bdev, struct gpu_example_device, bdev);
    
    switch (mem->mem_type) {
    case TTM_PL_SYSTEM:
        /* 系统内存不需要IO内存映射 */
        return 0;
        
    case TTM_PL_VRAM: {
        /* VRAM通过PCI BAR映射到CPU */
        unsigned long offset = mem->start << PAGE_SHIFT;
        
        /* 检查边界 */
        if (offset + mem->num_pages * PAGE_SIZE > gdev->vram_size) {
            DRM_ERROR("VRAM access out of bounds\n");
            return -EINVAL;
        }
        
        /* 设置IO内存信息 */
        mem->bus.offset = offset;
        mem->bus.addr = (void __force *)gdev->vram_mem + offset;
        mem->bus.size = mem->num_pages << PAGE_SHIFT;
        mem->bus.caching = ttm_write_combined;  // 写组合提升GPU性能
        mem->bus.is_iomem = true;
        break;
    }
        
    default:
        return -EINVAL;
    }
    
    return 0;
}

static void gpu_ttm_io_mem_free(struct ttm_device *bdev,
                                struct ttm_resource *mem)
{
    /* 无需操作 */
}

/* 3. 内存迁移 - 在VRAM和系统内存间移动 */
static int gpu_bo_move(struct ttm_buffer_object *bo,
                       bool evict,
                       struct ttm_operation_ctx *ctx,
                       struct ttm_resource *new_mem,
                       struct ttm_place *hop)
{
    int ret;
    
    /* 使用memcpy进行迁移（硬件通常有DMA引擎） */
    ret = ttm_bo_move_memcpy(bo, ctx, new_mem);
    if (ret) {
        DRM_ERROR("Failed to move buffer: %d\n", ret);
        return ret;
    }
    
    return 0;
}

/* 4. 逐出判断 - 决定哪些缓冲区可被逐出 */
static bool gpu_eviction_valuable(struct ttm_buffer_object *bo,
                                  const struct ttm_place *place)
{
    /* 始终允许逐出，实际驱动会根据优先级判断 */
    return true;
}

/* 5. 逐出标志 - 被逐出时建议的放置位置 */
static void gpu_evict_flags(struct ttm_buffer_object *bo,
                            struct ttm_placement *placement)
{
    static struct ttm_place sys_pl = {
        .mem_type = TTM_PL_SYSTEM,
        .flags = TTM_PL_FLAG_CACHED
    };
    
    /* 从VRAM逐出时，建议移到系统内存 */
    placement->num_placement = 1;
    placement->placement = &sys_pl;
}

/* 6. 内存类型管理器初始化 */
static int gpu_init_mem_type(struct gpu_example_device *gdev)
{
    struct ttm_device *bdev = &gdev->bdev;
    int ret;
    
    /* 初始化系统内存管理器 */
    ret = ttm_range_man_init(bdev, TTM_PL_SYSTEM, false, -1);
    if (ret) {
        DRM_ERROR("Failed to init system memory manager\n");
        return ret;
    }
    
    /* 初始化VRAM管理器（如果GPU有专用显存） */
    if (gdev->vram_size > 0) {
        unsigned long num_pages = gdev->vram_size >> PAGE_SHIFT;
        
        DRM_INFO("Initializing VRAM: %zu MB (%lu pages)\n",
                 gdev->vram_size >> 20, num_pages);
        
        ret = ttm_range_man_init(bdev, TTM_PL_VRAM, false, num_pages);
        if (ret) {
            DRM_ERROR("Failed to init VRAM manager\n");
            return ret;
        }
        
        /* 配置VRAM缓存属性 */
        bdev->man_drv[TTM_PL_VRAM]->default_caching = TTM_PL_FLAG_WC;
        bdev->man_drv[TTM_PL_VRAM]->available_caching = 
            TTM_PL_FLAG_WC | TTM_PL_FLAG_UNCACHED;
    }
    
    return 0;
}

/* 7. TTM设备初始化 */
static int gpu_ttm_init(struct gpu_example_device *gdev)
{
    static struct ttm_device_funcs ttm_funcs = {
        .ttm_tt_create = gpu_ttm_tt_create,
        .ttm_tt_populate = gpu_ttm_tt_populate,
        .ttm_tt_unpopulate = gpu_ttm_tt_unpopulate,
        .ttm_tt_destroy = gpu_ttm_tt_destroy,
        .io_mem_reserve = gpu_ttm_io_mem_reserve,
        .io_mem_free = gpu_ttm_io_mem_free,
        .move = gpu_bo_move,
        .eviction_valuable = gpu_eviction_valuable,
        .evict_flags = gpu_evict_flags,
    };
    
    int ret;
    
    /* 初始化TTM核心 */
    ret = ttm_device_init(&gdev->bdev, &ttm_funcs, &gdev->pdev->dev,
                          gdev->ddev.anon_inode->i_mapping,
                          &gdev->ddev.vma_offset_manager,
                          false, true);
    if (ret) {
        DRM_ERROR("Failed to init TTM device: %d\n", ret);
        return ret;
    }
    
    /* 配置TTM池（针对GPU优化） */
    gdev->bdev.pool.use_dma_alloc = true;  // 使用DMA分配器保证连续性
    
    /* 配置GPU的最佳连续内存阶数（来自硬件能力） */
    if (gdev->gpu_max_bo_order > 0) {
        /* 设置TTM池的最大有益分配阶数 - 内核6.1+特性 */
        gdev->bdev.pool.flags |= TTM_POOL_BENEFICIAL_ORDER(gdev->gpu_max_bo_order);
        DRM_INFO("GPU benefits from contiguous memory up to order %llu\n",
                 gdev->gpu_max_bo_order);
    }
    
    /* 初始化内存类型管理器 */
    ret = gpu_init_mem_type(gdev);
    if (ret)
        goto err_fini;
    
    return 0;
    
err_fini:
    ttm_device_fini(&gdev->bdev);
    return ret;
}

/* 8. 缓冲区对象创建（核心功能） */
static int gpu_bo_create(struct gpu_example_device *gdev,
                         size_t size,
                         uint32_t flags,
                         struct gpu_example_bo **bo_out)
{
    struct gpu_example_bo *bo;
    struct ttm_operation_ctx ctx = {
        .interruptible = true,
        .no_wait_gpu = false,
    };
    int ret;
    
    bo = kzalloc(sizeof(*bo), GFP_KERNEL);
    if (!bo)
        return -ENOMEM;
    
    bo->gdev = gdev;
    bo->gpu_flags = flags;
    
    /* 初始化TTM缓冲区对象 */
    ret = ttm_bo_init(&gdev->bdev, &bo->tbo, size,
                      ttm_bo_type_device, &gpu_placement,
                      0, false, NULL, NULL, &ctx);
    if (ret) {
        DRM_ERROR("Failed to init TTM BO: %d\n", ret);
        kfree(bo);
        return ret;
    }
    
    /* 分配GPU虚拟地址（示例中简化处理） */
    bo->gpu_addr = 0x1000000; /* 实际驱动需从GPU VM分配 */
    
    *bo_out = bo;
    return 0;
}

/* 9. 缓冲区验证 - 确保在正确的位置 */
int gpu_bo_validate(struct gpu_example_bo *bo)
{
    struct ttm_operation_ctx ctx = {
        .interruptible = true,
        .no_wait_gpu = false,
    };
    int ret;
    
    /* 验证缓冲区位置（如需要，会触发迁移） */
    ret = ttm_bo_validate(&bo->tbo, &gpu_placement, &ctx);
    if (ret)
        return ret;
    
    /* 如果缓冲区在VRAM中，记录GPU地址 */
    if (bo->tbo.resource->mem_type == TTM_PL_VRAM) {
        /* 计算GPU可见的物理地址 */
        bo->gpu_addr = gdev->vram_base + 
                       (bo->tbo.resource->start << PAGE_SHIFT);
    }
    
    return 0;
}

/* 10. GPU命令提交前的准备工作 */
int gpu_bo_prepare_for_gpu(struct gpu_example_bo *bo)
{
    struct ttm_operation_ctx ctx = {
        .interruptible = true,
        .no_wait_gpu = false,
    };
    
    /* 确保缓冲区在VRAM中（GPU访问最快） */
    return ttm_bo_validate(&bo->tbo, &gpu_placement, &ctx);
}

/* 11. CPU映射 - 用于调试或回读 */
int gpu_bo_kmap(struct gpu_example_bo *bo, void **virt_addr)
{
    int ret;
    
    /* 验证缓冲区 */
    ret = gpu_bo_validate(bo);
    if (ret)
        return ret;
    
    /* 创建内核映射 */
    ret = ttm_bo_kmap(&bo->tbo, 0, bo->tbo.base.size >> PAGE_SHIFT,
                      &bo->tbo.map);
    if (ret)
        return ret;
    
    *virt_addr = ttm_kmap_obj_virtual(&bo->tbo.map, &bo->tbo.map.bo_kmap);
    return 0;
}

void gpu_bo_kunmap(struct gpu_example_bo *bo)
{
    ttm_bo_kunmap(&bo->tbo.map);
}

/* 12. PCI探测函数 */
static int gpu_example_pci_probe(struct pci_dev *pdev,
                                 const struct pci_device_id *id)
{
    struct gpu_example_device *gdev;
    int ret;
    
    /* 使用devm_drm_dev_alloc简化错误处理 */
    gdev = devm_drm_dev_alloc(&pdev->dev, NULL,
                              struct gpu_example_device, ddev);
    if (IS_ERR(gdev))
        return PTR_ERR(gdev);
    
    gdev->pdev = pdev;
    pci_set_drvdata(pdev, gdev);
    
    /* 启用PCI设备 */
    ret = pcim_enable_device(pdev);
    if (ret)
        return ret;
    
    /* 获取VRAM资源（PCI BAR 0 - 典型GPU配置） */
    gdev->vram_base = pci_resource_start(pdev, 0);
    gdev->vram_size = pci_resource_len(pdev, 0);
    
    if (gdev->vram_size == 0) {
        DRM_INFO("No dedicated VRAM, using UMA mode\n");
    } else {
        /* 映射VRAM（写组合优化GPU访问） */
        gdev->vram_mem = devm_ioremap_wc(&pdev->dev, gdev->vram_base,
                                         gdev->vram_size);
        if (!gdev->vram_mem) {
            DRM_ERROR("Failed to map VRAM\n");
            return -ENOMEM;
        }
    }
    
    /* 设置GPU硬件能力（示例值） */
    gdev->gpu_max_bo_order = 8;  /* 最大连续内存为 2^8 = 256页 = 1MB */
    
    /* 初始化TTM */
    ret = gpu_ttm_init(gdev);
    if (ret)
        return ret;
    
    DRM_INFO("GPU TTM Example initialized:\n"
             "  VRAM: %zu MB at %pa\n"
             "  Max beneficial order: %llu\n",
             gdev->vram_size >> 20,
             &gdev->vram_base,
             gdev->gpu_max_bo_order);
    
    return 0;
}

/* 13. 设备移除 */
static void gpu_example_pci_remove(struct pci_dev *pdev)
{
    struct gpu_example_device *gdev = pci_get_drvdata(pdev);
    
    /* 清理内存管理器 */
    ttm_range_man_fini(&gdev->bdev, TTM_PL_VRAM);
    ttm_range_man_fini(&gdev->bdev, TTM_PL_SYSTEM);
    
    /* 清理TTM设备 */
    ttm_device_fini(&gdev->bdev);
    
    DRM_INFO("GPU TTM Example removed\n");
}

/* PCI设备表（示例ID） */
static const struct pci_device_id gpu_example_id_table[] = {
    { PCI_DEVICE(0x1234, 0x5678) }, /* 示例GPU Vendor/Device ID */
    { 0, }
};
MODULE_DEVICE_TABLE(pci, gpu_example_id_table);

/* PCI驱动结构 */
static struct pci_driver gpu_example_pci_driver = {
    .name = "gpu_ttm_example",
    .id_table = gpu_example_id_table,
    .probe = gpu_example_pci_probe,
    .remove = gpu_example_pci_remove,
};

/* 模块入口 */
static int __init gpu_example_init(void)
{
    return pci_register_driver(&gpu_example_pci_driver);
}

static void __exit gpu_example_exit(void)
{
    pci_unregister_driver(&gpu_example_pci_driver);
}

module_init(gpu_example_init);
module_exit(gpu_example_exit);

MODULE_LICENSE("GPL");
MODULE_DESCRIPTION("GPU TTM Example Driver");
MODULE_AUTHOR("Your Name");
