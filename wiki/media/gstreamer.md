GStreamer
=========

GObject
    ╰──GInitiallyUnowned
        ╰──GstObject
            ╰──GstElement
                ╰──GstVideoEncoder
                    ╰──GstVaapiEncode
                        ╰──vaapih265enc

GObject
    ╰──GInitiallyUnowned
        ╰──GstObject
            ╰──GstElement
                ╰──GstBin
                    ╰──GstPipeline

GObject
    ╰──GInitiallyUnowned
        ╰──GstObject
            ╰──GstElement
                ╰──GstBaseSrc
                    ╰──GstAppSrc

GObject
    ╰──GInitiallyUnowned
        ╰──GstObject
            ╰──GstElement
                ╰──GstBaseSink
                    ╰──GstAppSink

GObject
    ╰──GInitiallyUnowned
        ╰──GstObject
            ╰──GstVaDisplay
                ╰──GstVaDisplayDrm
                ╰──GstVaDisplayWrapped

GObject
    ╰──GInitiallyUnowned
        ╰──GstObject
            ╰──GstBufferPool
                ╰──VaPool

GObject
    ╰──GInitiallyUnowned
        ╰──GstObject
            ╰──GstAllocator
                ╰──VaAllocator
