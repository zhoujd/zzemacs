;;;; temp-setting.el --- develop setting file
;;

;;find ./gfx_Development/mainline ./gfx_Development/DEV/bzhao11/libva_staging_migration/libva ./gfx_ValMedia/ ~/work/vppguarder/  -name "*.[hc]" -print | etags -

(setq proj-list
      '(
        "~/work/vppguarder/"
        "~/work/putsurface-split/libva_ivb/gfx_ValMedia/"
        ;;"~/work/putsurface-split/libva_ivb/gfx_Development/DEV/bzhao11/libva_staging_migration/libva/"
        ;;"~/work/putsurface-split/libva_ivb/vpg-git.iind/libva"
        ;;"~/work/putsurface-split/libva_ivb/gfx_Development/mainline/"
        ;;"~/work/putsurface-split/libva_ivb/gfx_Development/releases/INT_15_31"
        "~/work/driver_src/libva/"
        "~/work/driver_src/mainline/"
        "~/work/driver_src/INT_15_31/"
        "~/work/mfx/"
        ;"~/work/drvtest/"
        ))

;; SET TAGS PATH
(setq tags-table-list
      '(
        "~/work/driver_src/TAGS"        ;;for mplayer and gfx
        ;;"~/work/putsurface-split/libva_ivb/gfx_Development/DEV/bzhao11/libva_staging_migration/TAGS"      ;;gfx
        ;;"~/work/putsurface-split/libva_ivb/gfx_ValMedia/DEV/DEV_mplayer/TAGS"                          ;;mplayer
        ;"~/work/mfx/TAGS"                                                                                 ;;for mfx
        ))

;; SET CSCOPE FILE PATH
(setq cscope-database-regexps
      '(
        ("~/work/driver_src/"  (t ("-q" "-d")) t)    ;;libva_ivb
        ("~/work/mfx/"         (t ("-q" "-d")) t)    ;;mfx
        ))

;; Add to PATH
(setq add-path-list
      '(
        "~/study/script"
        "~/work/mfx/__cmake/intel64.make.debug/__bin/debug"
        ;"~/work/tools/android-sdk-linux/platform-tools"                                          ;;android adb etc tools
        ;"~/work/android/jb-work-media/prebuilts/gcc/linux-x86/x86/i686-android-linux-4.4.3/bin"  ;;android compile tools
        ))

(mapcar 'zz-add-os-path add-path-list) 

;;for msdk
;(setenv "MEDIASDK_ROOT"     "~/work/mfx")
;(setenv "MEDIASDK_STREAMS"  (getenv "HOME"))
;(setenv "LD_LIBRARY_PATH"   (concat "~/work/mfx/build/lib" path-separator (getenv "LD_LIBRARY_PATH")))


;;perforce setting
;export P4PORT=192.168.4.88:1666  #P4所在的主机
;export P4CLIENT=dev-client       #指定了与perforce服务器交流的client是什么
;export P4USER=daihh              #P4用户名
;export P4PASSWD=123456           #P4密码
;export P4CHARSET=utf8            #调用命令时使用的字符集


;;liuth scp setting
;cmd: scp -r  liuth@10.239.140.211:mfx .
;pwd: 6

;;liuth ivb-server
;;ip 10.239.141.112
;;user:liuth
;;password:6
