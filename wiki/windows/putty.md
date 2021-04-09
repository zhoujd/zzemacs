Putty resource
====================================

1. Useful command lines

        $ putty -sercfg 19200,8,n,1,N -serial "$@" &
        $ putty -X -ssh "$@" &
        $ putty -load x11-forward "$@" &

