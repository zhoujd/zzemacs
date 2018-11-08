# Swap Caps_Lock and Control_L if necessary
#
# When the runlevel is 3 (full multiuser mode), loadkeys works
# When the runlevel is 5 (X11), xmodmap works

rl=`runlevel | awk '{print $2}'`
case $rl in
3)  loadkeys << "    EOF"
    keymaps 0-2,4-5,8,12
    keycode 29 = Caps_Lock
    keycode 58 = Control
    EOF
    ;;
5)  lock=`xmodmap | awk '/^lock/ { print $3 }'`
    test "$lock" = "(0x25)" || xmodmap - << "    EOF"
    remove Lock = Caps_Lock
    remove Control = Control_L
    keysym Control_L = Caps_Lock
    keysym Caps_Lock = Control_L
    add Lock = Caps_Lock
    add Control = Control_L
    EOF
    ;;
esac
