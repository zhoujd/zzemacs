;;Let putty/mintty support more key-chords (such as Shift+F1, C-home)
;;by sending corresponding XTerm control sequences

;;all of these sequences are translated from term/xterm.el .
;;http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/term/xterm.el?h=emacs-23
;;if emacs can't create keymap correctly from you TERM 
;;you can force to load it
;; (if (and (not (display-graphic-p))
;;          (load-library "term/xterm"))
;;   (terminal-init-xterm))

;;with NumLock off, most ambiguous keys would act as Xterm R6 compatibile.
;;with NumLock on, thses keys would act as Putty default settings 
;;   (note: PuTTY's function key & keypad mode settings is not respected)


;;References:
;;http://code.google.com/p/mintty/wiki/Keycodes
;;http://www.xfree86.org/current/ctlseqs.html#PC-Style%20Function%20Keys
;;http://en.wikipedia.org/wiki/ANSI_escape_code

;;`input-decode-map' in GNU Emacs (or: M-[ C-h, M-O C-h)
;;http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/xterm-extras.el
;;http://emacswiki.org/emacs/PuTTY#toc9 Using Emacs over PuTTY: how to use all function keys
;;http://emacswiki.org/emacs/PuTTY#toc10 ;Windows PuTTY client: xterm broken

;;http://offbytwo.com/2012/01/15/emacs-plus-paredit-under-terminal.html
;;http://blog.akinori.org/2013/01/18/pasting-a-text-literally-to-emacs-under-terminal/
;;http://www.joshstaiger.org/archives/2005/04/fixing_the_righ.html



;;AutoHotkey:  Ctrl ^ Alt ! Shift +
;;XTerm control sequences: Shift 1, Alt 2;;Ctrl  4


;;putty/mintty/putty-nd/mobaxterm
#if WinActive("ahk_class PuTTY") or WinActive("ahk_class mintty") or WinActive("ahk_class SysTabControl32") or WinActive("ahk_class TMobaXtermForm") 

;;* ===== Fx ==========
;;in default/VT400/VT100+ mode, putty sends ESC [11~ .. ESC [14~  (CSI sequences) for F1..F4 
;;in Xterm R6 mode, putty sends ESC OP .. ESC OS (SS3 seqences) for F1..F4
;;in Linux or SCO mode... who cares?
;;http://the.earth.li/~sgtatham/putty/0.62/htmldoc/Chapter4.html#config-funkeys
;;
;;but xterm R6 & mintty/gnome-terminal/xfce-terminal send SS3 sequences
F1::
  if not GetKeyState("Numlock", "T")  
    SendInput {F1}
  else
    SendInput {Esc}OP
  return
F2::
  if not GetKeyState("Numlock", "T")  
    SendInput {F2}
  else
    SendInput {Esc}OQ
  return
F3::
  if not GetKeyState("Numlock", "T")  
    SendInput {F3}
  else
    SendInput {Esc}OR
  return
F4::
  if not GetKeyState("Numlock", "T")  
    SendInput {F4}
  else
    SendInput {Esc}OS
  return

;;For F5..F12, most terminals sends ESC [15~ .. ESC[24~ (CSI sequences)
 
;;** Shift+Fx
;;** Shift+Fx
;;for ctrl/shift/alt+f1/f2/f3/f4, xterm-r6, mintty emit CSI sequences

;;NOTE: term/xterm.el maps both CSI and SS3 sequences for S-f1..S-f4 
;;(but only SS3 for C-f1..C-f4, S-f1..S-f4)

;;NOTE: xfce4-terminal & newer gnome-terminal emit wrong sequences for C/S/M-f1..f4
;;       https://bugs.launchpad.net/ubuntu/+source/gnome-terminal/+bug/96676  
;;      (gnome-terminal 2.16 proves to be correct (emiting SS3 sequences)


+F1::
  if not GetKeyState("Numlock", "T")
    SendInput  {ESC}[1;2P
  else
    SendInput  {ESC}O2P
  return
+F2::
  if not GetKeyState("Numlock", "T")
    SendInput  {ESC}[1;2Q
  else
    SendInput  {ESC}O2Q
  return
+F3::
  if not GetKeyState("Numlock", "T")
    SendInput  {ESC}[1;2R
  else
    SendInput  {ESC}O2R
  return
+F4::
  if not GetKeyState("Numlock", "T")
    SendInput  {ESC}[1;2S
  else
    SendInput  {ESC}O2S
  return

+F5::SendInput  {Esc}[15;2~
+F6::SendInput  {Esc}[17;2~
+F7::SendInput  {Esc}[18;2~
+F8::SendInput  {Esc}[19;2~
+F9::SendInput  {Esc}[20;2~
+F10::SendInput {Esc}[21;2~
+F11::SendInput {Esc}[23;2~
+F12::SendInput {Esc}[24;2~

;;** Alt+Fx
;;** Alt+Fx
;;NOTE: When numlock off, M-f1..f4 not compatible with xterm when numlock off, 
;;because xterm emits CSI sequences for M-f1..M-f12
;;but term/xterm.el has only SS3 sequences for M-f1..M-f4, only CSI sequences for M-f5..M-f12

!F1::
  if GetKeyState("Numlock", "T")
    SendInput  {ESC}[1;3P
  else
    SendInput  {ESC}O3P
  return
!F2::
  if GetKeyState("Numlock", "T")
    SendInput  {ESC}[1;3Q
  else
    SendInput  {ESC}O3Q
  return
!F3::
  if GetKeyState("Numlock", "T")
    SendInput  {ESC}[1;3R
  else
    SendInput  {ESC}O3R
  return
!F4::
  if GetKeyState("Numlock", "T")
    SendInput  {ESC}[1;3S
  else
    SendInput  {ESC}O3S
  return

!F5::SendInput  {Esc}[15;3~
!F6::SendInput  {Esc}[17;3~
!F7::SendInput  {Esc}[18;3~
!F8::SendInput  {Esc}[19;3~
!F9::SendInput  {Esc}[20;3~
!F10::SendInput {Esc}[21;3~
!F11::SendInput {Esc}[23;3~
!F12::SendInput {Esc}[24;3~

;;** Alt+Shift+F1
;;FIXME
+!F1::SendInput  {ESC}O4P
+!F2::SendInput  {ESC}O4Q
+!F3::SendInput  {ESC}O4R
+!F4::SendInput  {ESC}O4S

;;** Ctrl+Fx
;;NOTE: C-f1..f4 not compatible with xterm when numlock off, because xterm emits CSI sequences for C-f1..C-f12
;;but term/xterm.el has only SS3 sequences for C-f1..C-f4, only CSI sequences for C-f5..C-f12

^F1::
  if GetKeyState("Numlock", "T")
    SendInput  {ESC}[1;5P
  else
    SendInput  {ESC}O5P
  return
^F2::
  if GetKeyState("Numlock", "T")
    SendInput  {ESC}[1;5Q
  else
    SendInput  {ESC}O5Q
  return
^F3::
  if GetKeyState("Numlock", "T")
    SendInput  {ESC}[1;5R
  else
    SendInput  {ESC}O5R
  return
^F4::
  if GetKeyState("Numlock", "T")
    SendInput  {ESC}[1;5S
  else
    SendInput  {ESC}O5S
  return

^F5::SendInput  {Esc}[15;5~
^F6::SendInput  {Esc}[17;5~
^F7::SendInput  {Esc}[18;5~
^F8::SendInput  {Esc}[19;5~
^F9::SendInput  {Esc}[20;5~
^F10::SendInput {Esc}[21;5~
^F11::SendInput {Esc}[23;5~
^F12::SendInput {Esc}[24;5~

;;** Ctrl+Shift+Fx
;;FIXME
^+F1::SendInput  {ESC}[1;6P
^+F2::SendInput  {ESC}[1;6Q
^+F3::SendInput  {ESC}[1;6R
^+F4::SendInput  {ESC}[1;6S

^+F5::SendInput  {Esc}[15;6~
^+F6::SendInput  {Esc}[17;6~
^+F7::SendInput  {Esc}[18;6~
^+F8::SendInput  {Esc}[19;6~
^+F9::SendInput  {Esc}[20;6~
^+F10::SendInput {Esc}[21;6~
^+F11::SendInput {Esc}[23;6~
^+F12::SendInput {Esc}[24;6~

;;** Ctrl+Alt+Fx
;;FIXME
^!F1::SendInput  {ESC}O7P
^!F2::SendInput  {ESC}O7Q
^!F3::SendInput  {ESC}O7R
^!F4::SendInput  {ESC}O7S

^!F5::SendInput  {Esc}[15;7~
^!F6::SendInput  {Esc}[17;7~
^!F7::SendInput  {Esc}[18;7~
^!F8::SendInput  {Esc}[19;7~
^!F9::SendInput  {Esc}[20;7~
^!F10::SendInput {Esc}[21;7~
^!F11::SendInput {Esc}[23;7~
^!F12::SendInput {Esc}[24;7~


;;* ===== Cursor keys ==========
;;Cursor keycodes without modifier keys depend on whether "application cursor key mode" 
;;http://the.earth.li/~sgtatham/putty/0.62/htmldoc/Chapter4.html#config-appcursor
;;http://code.google.com/p/mintty/wiki/Keycodes#Cursor_keys


;;NOTE: when numlock off, not xterm compatible, most terminals emit ^[OH or ^[[1~
;;      (xterm/mintty emit ^[[H, but term/xterm.el has no mapping for ^[[H)

Home::
  if GetKeyState("Numlock", "T") 
    ;; putty way
    SendInput {Esc}[1~
  else
    ;; xterm 102/220 / gnome-terminal way
    SendInput {Esc}OH
  return
End::
  if GetKeyState("Numlock", "T")  
    SendInput {Esc}[4~
  else
    SendInput {Esc}OF
  return


;;** Shift+...

;;NOTE: old gnome-terminal (at least 2.16) emits strange sequences for C/S/M-up/down/left/right/
;;      (e.g. Shift-left emits ^[[2D  but SS3 sequences is ^[O2D, CSI sequences is ^[[1;2D)
+Up::SendInput    {Esc}[1;2A
+Down::SendInput  {Esc}[1;2B
+Left::SendInput  {Esc}[1;2D
+Right::SendInput {Esc}[1;2C

+Home::SendInput  {ESC}[1;2H
+End::SendInput   {ESC}[1;2F
;;Other versions of xterm might emit these.
;;+Up::SendInput    {Esc}O2A
;;+Down::SendInput  {Esc}O2B
;;+Left::SendInput  {Esc}O2D
;;+Right::SendInput {Esc}O2C
;;+Home::SendInput  {ESC}O2H
;;+End::SendInput   {ESC}O2F


;;** Alt+...
;;Alt+Up/Down/Left/Right/ work fine
;;!Up::SendInput {ESC}[1;3A

;;** Alt+Shift...
+!Up::SendInput {ESC}[1;4A
+!Down::SendInput {ESC}[1;4B

;;** Ctrl+..
^Up::SendInput    {Esc}[1;5A
^Down::SendInput  {Esc}[1;5B
^Left::SendInput  {Esc}[1;5D
^Right::SendInput {Esc}[1;5C
^Home::SendInput  {ESC}[1;5H
^End::SendInput   {ESC}[1;5F


;;** Ctrl+Shif+
^+Up::SendInput    {Esc}[1;6A
^+Down::SendInput  {Esc}[1;6B
^+Left::SendInput  {Esc}[1;6D
^+Right::SendInput {Esc}[1;6C
^+Home::SendInput  {ESC}[1;6H
^+End::SendInput   {ESC}[1;6F


;;* ====== Ins/Del ==========
;;* Shift+...
;;^+nsert::SendInput {Esc}[2;5~    (Windows: paste from clipboard)
+Delete::SendInput {Esc}[3;2~
;;+PGUP::SendInput {Esc}[5;2~
;;+PGDN::SendInput {Esc}[6;2~

;;** Ctrl+...
;;^Insert::SendInput {Esc}[2;5~    (Windows: copy selection)
^Delete::SendInput {Esc}[3;5~
^PGUP::SendInput {Esc}[5;5~
^PGDN::SendInput {Esc}[6;5~

;;** Alt+
;;Alft+Ins/Del/Home/End/PgUp/PgDwn work fine


;;* ====== some punctions ==========
;;Among Linux terminal emulators, only available on xterm>216, and need 
;;special sequences to turn on this feature.
;;emacs term/xterm.el would detect xterm version and let it turn on 

;;NOTE: by default, Ctrl+Shift is used for switching betweenn  different input methods
;; to press C-!, C-# etc, maybe you need to disable this (or use other key combos)

;;C-` (C-@ ?)
^!::SendInput {Esc}[27;6;33~
;;C-@
^#::SendInput {Esc}[27;6;35~
^$::SendInput {Esc}[27;6;36~
^%::SendInput {Esc}[27;6;37~
;;C-^
^&::SendInput {Esc}[27;6;38~
^*::SendInput {Esc}[27;6;42~
^(::SendInput {Esc}[27;6;40~
^)::SendInput {Esc}[27;6;41~

;;C-_ (undo)
^-::SendInput {Esc}[27;5;45~
^=::SendInput {Esc}[27;5;61~
^+::SendInput {Esc}[27;6;43~

;;FIXME: AHK bug here: how to set C-: as hotkey?
;;^:::
;;  SendInput {Esc}[27;6;58~
;;  return

;;^;::SendInput {Esc}[27;6;59~
^'::SendInput {Esc}[27;5,39~
;;FIXME: this would affect ^ key
;;^"::
;;  SendInput {Esc}[27;6,24~
;;  return
;;
^,::SendInput {Esc}[27;5;44~
^.::SendInput {Esc}[27;5;46~
;;C-/ => C-_
^/::SendInput {Esc}[27;5;47~

^<::SendInput {Esc}[27;6;60~
^>::SendInput {Esc}[27;6;62~
;;C-? => DEL
^?::SendInput {Esc}[27;6;63~


;;mintty
;;      Key 	plain 	Shift 	Ctrl 	Ctrl+Shift
;;      Tab 	^I 	^[[Z 	^[[1;5I	^[[1;6I
;;      Enter 	^M 	^J 	^^ 	U+009E
;;      Bksp 	^? 	^? 	^_ 	U+009F

;;most terminals emit ^[[Z for Shift-Tab
+Tab::
  if not GetKeyState("Numlock", "T")
     SendInput {Esc}[Z
  else
    SendInput {Esc}[27;2;9~
  return
;;!Tab::SendInput {Esc}{Tab}
^Tab::SendInput {Esc}[27;5;9~

+Enter::SendInput {Esc}[27;2;13~
^Enter::SendInput {Esc}[27;5;13~
;;!Enter::SendInput {Esc}{Enter}

;;NOTE:  Backspace => DEL  != Delete (=> <delete>)
;; Alt+Bksp  = M-DEL
;; map C-backspace to M-backspace
^BackSpace::SendInput {Esc}{BackSpace}


;;* ===== keypad ========
;;PuTTY original behavior:
;;  - for the top row, putty sends \eOP, \eOQ,\eOR \eOS,
;;    but they conflicts with xterm's F1..F4  (also used by mintty/gnome-terminal/xfce-terminal)
;;    (putty itself uses \e[11~ ..\e[14~ for F1..F4)
;;  - for other rows, putty sends \eOl ..\eOp
;;NOTE: Application keypad mode can be turned on and off by the server, depending on the application.
;;Emacs would turn on application keypad mode

;;With the following scripts, keypad acts similar to normal PC keyboard:
;;when NumLock on, keypad sends 0-9 and +-*/
;;when NumLock off, keypad send sequences \eOj .. \eOy (refer term/xterm.el)

;; see also
;; http://vim.wikia.com/wiki/PuTTY_numeric_keypad_mappings (wrong sequences for keypad +-*/ ?

~NumLock::return
NumpadMult::
  if GetKeyState("Numlock", "T")  
    SendInput *
  else
    SendInput {Esc}Oj
  return
NumpadAdd::
  if GetKeyState("Numlock", "T")  
    SendInput +
  else
    SendInput {Esc}Ok
  return
;;\e[0l         kp-separator ?  
NumpadSub::
  if GetKeyState("Numlock", "T")  
    SendInput -
  else
    SendInput {Esc}Om
  return
NumpadDiv::
  if GetKeyState("Numlock", "T")  
    SendInput /
  else
    SendInput {Esc}Oo
  return  


NumpadEnter::
  if GetKeyState("Numlock", "T")  
    SendInput -
  else
    SendInput {Esc}OM
  return


NumpadDel::SendInput {Esc}On
;;to kp-0 .. kp-9                           
NumpadIns::SendInput {Esc}Op
NumpadEnd::SendInput {Esc}Oq
NumpadDown::SendInput {Esc}Or
NumpadPgdn::SendInput {Esc}Os
NumpadLeft::SendInput {Esc}Ot
NumpadClear::SendInput {Esc}Ou
NumpadRight::SendInput {Esc}Ov
NumpadHome::SendInput {Esc}Ow
NumpadUp::SendInput {Esc}Ox
NumpadPgup::SendInput {Esc}Oy

NumpadDot::SendInput .
Numpad0::SendInput 0
Numpad1::SendInput 1
Numpad2::SendInput 2
Numpad3::SendInput 3
Numpad4::SendInput 4
Numpad5::SendInput 5
Numpad6::SendInput 6
Numpad7::SendInput 7
Numpad8::SendInput 8
Numpad9::SendInput 9

;;\e[OI         kp-tab



;;* ====== misc ==========
;;; http://code.google.com/p/mintty/wiki/Keycodes#Special_keys

;;\e[1~         find
;;\e[28~        help
;;\e[29~        print (menu?)


;;* =========== Misc ======================

;;super/hyper modifiers (only for Emacs)
*RWin::SendInput ^x@s
*AppsKey::SendInput ^x@h

;;tmux/gnu-screen
<#Tab::SendInput ^bn
<#1::SendInput ^b1
<#2::SendInput ^b2
<#3::SendInput ^b3
<#4::SendInput ^b4

;;on some system, <end> would be recognized as <select>
;End::SendInput {Esc}OF


;;* =========== MobaXterm only ======================
#if WinActive("ahk_class TMobaXtermForm")
;;AutoHotkey:  Ctrl ^ Alt ! Shift +
!x::SendInput {Esc}x
!s::SendInput {Esc}s
!g::SendInput {Esc}g
!f::SendInput {Esc}f
!b::SendInput {Esc}b
!p::SendInput {Esc}p
!n::SendInput {Esc}n
!q::SendInput {Esc}q
!v::SendInput {Esc}v
!y::SendInput {Esc}y
!/::SendInput {Esc}/

^h::SendInput {f1}
