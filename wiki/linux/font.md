font
====

## Tuning Fontconfig

```
https://www.linuxfromscratch.org/blfs/view/svn/x/tuning-fontconfig.html
https://wiki.archlinux.org/title/Font_configuration
```

## otf2ttf

```
##https://pypi.org/project/afdko/
$ pip install afdko
$ otf2ttf
```

## Path Nerd Font

```
$ sudo apt install fontforge
$ git clone --depth=1 https://github.com/ryanoasis/nerd-fonts.git
$ fontforge -script nerd-fonts/font-patcher -s -c tmpfont/SFMono-Medium.otf -out ./tmpfont/patched
```
