clipboard
=========

## Send file contents to your clipboard

```
## 1. For X11 Desktops
$ sudo apt install xclip

## Recommended
$ xclip -selection clipboard < filename.txt
$ cat filename.txt | xclip -selection clipboard

## Using xsel
xsel --clipboard --input < filename.txt

## 2. For Wayland Desktops
$ sudo apt install wl-clipboard
$ wl-copy < filename.txt

3. Pro-Tip: Create an Alias
$ alias cb='xclip -selection clipboard'
$ cb < filename.txt
```
