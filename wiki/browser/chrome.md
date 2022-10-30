chrome
======

## How to disable the "unlock your keyring" popup

    $ sudo gedit /usr/share/applications/google-chrome.desktop
    Exec=/opt/google/chrome/google-chrome  (...)  --password-store=basic

## Disable check default browser

    $ google-chrome -no-default-browser-check

## Disable GPU

    Setting -> Advance -> System -> Disable  HW accelerate
    $ google-chrome --disable-gpu

## Disable restore pages

    $ google-chrome --disable-session-crashed-bubble

## Vimium copy/paste

    - Search the starting point by: /yourSeach
    - Press enter.
    - Enable visual mode via: v, and visual mode on a line basis via Shift + V
    - Select text by vim navigation keys, aka: h, j, k, l, b, e, w, $ (I especially like shift + w, as it goes to the end of the next word)
    - Yank via y
    You now can switch the context and paste the text via Ctrl+V

## Saved Passwords Issues

    ## "Turn off" sync up with "clean"
    ## "Turn on" sync will auto update

## Black theme

    ## 'Just Black' theme

## How to hide bookmarks from the new tab page in Chrome

    ## https://superuser.com/questions/446424/how-to-hide-bookmarks-from-the-new-tab-page-in-chrome
    ## https://chrome.google.com/webstore/detail/clear-new-tab/felphkbfjadmcejnibcmcncimlappdde

## Launch Google Chrome in Fullscreen mode

    $ google-chrome --app=https://my_url.com
