chrome
======

1. How to disable the "unlock your keyring" popup

    $ sudo gedit /usr/share/applications/google-chrome.desktop
    Exec=/opt/google/chrome/google-chrome  (...)  --password-store=basic

2. Disable check default browser

    $ google-chrome -no-default-browser-check

3. Disable GPU

    Setting -> Advance -> System -> Disable  HW accelerate
    $ google-chrome --disable-gpu

4. Disable restore pages

    $ google-chrome --disable-session-crashed-bubble

5. Vimium copy/paste

    - Search the starting point by: /yourSeach
    - Press enter.
    - Enable visual mode via: v, and visual mode on a line basis via Shift + V
    - Select text by vim navigation keys, aka: h, j, k, l, b, e, w, $ (I especially like shift + w, as it goes to the end of the next word)
    - Yank via y
    You now can switch the context and paste the text via Ctrl+V
