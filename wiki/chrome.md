chrome
======

1. How to disable the "unlock your keyring" popup

    sudo gedit /usr/share/applications/google-chrome.desktop
    Exec=/opt/google/chrome/google-chrome  (...)  --password-store=basic
