VSCode
======

## URLs

    ## https://code.visualstudio.com/docs/supporting/FAQ

## How to prevent Visual Studio Code from always reopening the previous files or folders?

    ## https://stackoverflow.com/questions/31270774/how-to-prevent-visual-studio-code-from-always-reopening-the-previous-files-or-fo
    ## Go to File -> Preferences -> Settings
    ## In the "Search Settings" bar type Restore Windows.
    ## You will see Window:Restore Windows. Set it to none.
    ## "window.restoreWindows": "none"

## How to prevent Visual Studio Code from opening files in the current tab?

    ## https://superuser.com/questions/1483395/how-to-prevent-visual-studio-code-from-opening-files-in-the-current-tab
    ## It's controlled by configuration option Workbench > Editor: Enable Preview.
    ## Open settings by either main menu option File > Preferences > Settings or pressing Ctrl+,
    ## then search for "Enable Preview" and you will see the option. Uncheck the option to disable opening as a preview.

    ## Disable it in thr settings.json file
    ## "workbench.editor.enablePreview": false,

## How do I opt out of VS Code auto-updates?

    ## To modify the update mode, go to File > Preferences > Settings, search for update mode and change the setting to none.
    ## "update.mode": "none"

    ## Opt out of extension updates
    ## clear the `Extensions: Auto Update` check box in the Settings editor (Ctrl+,).
    ## "extensions.autoUpdate": false


## How to disable crash reporting

    ## From File > Preferences > Settings, search for telemetry, and set the Telemetry: Telemetry Level setting to off

## How to disable context menu animation

    ## From File > Preferences > Settings, search for titleBarStyle, and switched back to 'custom mode'
    ## "window.titleBarStyle": custom

## How to make Ctrl+k kill till the end of line in the terminal

    ## Open File -> Preferences -> Settings -> User (tab), search for allowChords and uncheck it
    ## or edit your %APPDATA%\Code\User\settings.json
    ## "terminal.integrated.allowChords": false

## Using GDB in VSCode

    ## https://docs.lagerdata.com/tutorials/vscode.html
    ## https://www.cs.swarthmore.edu/courses/cs35/f24/using_gdb_in_vscode/
    ## Install cpptools extension
    $ code --install-extension ms-vscode.cpptools
