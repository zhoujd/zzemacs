Serial Number
=============

1. Windows
Open a command prompt (by choosing the Command Prompt item from the Start menu, or typing cmd in the Start menu's "Run" box), and type the following:

        wmic bios get serialnumber

Note: Windows 8 and 8.1 users may get to the Command Prompt by right clicking on Start button, or by typing "cmd" on Start screen to search it.

2. Mac OS X
Go to Utilities. Open the Terminal application and type:

        ioreg -l | grep IOPlatformSerialNumber

3. Linux
Open a shell and type:

        sudo pacman -S dmidecode
        sudo apt install dmidecode
        sudo dmidecode -t system | grep Serial

Note: The user will need to have root access to the system.
