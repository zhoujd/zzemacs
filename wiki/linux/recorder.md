Recorder
=======

## How To Replay The Recorded Terminal Sessions Using Scriptreplay Command

    ## https://ostechnix.com/how-to-replay-the-recorded-terminal-sessions-using-scriptreplay-command/
    $ script -a my_terminal_activities --timing=time.log

    $ lsb_release -a
    $ uname -a
    $ exit

    ## replay
    $ scriptreplay --timing=time.log my_terminal_activities

## Simple recording

    ## https://asciinema.org/docs/installation
    ## https://asciinema.org/docs/usage
    $ sudo apt install asciinema
    $ sudo pip3 install asciinema
    $ asciinema -h

    ## recorde
    $ asciinema rec demo.cast
    $ lsb_release -a
    $ uname -a
    $ exit

    ## replay
    $ asciinema play demo.cast
