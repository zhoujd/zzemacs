python resource
================

1. Good pyhon libs
   - path.py(http://pypi.python.org/pypi/path.py)

        from path import path
        d = path('/home/guido/bin')
        for f in d.files('*.py'):
            f.chmod(0755)

   - sh(http://amoffat.github.com/sh/)

        from sh import git, ls, wc

        # checkout master branch
        git(checkout="master")

        # print(the contents of this directory
        print(ls("-l"))

        # get the longest line of this file
        longest_line = wc(__file__, "-L")

    - watchdog(http://packages.python.org/watchdog/)
    - pyquery(http://packages.python.org/pyquery/)
    - MySQLdb(http://mysql-python.sourceforge.net/MySQLdb.html)
    - fuzzywuzzy(https://github.com/seatgeek/fuzzywuzzy)

