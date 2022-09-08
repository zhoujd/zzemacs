zzokapi
=======

## Create entry script

```
cat > zz <<EOF
#!/usr/bin/env python3

# Script main entry

import os
import sys

coredirname = "zz-core"
okapidirname = "okapi"
helpdoc = None

def main():
    # add import path
    srcdir = os.path.dirname(os.path.realpath(__file__))
    srcdir = srcdir.replace("\\", "/")
    sys.path.append(os.path.join(srcdir, okapidirname))

    from utils import common
    from framework import dispatch

    common.setcoredir(coredirname)
    workdir = common.getworkdir()
    dispatcher = dispatch.Dispatch(workdir, srcdir, sys.argv, helpdoc);
    dispatcher.run()


if __name__ == '__main__':
    try:
        main()
    except KeyboardInterrupt:
        print("\nUser Press Ctrl+C, exit\n")
EOF
$ chmod +x zz
```
    
## Create sub commands

```
$ mkdir zz-core
$ cd zz-core
$ vim test.sh
$ chmod +x test.sh
$ zz
Use: zz test [argv]
```
