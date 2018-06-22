zzokapi
=======

1. Create entry script like below - 


        #!/usr/bin/env python

        ### Script main entry

        import os
        import sys
        
        coredirname="zz-core"
        
        def main():
            ## add for import path
            srcdir = os.path.dirname(os.path.realpath(__file__))
            srcdir = srcdir.replace("\\", "/")
            sys.path.append(srcdir + "zzokapi")
        
            import utils.common as common
            import framework.dispatch as dispatch
        
            common.setcoredir(coredirname)
            workdir = common.getworkdir()
            print "Work directory: %s" % (workdir)
        
            dispatcher = dispatch.Dispatch(workdir, srcdir, sys.argv);
            dispatcher.run()
        
        if __name__ == '__main__':
            try:
                main()
            except KeyboardInterrupt:
                print "\nUser Press Ctrl+C, exit\n"

2. Create core folder for sub commands

        $ mkdir zz-core
        $ cd zz-core
        $ touch test.sh
