## dispatch

import glob
import os
import config

class Dispatch:
    def __init__(self, workdir, srcdir, argv):
        self.workdir = workdir
        self.srcdir = srcdir
        self.params = argv
        self.appmap = {}
        self.entryname = os.path.basename(argv[0])
    def run(self):
        self.getappmap()
        
        if len(self.params) == 1:
            self.usage()
        elif len(self.params) == 2 and self.params[1] == "help":
            self.usage()
        elif len(self.params) == 3 and self.params[1] == "help":
            app = self.findapp(self.params[2])
            if app == None:
                print "Can't find %s to usaged\n" % self.params[2]
                print "Try to run '%s help' to get help\n" % self.entryname
            else:
                #print app
                cmdline = "%s %s" % (" ".join(app), "help")
                print "cmdline: %s\n" % cmdline
                os.system(cmdline)
        else:
            app = self.findapp(self.params[1])
            if app == None:
                print "Can't find %s to usaged\n" % self.params[1]
                print "Try to run '%s help' to get help\n" % self.entryname
            else:
                #print app
                cmdline = "%s %s" % (" ".join(app), " ".join(self.params[2:]))
                print "cmdline: %s\n" % cmdline
                os.system(cmdline)

        
    def usage(self):
        print "Use: %s help           -- for help" % self.entryname
        for app in self.appmap:
            print "Use: %s %s [argv]" % (self.entryname, app)

        print ""
        print "Use: %s help <app>     -- for application help" % self.entryname
        
    def findapp(self, app):
        if app in self.appmap:
            return self.appmap[app]
        else:
            return None
        
    def getappmap(self):
        appdir = "%s/%s" % (self.srcdir, config.appdirname)
        applist = []
        for appcfgitem in config.appnames.keys():
            applist += glob.glob(appdir + "/" + appcfgitem)

        applist = map(lambda x: os.path.basename(x), applist)
        for ignore in config.appignorefiles:
            try:
                applist.remove(ignore)
            except:
                pass

        for app in applist:
            appinfo = []
            appname, appext = os.path.splitext(app)

            ## for command line
            appinfo.append(config.appnames["*" + appext])
            appinfo.append(self.srcdir + "/" + config.appdirname + "/" + app)
            self.appmap[appname] = appinfo
        #print self.appmap
