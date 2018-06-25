### dispatch.py

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
        else:
            cmd = ""
            args = ""
            app = None
            match = False
            
            if len(self.params) >= 5 and match == False:
                cmd = "%s-%s-%s-%s" % (self.params[1], self.params[2], self.params[3], self.params[4])
                args = " ".join(self.params[5:])
                app = self.findapp(cmd)
                if app != None:
                    match = True;
                
            if len(self.params) >= 4 and match == False:
                cmd = "%s-%s-%s" % (self.params[1], self.params[2], self.params[3])
                args = " ".join(self.params[4:])
                app = self.findapp(cmd)
                if app != None:
                    match = True;

            if len(self.params) >= 3 and match == False:
                cmd = "%s-%s" % (self.params[1], self.params[2])
                args = " ".join(self.params[3:])
                app = self.findapp(cmd)
                if app != None:
                    match = True;

            if len(self.params) >= 2 and match == False:
                cmd = "%s" % (self.params[1])
                args = " ".join(self.params[2:])
                app = self.findapp(cmd)
                if app != None:
                    match = True;
                    
            if app == None and match == False:
                print "Can't find %s to usaged\n" % cmd
                print "Try to run '%s help' to get help\n" % self.entryname
            else:
                if config.verbal == True:
                    print app
                    
                cmdline = "%s %s" % (" ".join(app), args)
                print "cmdline: %s\n" % cmdline
                os.system(cmdline)
        
    def usage(self):
        for key, value in sorted(self.appmap.items()):
            print "Use: %s %s [argv]" % (self.entryname, key.replace("-", " "))
        
    def findapp(self, app):
        if app in self.appmap.keys():
            return self.appmap[app]
        else:
            return None
        
    def getappmap(self):
        appdir = "%s/%s" % (self.srcdir, config.appdirname)
        applist = []
        
        for (dirname, subdir, subfile) in os.walk(appdir):
            if config.verbose == True:
                print 'Dirname: %s' % dirname
                print 'Subdir: %s'  % subdir
                print 'Subfile: %s' % subfile

            for f in subfile:
                applist.append(f)
        
        for ignore in config.appignorefiles:
            try:
                applist.remove(ignore)
            except:
                pass

        for app in applist:
            appinfo = []
            appname, appext = os.path.splitext(app)

            if appext == "":
                appinfo.append("")
            else:
                appinfo.append(config.appnames["*" + appext])
                
            appinfo.append(self.srcdir + "/" + config.appdirname + "/" + app)
            self.appmap[appname] = appinfo
            
        if config.verbose == True:
            print self.appmap
