#!/usr/bin/env python

import os
import sys
import ConfigParser

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print "Using example: %s <push-url>" % sys.argv[0]
        sys.exit(1)

    hg_root=os.popen("hg root").readlines()[0].rstrip('\n')
    hg_rc_path = hg_root + os.sep + ".hg" + os.sep + "hgrc"
    print "current hgrc: " + hg_rc_path

    config = ConfigParser.ConfigParser()
    config.read(hg_rc_path)

    # paths.default
    path_url = sys.argv[1]
    if path_url != "" :
        if not config.has_section('paths') :
            config.add_section('paths')
        config.set('paths', 'default', path_url)

    with open(hg_rc_path, 'w') as configfile:    # save
        config.write(configfile)
