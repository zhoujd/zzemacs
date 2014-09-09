#!/usr/bin/env python

import os
import sys
import ConfigParser

if __name__ == "__main__":
    hg_root=os.popen("hg root").readlines()[0].rstrip('\n')
    hg_rc_path = hg_root + os.sep + ".hg" + os.sep + "hgrc"
    print "current hgrc: " + hg_rc_path

    config = ConfigParser.ConfigParser()
    config.read(hg_rc_path)

    # ui.username
    name = raw_input("please input name (Enter for skip):").rstrip('\n')
    email = raw_input("please input email (Enter for skip):").rstrip('\n')
    if name != "" and email != "":
        username = name + "<" + email + ">"
        if not config.has_section('ui') :
            config.add_section('ui')
        config.set('ui', 'username', username)

    # http_proxy.host
    proxy_host = raw_input("please input http_proxy.host (Enter for skip):").rstrip('\n')
    if proxy_host != "" :
        if not config.has_section('http_proxy') :
            config.add_section('http_proxy')
        config.set('http_proxy', 'host', proxy_host)

    # paths.default
    path_url = raw_input("please input paths.default (Enter for skip):").rstrip('\n')
    if path_url != "" :
        if not config.has_section('paths') :
            config.add_section('paths')
        config.set('paths', 'default', path_url)

    with open(hg_rc_path, 'w') as configfile:    # save
        config.write(configfile)
