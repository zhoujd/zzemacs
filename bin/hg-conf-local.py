#!/usr/bin/env python

import os
import ConfigParser

def set_conf_item(conf_file, section, name, value):
    config = ConfigParser.ConfigParser()
    config.read(conf_file)

    if not config.has_section(section):
        config.add_section(section)
    config.set(section, name, value)

    with open(conf_file, 'w') as configfile:
        config.write(configfile)


if __name__ == "__main__":
    hg_root = os.popen("hg root").readlines()[0].rstrip()
    hg_rc_path = hg_root + os.sep + ".hg" + os.sep + "hgrc"
    print("current hgrc: " + hg_rc_path)

    # ui.username
    name = os.raw_input("please input name (Enter for skip):").rstrip()
    email = os.raw_input("please input email (Enter for skip):").rstrip()
    if name != "" and email != "":
        username = name + "<" + email + ">"
        set_conf_item(hg_rc_path, 'ui', 'username', username)

    # http_proxy.host
    proxy_host = os.raw_input("please input http_proxy.host (Enter for skip/none for unset):").rstrip()
    if proxy_host != "":
        if proxy_host == "none":
            set_conf_item(hg_rc_path, 'http_proxy', 'host', "")
        else:
            set_conf_item(hg_rc_path, 'http_proxy', 'host', proxy_host)

    # paths.default
    path_url = os.raw_input("please input paths.default (Enter for skip):").rstrip()
    if path_url != "":
        set_conf_item(hg_rc_path, 'paths', 'default', path_url)
