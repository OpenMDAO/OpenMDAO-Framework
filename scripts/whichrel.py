#!/usr/bin/env python

import sys
from ticket_info import get_loginfos, get_tag_dict, get_branch_dict, get_loginfo_tree, walk_tree_chrono
from ticket_info import version_rgx, release_rgx


if __name__ == '__main__':
    numargs = len(sys.argv)
    if numargs != 2:
        print 'usage: whichrel <ticket number>'
        sys.exit(-1)
        
    loginfos = get_loginfos()
    tagdict = get_tag_dict(loginfos)
    tree = get_loginfo_tree(loginfos)
    
    tagname = 'T'+sys.argv[1]
    try:
        loginfo = tagdict[tagname]
    except KeyError:
        print "Tag '%s' was not found" %  tagname
        sys.exit(0)
        
    versions = []
    releases = []
    found = False
    versions_done = False
    done = False
    for loginf in walk_tree_chrono(tree):
        if loginf is loginfo:
            found = True
        if found:
            for tag in loginf.tags:
                if not versions_done and version_rgx.match(tag):
                    versions.append(tag)
                if release_rgx.match(tag):
                    releases.append(tag)
                    done = True
            if len(versions) > 0:
                versions_done = True
            if done:
                break
    print 'version:',versions
    print 'release:',releases
    
