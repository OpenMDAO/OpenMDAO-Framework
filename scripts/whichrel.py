#!/usr/bin/env python

import sys
from ticket_info import get_loginfos, get_tag_dict, get_branch_dict, version_rgx, release_rgx


if __name__ == '__main__':
    numargs = len(sys.argv)
    if numargs != 2:
        print 'usage: whichrel <ticket number>'
        sys.exit(-1)
        
    loginfos = get_loginfos()
    tagdict = get_tag_dict(loginfos)
    
    tagname = 'T'+sys.argv[1]
    try:
        loginfo = tagdict[tagname]
    except KeyError:
        print "Tag '%s' was not found" %  tagname
        sys.exit(0)
        
    version = None
    release = None
    for loginf in loginfos[loginfo.position:]:
        for tag in loginf.tags:
            if version is None and version_rgx.match(tag):
                version = tag
                print 'version:',tag
            if release is None and release_rgx.match(tag):
                release = tag
                print 'release:',tag
                break
                
                
    print ''
                
             
        
