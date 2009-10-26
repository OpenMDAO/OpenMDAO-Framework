#!/usr/bin/env python

import sys
from ticket_info import get_loginfos, get_tag_dict, get_branch_dict, ticket_rgx


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print 'usage: assocticks <ticket_number>'
        sys.exit(-1)
        
    loginfos = get_loginfos()
    tagdict = get_tag_dict(loginfos)
    
    tagname = 'T'+sys.argv[1]
    try:
        loginfo = tagdict[tagname]
    except KeyError:
        print "Tag '%s' was not found" %  tagname
        sys.exit(0)
        
    branchdict = get_branch_dict(loginfos)  
    
    for loginf in branchdict[loginfo.branch_nick]:
        for tag in loginf.tags:
            if ticket_rgx.match(tag) and tag != tagname:
                print tag,
                
    print ''
                
             
        
