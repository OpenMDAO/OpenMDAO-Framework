#!/usr/bin/env python

import sys
from ticket_info import get_loginfos, get_tag_dict, get_branch_dict, ticket_rgx


if __name__ == '__main__':
    numargs = len(sys.argv)
    if numargs != 3:
        print 'usage: ticketsbetween <starting_rev> <ending_rev>'
        sys.exit(-1)
        
    revs = (sys.argv[1], sys.argv[2])
    loginfos = get_loginfos(revs)
    
    for loginf in loginfos:
        for tag in loginf.tags:
            if ticket_rgx.match(tag):
                print tag,
                
    print ''
                
             
        
