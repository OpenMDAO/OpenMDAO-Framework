

import sys


def find_line(name, count, line_number):
    stream = open(name, 'r')
    local_count = 0
    for line in stream:
        count += 1
        local_count += 1
        if count == line_number:
            return name+' '+str(local_count)+': '+line
        if line.startswith('.. include'):
           newfname = line.split('::')[1].strip()
           ret = find_line(newfname, count, line_number) 
           if isinstance(ret,str):
             return ret
           else:
             count = ret
    return count       
           
           
count = 0
fname = sys.argv[1]
linenum = int(sys.argv[2])

print find_line(fname, count, linenum)


