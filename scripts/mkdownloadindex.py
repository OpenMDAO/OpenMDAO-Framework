#!/usr/bin/env python
"""
make the index page for all OpenMDAO downloads, per version.
"""
import sys
import os
import time
import stat
import re

_digit_rgx = re.compile('([0-9]+)')

def _keyfunct(val):
    def cvtdigits(val):
        if val.isdigit():
            return int(val)
        return val

    parts = [s for s in re.split(_digit_rgx, val) if s]
    return tuple(map(cvtdigits, parts))
    
"""make a list of versioned directories within downloads"""
def list_dirs(startdir):
    results = os.listdir(startdir)
    dir_results = []
    excludes = ["misc", "dev_docs", "user_guides"]
    for name in results:
        if os.path.isdir(os.path.join(startdir, name)) and  (name not in excludes):
            dir_results.append(name)

    return sorted(dir_results, key=_keyfunct, reverse=True)
   

def make_download_index(url):
    # this file is assumed to live in the downloads directory, so use __file__ to get the path
    startdir = os.path.abspath(os.path.dirname(__file__))
    out = open('index.html', 'w')
    out.write('<html>\n\n')
    out.write('<head>\n')
    out.write('  <title>OpenMDAO Downloads by Version</title>\n')
    out.write('  <link rel="stylesheet" href="/chrome/common/css/trac.css" type="text/css" />\n')
    out.write('  <link rel="stylesheet" href="/chrome/tracfullblog/css/fullblog.css" type="text/css" />\n')
    out.write('  <div id="header">\n')
    out.write('    <a id="logo" href="%s/">\n' % url)
    out.write('      <img src="/chrome/site/Feb2010_OpenMDAOLogo.png" alt="OpenMDAO Logo" height="93" width="334" />\n')
    out.write('    </a>\n')
    out.write('  </div>\n')
    out.write('</head>\n\n')
    out.write('<body>\n')
    out.write('  <br><br><br>\n')
    out.write('  <h1 class="blog-title" id="openmdao">\n')
    out.write('    OpenMDAO Downloads by Version\n')
    out.write('  </h1>\n')
    
    dirs = list_dirs(startdir)
    if len(dirs): out.write('  <ul>\n')
    for d in dirs:
        base_d = os.path.basename(d)
        #following lines get the date of each directory's creation.
        stats=os.stat(d)
        create_date = time.localtime(stats[stat.ST_MTIME])
        dir_date = time.strftime("%m/%d/%y %H:%M:%S", create_date)
        out.write('     <li><a href="%s">%s</a>&nbsp&nbsp&nbsp&nbsp %s(CST)\n'%(base_d, base_d, dir_date))
    if len(dirs): out.write('  </ul>\n')

    # This section is meant to display everything in the misc folder at this level. 
    miscdir=os.path.join(startdir,'misc')
    out.write('\n<h1>Miscellaneous Downloads</h1>\n')
    files = os.listdir(miscdir)
    if len(files): out.write('  <ul>\n')
    for f in files:
        base_f = os.path.basename(f)
        linkpath = os.path.join('misc', base_f)
        statspath = os.path.join(miscdir, f)
        stats=os.stat(statspath)
        create_date = time.localtime(stats[9])
        create_date1 = time.localtime(stats[8])
        create_date2 = time.localtime(stats[7])
        #print "times "+ time.localtime(stats[9]) + " " + time.localtime(stats[8]) + " " + time.localtime(stats[7]) 
        #dir_date = time.strftime("%m/%d/%y %H:%M:%S", create_date)
        #dir_date1 = time.strftime("%m/%d/%y %H:%M:%S", create_date1)
        #dir_date2 = time.strftime("%m/%d/%y %H:%M:%S", create_date2)
        #print "\n 1.  " + dir_date
        #print "\n 2.  " + dir_date1
        #print "\n 3.  " + dir_date2
        out.write('     <li><a href="%s">%s</a>&nbsp&nbsp&nbsp&nbsp %s(CST)\n'%(linkpath, base_f, dir_date))
    if len(dirs): out.write('  </ul>\n')
    #END MISC SECTION

    out.write('</body>\n')
    out.write('\n</html>')
    out.close()
    

if __name__ == '__main__':
    if len(sys.argv) > 1:
        url = sys.argv[1]
    else:
        url = 'http://openmdao.org'
    make_download_index(url)


