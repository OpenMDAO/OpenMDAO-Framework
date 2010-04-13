"""
This creates an index.html file for a version downloads dir on
the openmdao.org site.
"""

import sys
import os.path
import hashlib

def file_md5(fpath):
    """Return the MD5 digest for the given file"""
    with open(fpath,'rb') as f:
        m = hashlib.md5()
        while True:
            s = f.read(4096)
            if not s:
                break
            m.update(s)
        return m.hexdigest()

def make_index(startdir = '.'):
    startdir = os.path.abspath(startdir)
    out = open('index.html', 'w')
    version = os.path.basename(os.path.dirname(os.path.abspath(__file__)))
    out.write('<html>\n<title>OpenMDAO Version %s Downloads</title>\n' %
              version)
    out.write('<body>\nOpenMDAO Version %s Downloads\n' % version)
    files = []
    dirs = []
    for f in os.listdir(startdir):
        if f=='index.html' or f=='mkdlversionindex.py':
            continue
        if os.path.isfile(f):
            files.append(f)
        else:
            dirs.append(f)

    for d in dirs:
        lpath = os.path.join('http://openmdao.org/downloads/%s'%version, d)
        out.write('<li><a href="%s">%s</a>\n' % (lpath, d))

    for f in files:
        lpath = os.path.join('http://openmdao.org/downloads/%s'%version, f)
        checksum = file_md5(f)
        out.write('<li><a href="%s#md5=%s">%s</a>\n'%(lpath, checksum, f))
    out.write('</ul>\n')
    out.write('<br><a href="http://openmdao.org">Return to openmdao.org</a>')
    out.write('</body>\n</html>')
    out.close()
    

if __name__ == '__main__':
    if len(sys.argv) > 1:
        make_index(sys.argv[1])
    else:
        make_index()
