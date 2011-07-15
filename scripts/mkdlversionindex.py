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

def make_index():
    startdir = os.path.abspath(os.path.dirname(__file__))
    out = open('index.html', 'w')
    version = os.path.basename(os.path.dirname(os.path.abspath(__file__)))
    out.write('<html>\n\n')
    out.write('<head>\n')
    out.write('  <title>OpenMDAO Version %s Downloads</title>\n' % version)
    out.write('  <link rel="stylesheet" href="/chrome/common/css/trac.css" type="text/css" />\n')
    
    out.write('  <div id="header">\n')
    out.write('    <a id="logo" href="http://openmdao.org/">\n')
    out.write('      <img src="/chrome/site/Feb2010_OpenMDAOLogo.png" alt="OpenMDAO Logo" height="93" width="334" />\n')
    out.write('    </a>\n')
    out.write('  </div>\n')
    out.write('</head>\n\n')
    out.write('<body>\n')
    out.write('  <br><br><br>\n')
    out.write('  <h1 class="blog-title" id="openmdao">\n')
    out.write('    OpenMDAO Version %s Downloads\n' % version)
    out.write('  </h1>\n')
    out.write('  <ul>\n')
    out.write('    <li><a href="..">..</a>\n')

    files = []
    dirs = []
    for f in os.listdir(startdir):
        if f in ['index.html', 'mkdlversionindex.py'] or f.startswith('openmdao_src'):
            continue
        if os.path.isfile(f):
            files.append(f)
        else:
            dirs.append(f)

    for d in dirs:
        #lpath = os.path.join(url, 'downloads', version, d)
        out.write('    <li><a href="%s">%s</a>\n' % (d, d))

    for f in files:
        #lpath = os.path.join(url, 'downloads', version, f)
        checksum = file_md5(f)
        out.write('    <li><a href="%s#md5=%s">%s</a>\n'%(f, checksum, f))
    out.write('  </ul>\n')
    out.write('</body>\n</html>')
    out.close()
    

if __name__ == '__main__':
    make_index()
