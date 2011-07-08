"""
This creates an index.html file for the find-links directory on the openmdao.org site.
"""


import sys
import os.path
import hashlib
import fnmatch


def find_files(startdir, pats):
    """Return a list of files (using a generator) that match
    the given list of glob patterns. Walks an entire directory structure.
    """
    match = fnmatch.fnmatch
    join = os.path.join
    for path, dirlist, filelist in os.walk(startdir):
        for name in filelist:
            for pat in pats:
                if match(name, pat):
                    yield join(path, name)


def file_md5(fpath):
    """Return the MD5 digest for the given file"""
    try:
        f = open(fpath,'rb')
        m = hashlib.md5()
        while True:
            s = f.read(4096)
            if not s:
                break
            m.update(s)
        return m.hexdigest()
    finally:
        f.close()


def make_egglist_index():
    startdir = os.path.abspath(os.path.dirname(__file__))
    out = open('index.html', 'w')
    out.write('<html>\n<body>\n')
    text = []
    for f in find_files(startdir, ["*.egg", "*.tar.gz", "*.zip"]):
        checksum = file_md5(f)
        basef = os.path.basename(f)
        text.append('<li><a href="%s#md5=%s">%s</a>\n' % (basef, checksum, basef))
    text.sort()
    out.writelines(text)
    out.write('</ul>\n</body>\n</html>')
    out.close()
    

if __name__ == '__main__':
    make_egglist_index()
