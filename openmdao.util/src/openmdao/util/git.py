
import os
import urllib2

def download_github_tar(org_name, repo_name, version, dest='.'):
    """Pull a tarfile of a github repo and place it in the 
    specified destination directory. 'version' can be a tag or a commit id.
    """
    dest = os.path.abspath(os.path.expanduser(os.path.expandvars(dest)))
    
    resp = urllib2.urlopen('https://nodeload.github.com/%s/%s/tarball/%s' % 
                           (org_name, repo_name, version))
    
    tarpath = os.path.join(dest, "%s-%s.tar.gz" % (repo_name, version))
    bs = 1024*8
    with open(tarpath, 'wb') as out:
        while True:
            block = resp.fp.read(bs)
            if block == '':
                break
            out.write(block)

    return tarpath
    
