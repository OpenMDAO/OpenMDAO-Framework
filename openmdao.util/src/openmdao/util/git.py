
import os
import urllib2

def download_github_tar(org_name, repo_name, version, dest='.'):
    """Pull a tarfile of a github repo and place it in the 
    specified destination directory. 'version' can be a tag or a commit id.
    """
    dest = os.path.abspath(os.path.expanduser(os.path.expandvars(dest)))
    
    try:
        resp = urllib2.urlopen('https://nodeload.github.com/%s/%s/tarball/%s' % 
                               (org_name, repo_name, version))
    except urllib2.HTTPError as err:
        print str(err)
        exit(-1)
    
    tarpath = os.path.join(dest, "%s-%s.tar.gz" % (repo_name, version))
    with open(tarpath, 'wb') as out:
        out.write(resp.fp.read())

    return tarpath
    
