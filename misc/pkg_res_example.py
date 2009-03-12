import sys
import os
import os.path
import rfc822
import StringIO
import fnmatch

from pkg_resources import get_entry_map, get_importer, find_distributions
from pkg_resources import Environment, Distribution, EggMetadata
import zipfile

def _match(name, inlist):
    """Return True if the given name matches any of the
    contents of the list of glob patterns inlist.
    """
    for pat in inlist:
        if fnmatch.fnmatchcase(name, pat):
            return True
    return False
    

def get_resource_files(dist, exList=None, incList=None, dirname=''):
    """Retrieve resource file pathnames from within a distribution."""
    
    exlist = exList or []
    inclist = incList or ['*']
    
    for res in dist.resource_listdir(dirname):
        if dirname != '':
            respath = '/'.join([dirname, res])
        else:
            respath = res
        if dist.resource_isdir(respath):
            for r in get_resource_files(dist, exlist, inclist, respath):
                if _match(r, inclist) and not _match(r, exlist):
                    yield r
        else:
            if _match(respath, inclist) and not _match(respath, exlist):
                yield respath


def get_metadata(dist, dirname=''):
    """Retrieve metadata from within a distribution and return it as
    a generator of tuples of the form (metadata_key, value).
    """
    
    for name in dist.metadata_listdir(dirname):
        print 'name = ',name
        if dirname != '':
            path = '/'.join([dirname, name])
        else:
            path = name
        if dist.metadata_isdir(path):
            for md in get_metadata(dist, path):
                yield md
        elif name.endswith('.txt'):
            yield (path[:-4], [x.strip() for x in 
                     dist.get_metadata(path).splitlines() if x.strip() != ''])
        elif name == 'PKG-INFO':
            instr = StringIO.StringIO(dist.get_metadata(name))
            message = rfc822.Message(instr)
            for k,v in message.items():
                yield (k, v)
        elif name == 'not-zip-safe':
            yield ('zip-safe', False)
        elif name == 'zip-safe':
            yield ('zip-safe', True)
        else:
            yield (path, dist.get_metadata(path))
                

class EggWrapper(object):
    """An object that wraps an installed or zipped egg an reads all of its
    metadata into a dictionary.
    """
    def __init__(self, path):
        self.path = path
        self.metadata = {}
        self._populate_metadata()
        
    def _populate_metadata(self):
        dists = [x for x in find_distributions(self.path,only=True)]
        if len(dists) == 0:
            raise RuntimeError('%s is not a zipeed egg or an installed distribution'%self.path)
            
        dist = dists[0]
        
        if self.path != dist.location:
            raise RuntimeError('%s is not a valid distribution'%self.path)
         
        # getting access to metadata in a zipped egg doesn't seem to work
        # right, so just use a ZipFile to access files under the EGG-INFO
        # directory to retrieve the metadata.   
        if self.path.lower().endswith('.egg') and os.path.isfile(self.path):
            # it's a zip file
            zf = zipfile.ZipFile(self.path, 'r')
            for name in zf.namelist():
                if name.startswith('EGG-INFO/'):
                    metaname = os.path.splitext(os.path.basename(name))[0]
                    meta = zf.read(name).strip()
                    if name.endswith('/PKG-INFO'):
                        instr = StringIO.StringIO(meta)
                        message = rfc822.Message(instr)
                        for k,v in message.items():
                            self.metadata[k] = v
                    elif name.endswith('.txt'):
                        self.metadata[metaname] = [x.strip() for x in 
                                             meta.splitlines() if x.strip() != '']
                    elif name.endswith('/not-zip-safe'):
                        self.metadata['zip-safe'] = False
                    elif name.endswith('/zip-safe'):
                        self.metadata['zip-safe'] = True
                    else:
                        continue
        else:
            self.metadata = dict(list(get_metadata(dist)))
        
                
        self.metadata['py_version'] = dist.py_version
        if 'platform' not in self.metadata or self.metadata['platform']=='UNKNOWN':
            self.metadata['platform'] = dist.platform
            
        self.metadata['entry_points'] = {}
        for gname,group in get_entry_map(dist, group=None).items():
            self.metadata['entry_points'][gname] = [ep for ep in group]


ew = EggWrapper(sys.argv[1])
print 'metadata = ',ew.metadata
        
