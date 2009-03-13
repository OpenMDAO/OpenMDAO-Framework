
import os
import sys
from os.path import join, splitext, dirname, basename, abspath
import fnmatch
import rfc822
import StringIO
from pkg_resources import get_distribution, resource_listdir
from pkg_resources import WorkingSet, Requirement

def mod_sphinx_info(mod, outfile, show_undoc=False):
    name = os.path.splitext(mod.replace(os.sep, '.'))[0]
    short = os.path.basename(name)
    
    print >> outfile, '%s.py' % short.split('.').pop()
    print >> outfile, '_'*(3+len(short.split('.').pop()))+'\n'
    print >> outfile, '.. automodule:: %s' % short
    print >> outfile, '   :members:'
    if show_undoc:
        print >> outfile, '   :undoc-members:'
    print >> outfile, '   :show-inheritance:\n\n'


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
        if dirname != '':
            path = '/'.join([dirname, name])
        else:
            path = name
        if dist.metadata_isdir(path):
            for md in get_metadata(dist, path):
                yield md
        elif name.endswith('.txt'):
            yield (path[:-4], [x.strip() for x in dist.get_metadata(path).splitlines() if x.strip() != ''])
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
                
            
def pkg_sphinx_info(env,startdir, pkg, outfile, show_undoc=False, underline='-'):
    """Generate Sphinx autodoc directives for all of the modules in 
    the given package.
    
    """
    # locate the package directory
    topdir = pkg
    pkgdir = pkg
    
    ws = WorkingSet()
    dist = env.best_match(Requirement.parse(pkg), ws)

    print >> outfile, 'Package %s' % pkg
    print >> outfile, underline*(len('Package ')+len(pkg))
    print >> outfile, '\n\n'
    
    names = list(get_resource_files(dist,['*__init__.py','*setup.py','*test_*.py'],
                                    ['*.py']))            
    names.sort()
    for name in names:
        mod_sphinx_info(name, outfile, show_undoc=show_undoc)

# TODO: add metadata info to doc page        
#    for md,val in get_metadata(dist):
#        print "%s:\n%s\n"%(md,val)


if __name__ == '__main__':
    from optparse import OptionParser
    
    parser = OptionParser()
    parser.add_option("-u","", action="store_true", dest="show_undoc",
                      help="show undocumented members")
    parser.add_option("-o","", action="store", type='string', dest="out",
                      help="output filename (defaults to stdout)")
    (options, args) = parser.parse_args(sys.argv[1:])
    
    if options.out:
        outf = open(options.out, 'w')
    else:
        outf = sys.stdout
    
    if len(args) == 1:
        pkg_sphinx_info(args[0], outf, options.show_undoc)
    else:
        parser.print_help()
        sys.exit(-1)
