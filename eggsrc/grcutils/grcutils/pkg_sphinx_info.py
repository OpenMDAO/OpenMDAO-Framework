
import os
import sys
#from inspect import getmembers, isclass, isfunction
from os.path import join, splitext, dirname, basename, abspath
from fnmatch import fnmatchcase

from grcutils.fileutil import glob_walk

            
def mod_sphinx_info(mod, outfile, package=None, show_undoc=False):
    mod = mod.replace(os.sep, '.')
    name = os.path.splitext(mod)[0]
    if package is not None:
        name = '.'.join([package, name])
    short = os.path.basename(name)
    
    try:
        __import__(name)
    except Exception, err:
        print >> sys.stderr, 'Import of %s failed: %s' % (name, str(err))
        return

    m = sys.modules[name]
    print >> outfile, 'Module %s.py' % short.split('.').pop()
    print >> outfile, '_'*(len(short.split('.').pop())+len('Module    '))+'\n'
    print >> outfile, '.. automodule:: %s' % short
    print >> outfile, '   :members:'
    if options.show_undoc:
        print >> outfile, '   :undoc-members:'
    print >> outfile, '   :show-inheritance:\n\n'


def pkg_sphinx_info(pkg, outfile, show_undoc=False, underline='-'):
    """Generate Sphinx autodoc directives for all of the modules in the given package.
    
    """
    print 'importing ',pkg
    for x in sys.path:
        print x
    __import__(pkg)
    p = sys.modules[pkg]
    pack = p.__name__
    
    # p.__file__ should give us the path of the __init__.py file 
    # for the package, so start at its containing directory
    dname = dirname(p.__file__)
    dparts = dname.split(os.sep)

    print >> outfile, 'Package %s' % pkg
    print >> outfile, underline*(len(pkg)+len('Package '))
    print >> outfile, '\n\n'
    
    names = []
    for fname in glob_walk(dname,['*.py']):
        pathparts = fname.split(os.sep)
        path = '.'.join(pathparts[len(dparts):])
        base = basename(fname)
        if not base.startswith('_') and not base.startswith('test_') and not base == 'setup.py':
            names.append(path)
            
    names.sort()
    for name in names:
        mod_sphinx_info(name, outfile, package=pkg, show_undoc=show_undoc)


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
