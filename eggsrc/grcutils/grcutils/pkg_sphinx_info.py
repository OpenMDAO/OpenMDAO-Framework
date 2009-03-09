
import os
import sys
#from inspect import getmembers, isclass, isfunction
from os.path import join, splitext, dirname, basename, abspath
from fnmatch import fnmatchcase

from grcutils.fileutil import glob_walk, dirtreegen

            
def mod_sphinx_info(mod, outfile, package=None, show_undoc=False):
    name = os.path.splitext(mod.replace(os.sep, '.'))[0]
    if package is not None:
        name = '.'.join([package, name])
    short = os.path.basename(name)
    
    print >> outfile, '%s.py' % short.split('.').pop()
    print >> outfile, '_'*(3+len(short.split('.').pop()))+'\n'
    print >> outfile, '.. automodule:: %s' % short
    print >> outfile, '   :members:'
    if show_undoc:
        print >> outfile, '   :undoc-members:'
    print >> outfile, '   :show-inheritance:\n\n'


def pkg_sphinx_info(startdir, pkg, outfile, show_undoc=False, underline='-'):
    """Generate Sphinx autodoc directives for all of the modules in 
    the given package.
    
    """
    # locate the package directory
    topdir = pkg
    pkgdir = pkg
    # directory form of the package, e.g., openmdao.main --> openmdao/main
    dirform = os.sep.join(pkg.split('.'))
    found = 0
    for dd in dirtreegen(startdir):
        if pkg == basename(dd):
            found = 1
            topdir = join(os.getcwd(), dd)
        elif dd.endswith(dirform) and found == 1:
            pkgdir = dd
            break
            

    print >> outfile, 'Package %s' % pkg
    print >> outfile, underline*(len('Package ')+len(pkg))
    print >> outfile, '\n\n'
    
    names = []
    used = set()
    exclude = ['setup.py', '__init__.py']
    abspkg = abspath(pkgdir)
    
    # directory form of the package, e.g., openmdao.main --> openmdao/main
    dirform = os.sep.join(pkg.split('.'))
    
    for fname in glob_walk(abspkg, [join(pkgdir,'*.py')]):
        base = basename(fname)
        if base.startswith('_') or base.startswith('test_') or base in exclude:
            continue        
                
        lpath = fname[len(abspkg)+1:]
        names.append(lpath)
            
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
