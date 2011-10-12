
import os

from inspect import getsourcefile, getsource, getmodule
from subprocess import check_call

class ConfigInfo(object):
    def __init__(self, instance, name, *initargs, **initkwargs):
        self.name = name  # name of the object that this config describes
        self.klass = instance.__class__
        self.classname = self.klass.__name__
        module = getmodule(self.klass)
        self.modname = module.__name__
        self.package = module.__package__
        self.sourcefile = getsourcefile(self.klass)
        self.initargs = initargs
        self.initkwargs = initkwargs
        # the following is a list of instructions that can have 2 possible forms:
        #   1) a string (python syntax with possible %(pathname)s formatting in it)
        #   2) a tuple containing a name and a ConfigInfo object for a child who is to be initialized
        self.cmds = []
    
    def get_ctor(self):
        """Return a str containing code to initilize an instance of self.classname."""
        parts = [self.classname]
        parts.append('(')
        for arg in self.initargs:
            parts.append("%s," % arg)
        for name,val in self.initkwargs.items():
            parts.append("%s=%s," % (name,val))
        parts.append(')')
        return ''.join(parts)
    
    def get_info(self, inst_name='self'):
        """Return a tuple of the form (lines, pkgs, imports, classes) where lines is a
        list of python statements needed to create and initialize the current
        instance, pkgs is the set of packages that the current instance
        depends on, imports is the set of modules that must be imported in
        order to create the current instance, and classes is any classes that are
        defined in the same file as top top level class.
        """
        lines = []
        pkgs = set()
        imports = set()
        classes = set()
        
        if self.modname != '__main__':
            imports.add((self.modname, self.classname))
            pkgs.add(self.package)
            
        for cmd in self.cmds:
            if isinstance(cmd, basestring):
                lines.append(cmd)
            else:
                lines.append(cmd[1])
                l,p,i,c = cmd[0].get_info(cmd[0].name)
                lines.extend(l)
                pkgs.update(p)
                imports.update(i)
                classes.update(c)
        return lines, pkgs, imports, classes
    
    def save_as_class(self, stream, classname):
        assert(classname != self.klass.__name__)
        lines, pkgs, imports, classes = self.get_info('self')
        
        for p,c in imports:
            stream.write('from %s import %s\n' % (p, c))
        
        stream.write('\n\n')
        
        #for klass in classes:
            #stream.write(getsource(klass))
            #stream.write('\n\n')
        
        stream.write('\nclass %s(%s):\n' % (classname, self.classname))
        stream.write('    def __init__(self, *args, **kwargs):\n')
        stream.write('        super(%s, self).__init__(*args, **kwargs)\n        ' % self.classname)
        
        stream.write('\n        '.join(lines[1:]))
        stream.write("\n")

