
from inspect import getsourcefile

class ConfigInfo(object):
    def __init__(self, instance, name, *initargs, **initkwargs):
        self.name = name  # name of the object that this config describes
        self.klass = instance.__class__
        self.initargs = initargs
        self.initkwargs = initkwargs
        # the following is a list of instructions that can have 2 possible forms:
        #   1) a string (python syntax with possible %(pathname)s formatting in it)
        #   2) a tuple containing a name and a ConfigInfo object for a child who is to be initialized
        self.cmds = []
    
    def get_ctor(self):
        """Return a str containing code to initilize an instance of self.classname."""
        parts = [self.klass.__name__]
        parts.append('(')
        for arg in self.initargs:
            parts.append("%s," % arg)
        for name,val in self.initkwargs.items():
            parts.append("%s=%s," % (name,val))
        parts.append(')')
        return ''.join(parts)
    
    def dump(self, inst_name='self'):
        print '%s = %s' % (inst_name, self.get_ctor())
        for cmd in self.cmds:
            if isinstance(cmd, basestring):
                print cmd
            else:
                cmd[1].dump(cmd[0])

    def get_lines(self, inst_name='self'):
        lines = []
        for cmd in self.cmds:
            if isinstance(cmd, basestring):
                lines.append(cmd)
            else:
                lines.append(cmd[1])
                lines.extend(cmd[0].get_lines(cmd[0].name))
        return lines
        
    def save_as_class(self, stream, classname):
        assert(classname != self.klass.__name__)
        stream.write('\nclass %s(%s):\n' % (classname, self.klass.__name__))
        stream.write('    def __init__(self, *args, **kwargs):\n')
        stream.write('        super(%s, self).__init__(*args, **kwargs)\n        ' % self.klass.__name__)
        
        lines = self.get_lines('self')
        stream.write('\n        '.join(lines[1:]))
        