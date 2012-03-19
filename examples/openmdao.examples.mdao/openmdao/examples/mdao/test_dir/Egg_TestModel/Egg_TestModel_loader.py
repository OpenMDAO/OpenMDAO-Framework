import os
import sys
if not '.' in sys.path:
    sys.path.append('.')

try:
    from openmdao.main.api import Component, SAVE_CPICKLE
except ImportError:
    print 'No OpenMDAO distribution available.'
    if __name__ != '__main__':
        print 'You can unzip the egg to access the enclosed files.'
        print 'To get OpenMDAO, please visit openmdao.org'
    sys.exit(1)

def load(**kwargs):
    '''Create object(s) from state file.'''
    return Component.load('Egg_TestModel.pickle', SAVE_CPICKLE, **kwargs)

def main():
    '''Load state and run.'''
    model = load()
    model.run()

if __name__ == '__main__':
    main()
