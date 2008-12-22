
import os, sys

class Dummy(object):
    """A dummy recipe that just dumps out what is passed to it"""

    def __init__(self, buildout, name, options):
        print 'buildout = ',buildout
        print 'name = ',name
        print 'options = ',options

    def install(self):
        return []

    update = install

