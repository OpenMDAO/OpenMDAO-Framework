
import os, sys
import pkg_resources
from setuptools.package_index import PackageIndex

class CacheFiller(object):
    """Looks through the eggs directory and makes sure that uninstalled 
    distributions for each egg are placed in the specified cache directory
    """

    def __init__(self, buildout, name, options):
        self.buildout = buildout
        self.name = name
        self.options = options
        
        self.cache = buildout['buildout']['download-cache']
        self.eggdir = buildout['buildout']['eggs-directory']
        self.deveggs = buildout['buildout']['develop-eggs-directory']
        self.links = options.get('find-links',
                                  buildout['buildout'].get('find-links'))
        index_link = options.get('index_link',
                             'http://pypi.python.org/simple')
        self.index_link = index_link.strip()
        self.pkg_index PackageIndex(self.index_link)


    def install(self):
        options = self.options
        dest = []

        
        return ()

    update = install


    def find_buildout_eggs(self):
        """search through whole buildout dict to find any 'eggs' options"""
        eggs = set()
        
