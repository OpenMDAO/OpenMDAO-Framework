import sys
import os

name = 'conmindriver'

# add our package to python path so autodoc will find our source code
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)),name))

#from distutils.core import setup
from setuptools import setup
from distutils.command.sdist import sdist

from sphinx.setup_command import BuildDoc

class mysdist(sdist):
    def run(self):
        mydir = os.path.dirname(os.path.abspath(__file__))
        docbuilddir = os.path.join(mydir, 'sphinx_build')
        if not os.path.isdir(docbuilddir):
            os.mkdir(docbuilddir)
        self.run_command('build_sphinx')
        sdist.run(self)

cmdclass = { 
   'build_sphinx': BuildDoc,
   'sdist': mysdist,
}

setup(
    name='conmindriver',
    packages=['conmindriver'],
    zip_safe=False,
    install_requires=['foo'],
    cmdclass=cmdclass,
)   
   
