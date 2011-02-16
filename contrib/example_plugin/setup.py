import sys
import os

name = 'conmindriver'

# add our package to python path so autodoc will find our source code
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)),name))

from distutils.core import setup
import distutils.command.build
import distutils.command.sdist

# this makes sure that the 'build' command also runs 'build_sphinx'
distutils.command.build.build.sub_commands.append(('build_sphinx', lambda self:True))

# at the moment, extensions requires setuptools in order to create PLUGINS file  :(
#from setuptools import setup
from sphinx.setup_command import BuildDoc

class mysdist(distutils.command.sdist):
    def get_file_list(self):
        # automatically add sphinx generated docs to distribution
        mydir = os.path.dirname(os.path.abspath(__file__))
        htmldir = os.path.join(mydir, 'build', 'sphinx', 'html')
        if os.path.isdir(htmldir):
            self.filelist.append(htmldir)
        distutils.command.sdist.get_file_list(self)

cmdclass = { 
   'build_sphinx': BuildDoc,
   'sdist': mysdist,
}

#from extensions import register
#register('openmdao.driver', 'CONMINdriver', 'conmindriver:CONMINdriver')

setup(
    name='conmindriver',
    packages=['conmindriver'],
    zip_safe=False,
    install_requires=['foo'],
    cmdclass=cmdclass,
)   
   
