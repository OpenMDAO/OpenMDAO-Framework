import sys
import os

name = 'conmindriver'

# add our package to python path so autodoc will find our source code
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)),name))

# at the moment, extensions requires setuptools in order to create PLUGINS file  :(
from setuptools import setup
from sphinx.setup_command import BuildDoc

from extensions import register

register('openmdao.driver', 'CONMINdriver', 'conmindriver:CONMINdriver')

setup(
    name='conmindriver',
    description=None,
    author=None,
    author_email=None,
    license=None,
    url=None,
    packages=['conmindriver'],
    zip_safe=False,
    install_requires=[],
    cmdclass={ 'build_sphinx': BuildDoc },
    #entry_points={
    #    'openmdao.driver': ['CONMINdriver = conmindriver:CONMINdriver'], 
    #    'openmdao.component': ['CONMINdriver = conmindriver:CONMINdriver']
    #}
)   
   
