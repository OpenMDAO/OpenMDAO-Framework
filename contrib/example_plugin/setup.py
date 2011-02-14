import sys
import os

name = 'conmindriver'
version = 1.0

# add our package to python path so autodoc will find us
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)),name))

from setuptools import setup
from sphinx.setup_command import BuildDoc

setup(
    name=name,
    version=version,
    description=None,
    author=None,
    author_email=None,
    license=None,
    url=None,
    packages=['conmindriver'],
    zip_safe=False,
    install_requires=[],
    cmdclass={ 'build_sphinx': BuildDoc },
    entry_points={'openmdao.driver': ['CONMINdriver = conmindriver:CONMINdriver'], 'openmdao.component': ['CONMINdriver = conmindriver:CONMINdriver']}
)   
   
