"""
A small example plugin
"""
from setuptools import setup

__author__ = 'Your Name Here'

setup(
    name='foo',
    version='1.4',
    description=__doc__,
    author=__author__,
    packages=[],
    py_modules=['foo'],
    entry_points="""
    [openmdao.dumbplugins]
    foo.Comp1Plugin = foo:Comp1Plugin
    foo.Comp2Plugin = foo:Comp2Plugin
    """
)

