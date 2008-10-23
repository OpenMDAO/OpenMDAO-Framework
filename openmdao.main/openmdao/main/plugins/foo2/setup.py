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
    entry_points='''
    [openmdao.dumbplugins]
    comp1 = foo:Comp1Plugin
    comp2 = foo:Comp2Plugin
    '''
)

