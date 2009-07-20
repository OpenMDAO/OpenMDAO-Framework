"""
A small example plugin
"""
from setuptools import setup

__author__ = 'Your Name Here'

setup(
    name='bar',
    version='1.0',
    description=__doc__,
    author=__author__,
    packages=[],
    py_modules=['bar'],
    install_requires = ['foo==1.0'],
    entry_points='''
    [openmdao.dumbplugins]
    bar.Comp1Plugin = bar:Comp1Plugin
    bar.Comp2Plugin = bar:Comp2Plugin
    '''
)


