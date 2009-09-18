import os, sys

# pylint: disable-msg=F0401

#from setuptools import setup, find_packages
from setuptools import find_packages
from numpy.distutils.core import setup
from numpy.distutils.misc_util import Configuration

version = '0.1.0'

config = Configuration()
config.add_extension('engineC', \
                     sources=['openmdao/examples/engine_design/engineC.pyf', \
                              'openmdao/examples/engine_design/engineC.c'] )

kwds = { 'name':'openmdao.examples',
      	 'version':version,
         'description':"OpenMDAO examples",
         'long_description':"""\
         """,
         'classifiers':[
            'Development Status :: 2 - Pre-Alpha',
            'Intended Audience :: Science/Research',
            'License :: OSI Approved',
            'Natural Language :: English',
            'Operating System :: OS Independent',
            'Programming Language :: Python :: 2.5',
            'Topic :: Scientific/Engineering',
             ],
         'keywords':'optimization multidisciplinary multi-disciplinary analysis',
         'author':'',
         'author_email':'',
         'url':'',
         'license':'NASA Open Source Agreement 1.3',
         'namespace_packages':["openmdao"],
         #'package_dir':{'': 'src'},
         'packages':find_packages(), #['openmdao','openmdao.examples'],
         'include_package_data':True,
         'test_suite':'nose.collector',
         'zip_safe':False,
         'install_requires':[
             'setuptools'
             ],
         'entry_points':"""
         # -*- Entry points: -*-
         """,
      }

kwds.update(config.todict())

setup(**kwds)

#setup(name='openmdao.examples',
      #version=version,
      #description="OpenMDAO examples",
      #long_description="""\
#""",
      #classifiers=[
        #'Development Status :: 2 - Pre-Alpha',
        #'Intended Audience :: Science/Research',
        #'License :: OSI Approved',
        #'Natural Language :: English',
        #'Operating System :: OS Independent',
        #'Programming Language :: Python :: 2.5',
        #'Topic :: Scientific/Engineering',
      #],
      #keywords='optimization multidisciplinary multi-disciplinary analysis',
      #author='',
      #author_email='',
      #url='',
      #license='NASA Open Source Agreement 1.3',
      #namespace_packages=["openmdao"],
      ##package_dir={'': 'src'},
      #packages=find_packages(), #['openmdao','openmdao.examples'],
      #include_package_data=True,
      #test_suite='nose.collector',
      #zip_safe=False,
      #install_requires=[
         #'setuptools'
      #],
      #entry_points="""
      ## -*- Entry points: -*-
      #""",
      #)

