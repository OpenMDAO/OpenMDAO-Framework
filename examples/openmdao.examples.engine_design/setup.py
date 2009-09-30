import os, sys

# pylint: disable-msg=F0401

#from setuptools import setup, find_packages
from setuptools import find_packages
from numpy.distutils.core import setup
from numpy.distutils.misc_util import Configuration

version = '0.1.0'

if sys.platform == 'win32':
    sdkdir = os.environ.get('WindowsSdkDir')
    include_dirs = [os.path.join(sdkdir,'Include')]
    library_dirs = [os.path.join(sdkdir,'Lib')]
    # make sure we have mt.exe available in path
    path = os.environ['PATH'].split(';')
    path.append(os.path.join(sdkdir,'bin'))
    os.environ['PATH'] = ';'.join(path)
else:
    include_dirs = []
    library_dirs = []
    
config = Configuration()
config.add_extension('openmdao.examples.engine_design.engineC', \
                     sources=['openmdao/examples/engine_design/engineC.pyf', \
                              'openmdao/examples/engine_design/engineC.c'],
                     include_dirs=include_dirs,
                     library_dirs=library_dirs)

kwds = { 'name':'openmdao.examples.engine_design',
      	 'version':version,
         'description':"OpenMDAO examples - Engine Design Problem",
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
         'namespace_packages':["openmdao", "openmdao.examples"],
         #'package_dir':{'': 'openmdao/examples/engine_design'},
         'packages':find_packages(), #['openmdao','openmdao.examples'],
         'include_package_data':True,
         'test_suite':'nose.collector',
         'zip_safe':False,
         'install_requires':[
             'setuptools',
             'openmdao.lib',
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

