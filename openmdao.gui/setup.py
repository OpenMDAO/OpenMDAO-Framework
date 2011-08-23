from setuptools import setup, find_packages
import sys, os

here = os.path.dirname(os.path.realpath(__file__))
sys.path.insert(0, os.path.normpath(os.path.join(here,
                                                 'src',
                                                 'openmdao',
                                                 'gui')))

#import releaseinfo
#version = releaseinfo.__version__

setup(name='openmdao.gui',
      #version=version,
      description="OpenMDAO graphical user interface",
      long_description="""\
OpenMDAO graphical user interface""",
      classifiers=[], # Get strings from http://pypi.python.org/pypi?%3Aaction=list_classifiers
      keywords='OpenMDAO GUI',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement 1.3',
      namespace_packages=["openmdao"],
      packages=find_packages('src'),
      package_dir={'': 'src'},
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'jsonpickle', 'web.py', 'django'
      ],
      entry_points="""
      # -*- Entry points: -*-
      """,
      )
