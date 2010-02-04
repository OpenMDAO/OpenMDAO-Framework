import os, sys

# pylint: disable-msg=F0401

from setuptools import setup, find_packages


setup(name='openmdao.main',
      version='0.1',
      description="OpenMDAO framework infrastructure",
      long_description="""\
""",
      classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Intended Audience :: Science/Research',
        'License :: OSI Approved',
        'Natural Language :: English',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 2.6',
        'Topic :: Scientific/Engineering',
      ],
      keywords='optimization multidisciplinary multi-disciplinary analysis',
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
          'setuptools',
          'pyparsing>=1.5.2',
          'numpy>=1.3.0',
          'PyYAML',
          'networkx==1.0rc1',
          'Traits>=3.0',
          'units',
          'openmdao.util',
      ],
      entry_points = {
          ## This is how we add openmdao specific metadata that can be supplied by
          ## a plugin as an argument to the setup() function.
          ## NOTE: You can't do this in a plugin's setup.py file because the
          ## distib that contains these entry points must already be on sys.path
          ## at the time that the setup() function is running.
          #"distutils.setup_keywords": [
          ## give <keyword_name>=<validation_function> here
          #    "openmdao_metadata=openmdao.main.dist:assert_dict_or_none",
          #    "mylist=setuptools.dist:assert_string_list",
          #    ],
          #"egg_info.writers": [
          ## give <keyword_name.txt>=<function_to_write_txt_file> here
          #    "openmdao_metadata.txt=openmdao.main.dist:write_openmdao_meta",
          #    "mylist.txt = setuptools.command.egg_info:overwrite_arg",
          #    ],
          },
    )
