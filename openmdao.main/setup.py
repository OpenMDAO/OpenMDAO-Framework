import os, sys

# pylint: disable-msg=F0401

from setuptools import setup


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
        'Programming Language :: Python :: 2.5',
        'Topic :: Scientific/Engineering',
      ],
      keywords='optimization multidisciplinary multi-disciplinary analysis',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement 1.3',
      namespace_packages=["openmdao"],
      packages=['openmdao', 'openmdao.main'],
      package_dir={'': 'src'},
      #package_data={'openmdao.main': ['plugins/*.egg','test/*.py']},
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
      ],
      entry_points = {
          "distutils.setup_keywords": [
              "openmdao_metadata=openmdao.main.dist:assert_dict_or_none",
              ],
          "egg_info.writers": [
              "openmdao_metadata.txt=openmdao.main.dist:write_pretty",
              ],
          },
    )
