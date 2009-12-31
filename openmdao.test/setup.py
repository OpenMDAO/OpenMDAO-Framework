
# pylint: disable-msg=F0401

from distutils.errors import DistutilsExecError, DistutilsPlatformError

try:
    from setuptools import setup
except ImportError, e:
    from distutils.core import setup


version = '0.1.0'

setup(name='openmdao.test',
      version=version,
      description="OpenMDAO framework testing package",
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
      keywords='testing',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement 1.3',
      namespace_packages=["openmdao"],
      packages=['openmdao', 'openmdao.test'],
      package_dir={'': 'src'},
      package_data={ 'openmdao.test': ['plugins/*.egg'] },
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'openmdao.lib',
          'nose',
      ],
      entry_points="""
      [openmdao.component]
      openmdao.test.HollowSphere.HollowSphere = openmdao.test.HollowSphere:HollowSphere
      openmdao.test.Box.Box = openmdao.test.Box:Box
      
      [console_scripts]
      openmdaotest = openmdao.test.testing:run_openmdao_suite
      """,
      )
