
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
      classifiers=[],
      keywords='testing',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement',
      namespace_packages=["openmdao"],
      packages=['openmdao', 'openmdao.test'],
      package_dir={'': 'src'},
      package_data={ 'openmdao.test': ['plugins/*.egg'] },
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
          'openmdao.main',
          'openmdao.lib',
          # -*- Extra requirements: -*-
      ],
      entry_points="""
      [openmdao.components]
      hollowsphere = openmdao.test.HollowSphere:HollowSphere
      box = openmdao.test.Box:Box
      
      [console_scripts]
      openmdaotest=openmdao.test.testing:run_openmdao_suite
      
      """,
      )
