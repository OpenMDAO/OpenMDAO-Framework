
# pylint: disable-msg=F0401

from setuptools import setup, find_packages

version = '0.1.0'

setup(name='openmdao.util',
      version=version,
      description="various utility routines",
      long_description="""\
""",
      classifiers=[],
      keywords='',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement 1.3',
      namespace_packages=["openmdao"],
      packages=find_packages('src'),
      package_dir={'': 'src'},
      include_package_data=True,
      package_data={
          'openmdao.util.test': ['src/doubler.py']
      },
      test_suite='nose.collector',
      zip_safe=False,
      install_requires=[
          'setuptools',
      ],
      entry_points = """
      [console_scripts]
      mod2egg=openmdao.util.mod2egg:mod2egg
      """
    )
