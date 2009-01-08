from setuptools import setup, find_packages
import os

version = '0.1'

setup(name='openmdao.recipes',
      version=version,
      description="various zc.buildout recipes for openmdao",
      long_description=open("README.txt").read() + "\n" +
                       open(os.path.join("docs", "HISTORY.txt")).read(),
      # Get more strings from http://www.python.org/pypi?%3Aaction=list_classifiers
      classifiers=[
        "Programming Language :: Python",
        "Topic :: Software Development :: Libraries :: Python Modules",
        ],
      keywords='recipe buildout',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement',
      packages=find_packages(exclude=['ez_setup']),
      namespace_packages=['openmdao'],
      include_package_data=True,
      zip_safe=True,
      install_requires=[
          'setuptools',
          'zc.recipe.egg==1.1.0'
      ],
      entry_points="""
      [zc.buildout]
      default = openmdao.recipes.wingproj:WingProj
      isolatedegg = openmdao.recipes.isolatedegg:IsolatedEgg
      wingproj = openmdao.recipes.wingproj:WingProj
      """,
      )
