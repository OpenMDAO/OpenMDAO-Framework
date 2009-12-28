import os, sys

# pylint: disable-msg=F0401

from setuptools import setup

version = '0.1.0'

setup(name='openmdao.recipes',
      version=version,
      description="Recipes for OpenMDAO buildouts",
      long_description="""\
""",
      classifiers=[],
      keywords='recipe buildout openmdao',
      author='',
      author_email='',
      url='',
      license='NASA Open Source Agreement 1.3',
      namespace_packages=["openmdao"],
      packages=['openmdao', 'openmdao.recipes'],
      package_dir={'': 'src'},
      include_package_data=True,
      test_suite='nose.collector',
      zip_safe=False,
      # NOTE: the openmdao.recipes distrib should NOT be dependent on
      #       any other openmdao packages
      install_requires=[
          'setuptools',
          'zc.recipe.egg',
          'Sphinx',
          'nose',
          'coverage',
      ],
      entry_points="""
      [zc.buildout]
      default = openmdao.recipes.isolatedegg:IsolatedEgg
      isolatedegg = openmdao.recipes.isolatedegg:IsolatedEgg
      wingproj = openmdao.recipes.wingproj:WingProj
      sphinxbuild = openmdao.recipes.sphinxbuild:SphinxBuild
      bundler = openmdao.recipes.bundler:Bundler
      metatable = openmdao.recipes.metatable:MetadataTable
      """,
      )
