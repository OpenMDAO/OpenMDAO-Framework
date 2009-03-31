from setuptools import setup, find_packages

setup(name='npsscomponent',
      version="0.1",
      description="OpenMDAO component wrapping for NPSS",
      long_description="""\
""",
      classifiers=[], # Get strings from http://pypi.python.org/pypi?%3Aaction=list_classifiers
      keywords='',
      author='',
      author_email='',
      url='',
      license='',
      #py_modules=['npsscomponent'],
      include_package_data=True,
      zip_safe=False,
      packages = find_packages(), #['npsscomponent'],
      package_data={'npsscomponent': ['test/*.py','test/*.mdl'],
                     '': ['*.int','*.ncp','*.map','*.mdl','*.run','*.fnc']},
      install_requires=[
          'pyNPSS'
      ],
      entry_points="""
      # -*- Entry points: -*-
      """,
      )


