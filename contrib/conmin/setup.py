import setuptools

from numpy.distutils.core import setup
from numpy.distutils.misc_util import Configuration

config = Configuration(name='conmin')
config.add_extension('conmin', sources=['conmin.f'])

kwds = {'install_requires':['numpy'],
        'version':'1.0',
        'zip_safe':False,
       }
kwds.update(config.todict())

setup(**kwds)

