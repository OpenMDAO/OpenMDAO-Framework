import setuptools

from numpy.distutils.core import setup
from numpy.distutils.misc_util import Configuration

config = Configuration(name='conmin')
config.add_extension('conmin', sources=['conmin.f'])

kwds = {'install_requires':['numpy'],
        'version': '1.0',
        'zip_safe': False,
        'license': 'public domain',
   # NOTE: we use 'url' here, but it really translates to 'home-page'
   # in the metadata. Go figure.
        'url': 'http://www.scilab.org/contrib/index_contrib.php?page=displayContribution&fileID=1086',
        'package_data': {'openmdao.main': ['*.html']},
       }
kwds.update(config.todict())

setup(**kwds)

