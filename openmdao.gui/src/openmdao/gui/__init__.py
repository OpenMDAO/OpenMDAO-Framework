"""
This package contains a graphical user interface for the openmdao framework.
"""
from releaseinfo import __version__

import os
os.environ['DJANGO_SETTINGS_MODULE'] = 'openmdao.gui.settings'
