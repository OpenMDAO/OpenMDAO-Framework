"""
Run pylint under restricted python environment.
Normally executed by scripts/lint.py
"""

import os.path
import site
import sys

REPO = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))
SITE = os.path.join(os.path.dirname(site.__file__), 'site-packages')

sys.path.append(os.path.join(SITE, 'logilab_common-0.33.0-py2.5.egg'))
sys.path.append(os.path.join(SITE, 'pylint-0.14.0-py2.5.egg'))

# Needed for M4, which refers to scipy, etc.
sys.path.append(os.path.join(REPO, 'contrib'))
sys.path.append(SITE)

execfile(os.path.join(SITE, 'scripts', 'pylint'))
sys.exit(0)

