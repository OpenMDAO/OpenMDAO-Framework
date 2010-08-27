import sys
import os

import numpy as np
from optparse import OptionParser

from enthought.traits.api import on_trait_change
from openmdao.main.api import Component, ExprEvaluator
from openmdao.lib.api import Str
from openmdao.util.plot import cmdlineXYplot


if __name__ == '__main__': #pragma: no cover
    cmdlineXYplot()
