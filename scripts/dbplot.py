import sys
import os

import numpy as np
from optparse import OptionParser

from openmdao.lib.datatypes.api import on_trait_change, Str
from openmdao.main.api import Component, ExprEvaluator
from openmdao.util.casedb import cmdlineXYplot


if __name__ == '__main__': #pragma: no cover
    cmdlineXYplot()
