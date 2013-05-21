"""
This solver can converge an MDA that contains a cyclic graph without requiring
the user to break connections and specify independent and dependent
variables
"""

# pylint: disable-msg=C0103

#public symbols
__all__ = ['MDASolver']

import logging

try:
    import numpy
except ImportError as err:
    logging.warn("In %s: %r" % (__file__, err))
else:
    # this little funct replaces a dependency on scipy
    npnorm = numpy.linalg.norm
    def norm(a, ord=None):
        return npnorm(numpy.asarray_chkfinite(a), ord=ord)
    
# pylint: disable-msg=E0611, F0401
from openmdao.main.api import Driver    
from openmdao.util.decorators import stub_if_missing_deps


@stub_if_missing_deps('numpy')
class MDASolver(Driver):