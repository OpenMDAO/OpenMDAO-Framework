"""Pseudo package providing a central place to access all of the
OpenMDAO surrogatemodels in the standard library."""

from openmdao.lib.surrogatemodels.kriging_surrogate import FloatKrigingSurrogate,KrigingSurrogate
from openmdao.lib.surrogatemodels.multifi_cokriging_surrogate \
     import FloatMultiFiCoKrigingSurrogate, MultiFiCoKrigingSurrogate, MultiFiCoKriging
from openmdao.lib.surrogatemodels.logistic_regression import LogisticRegression
from openmdao.lib.surrogatemodels.response_surface import ResponseSurface
