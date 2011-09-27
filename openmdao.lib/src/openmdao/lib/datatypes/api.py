"""Pseudo package providing a central place to access all of the
OpenMDAO datatypes in the standard library."""

# Traits that we've modified
from openmdao.lib.datatypes.enum import Enum
from openmdao.lib.datatypes.float import Float
from openmdao.lib.datatypes.file import File
from openmdao.lib.datatypes.int import Int
from openmdao.lib.datatypes.array import Array
from openmdao.main.slot import Slot

# Traits from Enthought - don't import these directly because we may
# change what they point to later
from enthought.traits.api import Bool, List, Str, \
     Complex, CBool, Dict, ListStr, Any, on_trait_change,\
     Python, Event, Dict
