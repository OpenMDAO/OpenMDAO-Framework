"""Psudo package providing a central place to access all of the
OpenMDAO datatypes standard library."""

# Traits that we've modified
from openmdao.lib.datatypes.enum import Enum
from openmdao.lib.datatypes.float import Float
from openmdao.lib.datatypes.file import File
from openmdao.lib.datatypes.int import Int
from openmdao.lib.datatypes.array import Array
from openmdao.main.pluginsock import Socket

Instance = Socket

# Traits from Enthought
from enthought.traits.api import Bool, List, Str, \
     Complex, CBool, Dict, ListStr, Any, on_trait_change,\
     Python, Event, Dict
