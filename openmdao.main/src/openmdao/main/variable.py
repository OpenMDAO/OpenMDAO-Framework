""" Base class for all OpenMDAO variables
"""

#public symbols
__all__ = ["Variable", "gui_excludes"]

import re
from keyword import iskeyword

from traits.api import TraitType
from traits.trait_handlers import NoDefaultSpecified
from openmdao.main.interfaces import implements, IVariable, IVariableTree
from openmdao.main.mp_support import has_interface
from openmdao.main.expreval import _expr_dict

# regex to check for valid names.
namecheck_rgx = re.compile(
    '([_a-zA-Z][_a-zA-Z0-9]*)+(\.[_a-zA-Z][_a-zA-Z0-9]*)*')

gui_excludes = ['type', 'vartypename', 'iotype', 'copy', 'validation_trait']

_verified = {}

def is_legal_name(name):
    '''Verifies a Pythonic legal name for use as an OpenMDAO object.'''
    legal = _verified.get(name)
    if legal is None:
        match = namecheck_rgx.match(name)
        if match is None or match.group() != name or iskeyword(name) or \
           name in _expr_dict or name in ('parent', 'self'):
            legal = False
        else:
            legal = True
        _verified[name] = legal
    return legal

def make_legal_path(path):
    """Return `path`, possibly with some character replacements.
    ``[,]`` are replaced with ``_``.
    """
    if '[' in path:
        path = path.replace('[', '_')
        path = path.replace(']', '_')
    return path


def json_default(obj):
    """A function to be passed to json.dumps to handle objects that aren't:
    JSON serializable by default.
    """
    return repr(obj)

_missing = object()

class Variable(TraitType):
    """An OpenMDAO-specific trait type that serves as a common base
    class for framework visible inputs and outputs.
    """
    implements(IVariable)

    def __init__(self, default_value=NoDefaultSpecified, **metadata):
        if 'vartypename' not in metadata:
            metadata['vartypename'] = self.__class__.__name__

        is_vt = False
        # force default value to a value that will always be different
        # than any value assigned to the variable so that the callback
        # will always fire the first time the variable is set.
        if metadata['vartypename'] != 'Slot' and metadata.get('required') == True:
            if default_value is not NoDefaultSpecified:
                is_vt = has_interface(default_value, IVariableTree)
                if not is_vt:
                    # set a marker in the metadata that we can check for later
                    # since we don't know the variable name yet and can't generate
                    # a good error message from here.
                    metadata['_illegal_default_'] = True

            if not is_vt:
                default_value = _missing
        super(Variable, self).__init__(default_value=default_value, **metadata)

    def get_attribute(self, name, value, trait, meta):
        """Return the attribute dictionary for this variable. This dict is
        used by the GUI to populate the edit UI. The basic functionality that
        most variables need is provided here; you can overload this for
        special cases, like lists and dictionaries, or custom datatypes.

        name: str
          Name of variable.

        value: object
          The value of the variable.

        trait: CTrait
          The variable's trait.

        meta: dict
          Dictionary of metadata for this variable.
        """

        attr = {}

        attr['name'] = name
        attr['type'] = type(value).__name__
        attr['value'] = value

        for field in meta:
            if field not in gui_excludes:
                attr[field] = meta[field]

        return attr, None
