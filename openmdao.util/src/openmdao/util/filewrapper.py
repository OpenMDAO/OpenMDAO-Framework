"""
Utilities for generating input files and parsing output files.
"""

class Undefined(object):
    pass


class Variable(object):
    """
    """
    def __init__(self, name, typ, **kwargs):
        self.name = name
        self.type = typ
        self.default = kwargs.get('default', Undefined)
        self.description = kwargs.get('description', '')
        self.units = kwargs.get('units', None)
        self.upper_bound = kwargs.get('upper_bound', None)
        self.lower_bound = kwargs.get('lower_bound', None)
        self.enum_values = kwargs.get('enum_values', None)
        self.enum_aliases = kwargs.get('enum_alises', None)

class RowFieldVariable(Variable):
    """
    """
    def __init__(self, name, typ, row, field, **kwargs):
        super(RowFieldVariable, self).__init__(name, typ, **kwargs)
        self.row = row
        self.field = field
    
class KeyVariable(Variable):
    """
    """
    def __init__(self, name, typ, search_str, **kwargs):
        super(KeyVariable, self).__init__(name, typ, **kwargs)
        self.search_str = search_str
        self.occurrence = kwargs.get('occurrence', 1)
        self.row_offset = kwargs.get('row_offset', 0)
        self.field = kwargs.get('field', 1)


class FileMarker(object):
    def __init__(self, search_str, is_end=False):
        self.search_str = search_str
        self.is_end = is_end
        self.line = None # when located, this will be a line number
    

class FileParser(object):
    """
    """
    def __init__(self):
        self.variables = []
        self.markers = []
        self.active_marker = FileMarker('')
        self.active_marker.line = 1
        
    def add_marker(self, marker):
        self.markers.append(marker)
    
    def add_variable(self, variable):
        self.variables.append(variable)