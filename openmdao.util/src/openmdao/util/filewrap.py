"""
Utilities for file wrapping.

Note: This is a work in progress.
"""

import re
from pyparsing import CaselessLiteral, Combine, Literal, OneOrMore, Optional, \
                      TokenConverter, Word, nums, oneOf, printables

class _SubHelper(object):
    """Replaces file text at the correct word location in a line."""
    
    def __init__(self):
        
        self.newtext = ""
        self.replace_location = 0
        self.current_location = 0
        self.counter = 0
        
    def set(self, newtext, location):
        """Sets a new word location and value for replacement."""
        
        self.newtext = newtext
        self.replace_location = location
        self.current_location = 0
        
    def set_array(self, newtext, start_location, end_location):
        """For an array, sets a new starting location, ending location, and
        value for replacement."""
        
        self.newtext = newtext
        self.start_location = start_location
        self.end_location = end_location
        self.current_location = 0
        
    def replace(self, text):
        """This function should be passed to re.sub.
        Outputs newtext if current_location = replace_location
        Otherwise, outputs the input text."""
        
        self.current_location += 1
        
        if self.current_location == self.replace_location:
            return str(self.newtext)
        else:
            return text.group()
        
    def replace_array(self, text):
        """This function should be passed to re.sub.
        Outputs newtext if current_location = replace_location
        Otherwise, outputs the input text."""
        
        self.current_location += 1
        end = len(self.newtext)
        
        if self.current_location >= self.start_location and \
           self.current_location <= self.end_location and \
           self.counter < end:
            newval = str(self.newtext[self.counter])
            self.counter += 1
            return newval
        else:
            return text.group()


class ToInteger(TokenConverter):
    """Converter to make token into an integer."""
    def postParse( self, instring, loc, tokenlist ):
        return int(tokenlist[0])

class ToFloat(TokenConverter):
    """Converter to make token into a float."""
    def postParse( self, instring, loc, tokenlist ):
        return float(tokenlist[0])
        
def _parse_line():
    """Parse a single data line that may contain string or numerical data.
    Float and Int 'words' are converted to their appropriate type. 
    Exponentiation is supported, as are NaN and Inf."""
        
    digits = Word(nums)
    dot = "."
    sign = oneOf("+ -")
    ee = CaselessLiteral('E')

    num_int = ToInteger(Combine( Optional(sign) + digits ))
    
    num_float = ToFloat(Combine( Optional(sign) + 
                        ((digits + dot + Optional(digits)) |
                         (dot + digits)) +
                         Optional(ee + Optional(sign) + digits)
                        ))
    
    # special case for a float written like "3e5"
    mixed_exp = ToFloat(Combine( digits + ee + Optional(sign) + digits ))
    
    nan = ToFloat(oneOf("NaN Inf -Inf"))
    
    # sep = Literal(" ") | Literal("\n")
    
    data = ( OneOrMore( (num_float | mixed_exp | num_int | 
                         nan | Word(printables)) ) )
    
    return data


class InputFileGenerator(object):
    """Utility to generate an input file from a template.
    Substitution of values is supported. Data is located with
    a simple API."""
    
    def __init__(self):
        
        self.template_filename = []
        self.output_filename = []
        
        self.delimiter = " "
        self.reg = re.compile('[^ \n]+')
        
        self.data = []
        self.current_row = 0
    
    def set_template_file(self, filename):
        """Set the name of the template file to be used."""
        
        self.template_filename = filename
        
        templatefile = open(filename, 'r')
        self.data = templatefile.readlines()
        templatefile.close()

    def set_generated_file(self, filename):
        """Set the name of the file that will be generated."""
        
        self.output_filename = filename

    def set_delimiters(self, delimiter):
        """Lets you change the delimiter that is used to identify field
        boundaries."""
        
        self.delimiter = delimiter
        self.reg = re.compile('[^' + delimiter + '\n]+')
        
    def mark_anchor(self, anchor, occurrence=1):
        """Marks the location of a landmark, which lets you describe data by
        relative position.
        
        anchor: text to search for
        
        occurence: find nth instance of text; default is 1 (first). Use -1 to
        find last occurence."""
        
        if not isinstance(occurrence, int):
            raise ValueError("The value for occurrence must be an integer")
        
        instance = 0
        if occurrence > 0:
            count = 0
            for line in self.data:
                if line.find(anchor) > -1:
                    instance += 1
                    if instance == occurrence:
                        self.current_row = count
                        return
            
                count += 1
        elif occurrence < 0:
            count = len(self.data)-1
            for line in reversed(self.data):
                if line.find(anchor) > -1:
                    instance += -1
                    if instance == occurrence:
                        self.current_row = count
                        return
            
                count -= 1
        else:
            raise ValueError("0 is not valid for an anchor occurrence.")            
            
        raise RuntimeError("Could not find pattern %s in template file %s" % \
                           (anchor, self.template_filename))
        
    def reset_anchor(self):
        """Resets anchor to the beginning of the file."""
        
        self.current_row = 0
        
    def transfer_var(self, value, row, field):
        """Changes a single variable in the template relative to the 
        current anchor.
        
        row - number of lines offset from anchor line (0 is anchor line).
        This can be negative.
        
        field - which word in line to replace, as denoted by delimiter(s)"""
        
        j = self.current_row + row
        line = self.data[j]
        
        sub = _SubHelper()
        sub.set(value, field)
        newline = re.sub(self.reg, sub.replace, line)
        
        self.data[j] = newline
        
    def transfer_array(self, value, row_start, field_start, field_end,
                       row_end=None, sep=", "):
        """Changes the values of an array in the template relative to the 
        current anchor. This should generally be used for one-dimensional
        or free form arrays.
        
        value - array of values to insert.
        
        row_start - starting row for inserting the array. This is relative
        to the anchor, and can be negative.
        
        field_start - starting field in the given row_start as denoted by 
        delimiter(s). 
        
        field_end - the final field the array uses in row_end. 
        We need this to figure out if the template is too small or large
        
        row_end - Optional. Use if the array wraps to cover additional lines.
        
        sep - Optional. Separator to use if we go beyond the template."""
        
        # Simplified input for single-line arrays
        if row_end == None:
            row_end = row_start
            
        sub = _SubHelper()
        for row in range(row_start, row_end+1):
            
            j = self.current_row + row
            line = self.data[j]

            if row == row_end:
                f_end = field_end
            else:
                f_end = 99999
            sub.set_array(value, field_start, f_end)
            field_start = 0
            
            newline = re.sub(self.reg, sub.replace_array, line)
            self.data[j] = newline
            
        # Sometimes an array is too large for the example in the template
        # This is resolved by adding more fields at the end
        if sub.counter < len(value):
            for val in value[sub.counter:]:
                
                newline = newline[:-1] + sep + str(val) + "\n"
        
            self.data[j] = newline
            
        # Sometimes an array is too small for the template
        # This is resolved by removing fields
        elif sub.counter > len(value):
            
            # TODO - Figure out how to handle this.
            # Ideally, we'd remove the extra field placeholders
            pass
        
    def transfer_2Darray(self, value, row_start, row_end, field_start,
                       field_end, sep=", "):
        """Changes the values of a 2D array in the template relative to the 
        current anchor. This method is specialized for 2D arrays, where each
        row of the array is on its own line.
        
        value - array of values to insert.
        
        row_start - Starting row for inserting the array. This is relative
        to the anchor, and can be negative.
        
        row_end - Final row for the array, relative to the anchor.
        
        field_start - starting field in the given row_start as denoted by 
        delimiter(s). 
        
        field_end - the final field the array uses in row_end. 
        We need this to figure out if the template is too small or large
        
        sep - Optional. Separator to use if we go beyond the template"""

        sub = _SubHelper()
        i = 0
        for row in range(row_start, row_end+1):
            
            j = self.current_row + row
            line = self.data[j]

            sub.set_array(value[i,:], field_start, field_end)
            field_start = 0
            
            newline = re.sub(self.reg, sub.replace_array, line)
            self.data[j] = newline
            
            i += 1

    def clearline(self, row):
        """Replace the contents of a row with the newline character.
        
        row - row number to clear, relative to current anchor."""

        self.data[self.current_row + row] = "\n"
        
    def generate(self):
        """Use the template file to generate the input file."""

        infile = open(self.output_filename, 'w')
        infile.writelines(self.data)
        infile.close()


class FileParser(object):
    """Utility to locate and read data from a file."""
    
    def __init__(self):
        
        self.filename = []
        self.data = []
        
        self.delimiter = " "
        #self.reg = re.compile('[^ \n]+')
        
        self.current_row = 0
        
    def set_file(self, filename):
        """Set the name of the file that will be generated."""
        
        self.filename = filename
        
        inputfile = open(filename, 'r')
        self.data = inputfile.readlines()
        inputfile.close()

    def set_delimiters(self, delimiter):
        """Lets you change the delimiter that is used to identify field
        boundaries."""
        
        if delimiter not in [" ", "columns"]:
            raise NotImplementedError('Only " " and "columns" are currently' + \
                                      ' implemented as delimiters')
        
        self.delimiter = delimiter
        #self.reg = re.compile('[^' + delimiter + '\n]+')
        
    def mark_anchor(self, anchor, occurrence=1):
        """Marks the location of a landmark, which lets you describe data by
        relative position. Note that a forward search begins at the old anchor 
        location. If you want to restart the search for the anchor at the file
        beginning, then call reset_anchor() before mark_anchor. 
        
        anchor: The text you want to search for.
        
        occurence: find nth instance of text; default is 1 (first). Use -1 to
        find last occurence. Reverse searches always start at the end of the
        file no matter the state of any previous anchor."""
        
        if not isinstance(occurrence, int):
            raise ValueError("The value for occurrence must be an integer.")
        
        instance = 0
        if occurrence > 0:
            count = 0
            for line in self.data[self.current_row:]:
                if line.find(anchor) > -1:
                    instance += 1
                    if instance == occurrence:
                        self.current_row += count
                        return
            
                count += 1
                
        elif occurrence < 0:
            count = len(self.data)-1
            for line in reversed(self.data):
                if line.find(anchor) > -1:
                    instance += -1
                    if instance == occurrence:
                        self.current_row = count
                        return
            
                count -= 1
        else:
            raise ValueError("0 is not valid for an anchor occurrence.")            
            
        raise RuntimeError("Could not find pattern %s in output file %s" % \
                           (anchor, self.filename))
        
    def reset_anchor(self):
        """Resets anchor to the beginning of the file."""
        
        self.current_row = 0
        
    def transfer_line(self, row):
        """Returns a whole line, relative to current anchor.
        
        row - number of lines offset from anchor line (0 is anchor line).
        This can be negative."""
        
        return self.data[self.current_row + row]
        
    def transfer_var(self, row, field, fieldend=None):
        """Grabs a single variable relative to the current anchor.
        
        --- If the delimiter is " " ---
        
        row - number of lines offset from anchor line (0 is anchor line).
        This can be negative.
        
        field - which word in line to retrieve.
        
        fieldend - IGNORED
        
        --- If the delimiter is "columns" ---
        
        row - number of lines offset from anchor line (0 is anchor line).
        This can be negative.
        
        field - character position to start
        
        fieldend - position of last character to return"""
        
        j = self.current_row + row
        line = self.data[j]
        
        if self.delimiter == "columns":
            
            if not fieldend:
                fieldend = fieldstart
            
            line = line[(field-1):(fieldend)]
            
            # Let pyparsing figure out if this is a number, and return it
            # as a float or int as appropriate
            data = _parse_line().parseString(line)
            
            # data might have been split if it contains whitespace. If so,
            # just return the whole string
            if len(data) > 1:
                return line
            else:
                return data[0]
        else:
            data = _parse_line().parseString(line)
            return data[field-1]

    def transfer_keyvar(self, key, field, occurrence=1, rowoffset=0):
        """Searches for a key relative to the current anchor and then grabs
        a field from that line.
        
        field -- Which field to transfer. Field 0 is the key.
        
        occurrence -- find nth instance of text; default is 1 (first value
        field). Use -1 to find last occurence. Position 0 is the key
        field, so it should not be used as a value for occurrence.
        
        rowoffset -- Optional row offset from the occurrence of key. This can
        also be negative.
        
        You can do the same thing with a call to mark_anchor and transfer_var.
        This function just combines them for convenience."""

        if not isinstance(occurrence, int):
            raise ValueError("The value for occurrence must be an integer")
        
        instance = 0
        if occurrence > 0:
            row = 0
            for line in self.data[self.current_row:]:
                if line.find(key) > -1:
                    instance += 1
                    if instance == occurrence:
                        break
            
                row += 1
                
        elif occurrence < 0:
            for line in reversed(self.data[:current_row]):
                if line.find(key) > -1:
                    instance += -1
                    if instance == occurrence:
                        break
            
                row -= 1
        else:
            raise ValueError("0 is not valid for an anchor occurrence.")
        
        j = self.current_row + row + rowoffset
        line = self.data[j]
        
        fields = _parse_line().parseString(line.replace(key,"Key_Field"))
        
        return fields[field]

    def transfer_array(self, rowstart, fieldstart, rowend=None, fieldend=None):
        """Grabs an array of variables relative to the current anchor.
        """
        
        j1 = self.current_row + rowstart
        
        if rowend:
            j2 = self.current_row + rowend + 1
        else:
            j2 = j1
            
        if not fieldend:
            fieldend = fieldstart
            
        lines = self.data[j1:j2]

        data = []

        for line in lines:
            if self.delimiter == "columns":
                line = line[(fieldstart-1):fieldend]
                
                # Let pyparsing figure out if this is a number, and return it
                # as a float or int as appropriate
                parsed = _parse_line().parseString(line)
                
                # data might have been split if it contains whitespace. If so,
                # just return the whole string
                if len(parsed) > 1:
                    data.append(line)
                else:
                    data.append(parsed)
            else:
                parsed = _parse_line().parseString(line)
                data.append(parsed[(fieldstart-1):fieldend])
                
        return data
        
                
