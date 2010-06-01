"""
Utilities for file wrapping

Note: This is a work in progress.
"""

import re

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

        
class InputFileGenerator(object):
    """Utility to generate an intput file from a template.
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

    def set_delimiter(self, delimiter):
        """Lets you change the delimiter that is used to identify field
        boundaries."""
        
        self.delimiter = delimiter
        self.reg = re.compile('[^' + delimiter + '\n]+')
        
    def mark_anchor(self, anchor):
        """Marks the location of a landmark, which lets you describe data by
        relative position."""
        
        count = 0
        for line in self.data:
            if line.find(anchor) > -1:
                self.current_row = count
                return
            
            count += 1
            
        raise RuntimeError("Could not find pattern %s in template file %s" % \
                           (anchor, self.template_filename))
        
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
        
        value - array of values to insert
        
        row_start - starting row for inserting the array. This is relative
        to the anchor, and can be negative
        
        field_start - starting field in the given row_start as denoted by 
        delimiter(s). 
        
        field_end - the final field the array uses in row_end. 
        We need this to figure out if the template is too small or large
        
        row_end - Optional. Use if the array wraps to cover additional lines
        
        sep - Optional. Separator to use if we go beyond the template"""
        
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
        
        value - array of values to insert
        
        row_start - Starting row for inserting the array. This is relative
        to the anchor, and can be negative
        
        row_end - Final row for the array, relative to the anchor
        
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
        
        row - row number to clear, relative to current anchor"""

        self.data[self.current_row + row] = "\n"
        
    def generate(self):
        """Use the template file to generate the input file."""

        infile = open(self.output_filename, 'w')
        infile.writelines(self.data)
        infile.close()
        