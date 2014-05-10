"""
Utilities for reading and writing Fortran namelists.
"""

import logging

# pylint: disable-msg=E0611,F0401
import ordereddict

try:
    from numpy import ndarray, array, append, vstack, zeros, \
         int32, int64, float32, float64
except ImportError as err:
    logging.warn("In %s: %r", __file__, err)

from traits.trait_handlers import TraitListObject 

from pyparsing import CaselessLiteral, Combine, ZeroOrMore, Literal, \
                      Optional, QuotedString, Suppress, Word, alphanums, \
                      oneOf, nums, TokenConverter, Group

from openmdao.util.filewrap import ToFloat, ToInteger
from openmdao.util.decorators import stub_if_missing_deps

def _floatfmt(val):
    """ Returns the output format for a floating point number.
    The general format is used with 16 places of accuracy, except for when
    the floating point value is an integer, in which case a decimal point
    followed by a single zero is used."""
    
    if int(val) == val:
        return "%.1f"
    else:
        return "%.16g"

def _intfmt(val):
    """ Returns the output format for an integer """
    
    return "%d"

def _strfmt(val):
    """ Returns the output format for a string """
    
    return "'%s'"
    
def _boolfmt(val):
    """ Returns the output format for a boolean """
    
    if val == True:
        return 'T%.0s'
    else:
        return 'F%.0s'
    
    
def _process_card_info(card):
    """ Function to extract info from a card as returned from PyParsing a
    namelist file. """
    
    name = card.name

    # Sometimes we have a 1D array declared by element
    if card.index:
        index = card.index[0]-1
        
        # Strings go into lists, not arrays
        if isinstance(card[2], str):
            val = card[2:]
            value = ['']*(index+len(val))
            value[index:] = val
        else:
            val = array(card[2:])
            value = zeros(index+len(val))
            value[index:] = val
        
    # Alternate array specification
    elif card.dimension:
        dim = card.dimension
        value = zeros(dim)
        value.fill(card.value)
        
    # Comma-delimited arrays
    elif len(card) > 2:
        value = array(card[1:])
    else:
        value = card.value
        
    return name, value
        
class Card(object):
    """ Data object that stores the value of a single card for a namelist."""
    
    def __init__(self, name, value, is_comment=0):
        
        self.name = name
        self.value = value
        self.is_comment = is_comment

class ToBool(TokenConverter):
    """Converter for PyParsing that is used to turn a token into a Boolean."""
    def postParse( self, instring, loc, tokenlist ):
        """Converter to make token into a bool."""
        
        if tokenlist[0] in ['T', 'True', 'TRUE', 'true', '.TRUE.', '.T.']:
            return True
        elif tokenlist[0] in ['F', 'False', 'FALSE', 'false', '.FALSE.', '.F.']:
            return False
        else:
            raise RuntimeError('Unexpected error while trying to identify a'
                               ' Boolean value in the namelist.')

@stub_if_missing_deps('numpy')
class Namelist(object):
    """Utility to ease the task of constructing a formatted output file."""
    
    def __init__(self, comp):
        
        self.filename = None
        self.delimiter = ", "
        self.terminator = "/"
        self.title = ""
        
        self.comp = comp
        self.groups = []
        self.cards = []
        
        self.currentgroup = 0
    
    def set_filename(self, filename):
        """Set the name of the file that will be generated or parsed.
        
        filename: string
            Name of the file to be written."""
        
        self.filename = filename
        
    def set_title(self, title):
        """Sets the title for the namelist Note that a title is not
        required.
        
        title: string
            The title card in the namelist - generally optional."""
        
        self.title = title
        
    def add_group(self, name):
        """Add a new group to the namelist. Any variables added after this are
        added to this new group.
        
        name: string
            Group name to be added."""
        
        self.groups.append(name)
        self.currentgroup = len(self.groups)-1
        self.cards.append([])
        
    def add_var(self, varpath):
        """Add an openmdao variable to the namelist.
        
        varpath: string
        varpath is the dotted path (e.g., comp1.container1.var1)."""
        
        paths = varpath.split('.')
        name = paths[-1]
            
        value = self.comp.get(varpath)
        
        self.cards[self.currentgroup].append(Card(name, value))
        
    def add_newvar(self, name, value):
        """Add a new variable to the namelist.
        
        name: string
            Name of the variable to be added.
        
        value: int, float, string, ndarray, list, bool
            Value of the variable to be added."""

        self.cards[self.currentgroup].append(Card(name, value))
        
    def add_container(self, varpath='', skip=None):
        """Add every variable in an OpenMDAO container to the namelist. This
        can be used it your component has containers of variables.
        
        varpath: string
            Dotted path of container in the data hierarchy.
            
        skip: list of str
            List of variables to skip printing to the file."""
        
        target_container = self.comp.get(varpath)
        
        if not skip:
            skip = []
            
        for name in sorted(target_container.list_vars()):
            if name not in skip:
                self.add_var("%s.%s" % (varpath, name))
        
    def add_comment(self, comment):
        """Add a comment in the namelist.
        
        comment: string
            Comment text to be added. Text should include comment character
            if one is desired. (Note that a comment character isn't always
            needed in a Namelist. It seems to figure out whether something
            is a comment without it.)"""
        
        self.cards[self.currentgroup].append(Card("C", comment, 1))
        
    def generate(self):
        """Generates the input file. This should be called after all cards
        and groups are added to the namelist."""

        data = []
        data.append("%s\n" % self.title)
        for i, group_name in enumerate(self.groups):
            
            # Groups get a '&', freeform cards don't.
            if self.cards[i]:
                data.append("&%s\n" % group_name)
            else:
                data.append("%s\n" % group_name)
                
            for card in self.cards[i]:
                
                if card.is_comment:
                    line = "  %s\n" % (card.value)
                    
                elif isinstance(card.value, bool):
                    fstring = "  %s = " + _boolfmt(card.value) + "\n"
                    line = fstring % (card.name, card.value)
                    
                elif isinstance(card.value, int):
                    fstring = "  %s = " + _intfmt(card.value) + "\n"
                    line = fstring % (card.name, card.value)
                    
                elif isinstance(card.value, float):
                    fstring = "  %s = " + _floatfmt(card.value) + "\n"
                    line =  fstring % (card.name, card.value)
                    
                elif isinstance(card.value, str):
                    fstring = "  %s = " + _strfmt(card.value) + "\n"
                    line =  fstring % (card.name, card.value)
                    
                # Lists are mainly supported for the Enum Array
                elif isinstance(card.value, list):
                    line = "  %s = " % (card.name)
                    sep = ""
                    for val in card.value:
                        
                        # We can have integer, real, or string lists
                        if isinstance(val, bool):
                            fmt = _boolfmt
                        elif isinstance(val, (int, int32, int64)):
                            fmt = _intfmt
                        elif isinstance(val, (float, float32, float64)):
                            fmt = _floatfmt
                        else:
                            fmt = _strfmt
                    
                        fstring = sep + fmt(val)
                        line += fstring % val
                        sep = self.delimiter
                        
                    line += "\n"

                elif isinstance(card.value, (ndarray)):
                    
                    # We can have integer, real, or string arrays
                    if card.value.dtype == bool:
                        fmt = _boolfmt
                    elif card.value.dtype in (int, int32, int64):
                        fmt = _intfmt
                    elif card.value.dtype in (float, float32, float64):
                        fmt = _floatfmt
                    else:
                        fmt = _strfmt
                    
                    # We don't need to output 0D arrays
                    if len(card.value) == 0:
                        continue
                    
                    elif len(card.value.shape) == 1:
                        line = "  %s = " % (card.name)
                        sep = ""
                        for val in card.value:
                            fstring = "%s" + fmt(val)
                            line += fstring % (sep, val)
                            sep = self.delimiter
                        line += "\n"
                            
                    elif len(card.value.shape) == 2:
                        
                        line = "  "
                        for row in range(0, card.value.shape[0]):
                            line += card.name + "(1," + str(row+1) + ") ="
                            for col in range(0, card.value.shape[1]):
                                val = card.value[row, col]
                                fstring = " " + fmt(val) + "%s"
                                line += fstring % (val, self.delimiter)
                            line += "\n"
                        
                    else:
                        raise RuntimeError("Don't know how to handle array"
                                           " of %s dimensions"
                                           % len(card.value.shape))
                    
                else:
                    raise RuntimeError("Error generating input file. Don't"
                                       " know how to handle data in variable"
                                       " %s in group %s." % (card.name,
                                                             group_name))

                data.append(line)

            # A group with no cards is treated like a free-form entity.
            if len(self.cards[i])>0:        
                data.append("%s\n" % self.terminator)
                
            
        outfile = open(self.filename, 'w')
        outfile.writelines(data)
        outfile.close()
        
    def parse_file(self):
        """Parses an existing namelist file and creates a deck of cards to
        hold the data. After this is executed, you need to call the ``load_model()``
        method to extract the variables from this data structure."""
        
        infile = open(self.filename, 'r')
        data = infile.readlines()
        infile.close()
        
        # Lots of numerical tokens for recognizing various kinds of numbers
        digits = Word(nums)
        dot = "."
        sign = oneOf("+ -")
        ee = CaselessLiteral('E') | CaselessLiteral('D')
    
        num_int = ToInteger(Combine( Optional(sign) + digits ))
        
        num_float = ToFloat(Combine( Optional(sign) + 
                            ((digits + dot + Optional(digits)) |
                             (dot + digits)) +
                             Optional(ee + Optional(sign) + digits)
                            ))
        
        # special case for a float written like "3e5"
        mixed_exp = ToFloat(Combine( digits + ee + Optional(sign) + digits ))
        
        # I don't suppose we need these, but just in case (plus it's easy)
        nan = ToFloat(oneOf("NaN Inf -Inf"))
        
        numval = num_float | mixed_exp | num_int | nan
        strval =  QuotedString(quoteChar='"') | QuotedString(quoteChar="'")
        b_list = "T TRUE True true F FALSE False false .TRUE. .FALSE. .T. .F."
        boolval = ToBool(oneOf(b_list))
        fieldval = Word(alphanums)
        
        # Tokens for parsing a line of data
        numstr_token = numval + ZeroOrMore(Suppress(',') + numval) \
                   | strval
        data_token = numstr_token | boolval
        index_token = Suppress('(') + num_int + Suppress(')')
        
        card_token = Group(fieldval("name") +
                           Optional(index_token("index")) +
                           Suppress('=') +
                           Optional(num_int("dimension") + Suppress('*')) +
                           data_token("value") +
                           Optional(Suppress('*') + num_int("dimension")))
        multi_card_token = (card_token + ZeroOrMore(Suppress(',') + card_token))
        array_continuation_token = numstr_token.setResultsName("value")
        array2D_token = fieldval("name") + Suppress("(") + \
                        Suppress(num_int) + Suppress(',') + \
                        num_int("index") + Suppress(')') + \
                        Suppress('=') + numval + \
                        ZeroOrMore(Suppress(',') + numval)
        
        # Tokens for parsing the group head and tai
        group_end_token = Literal("/") | \
                          Literal("$END") | Literal("$end") | \
                          Literal("&END") | Literal("&end")
        group_name_token = (Literal("$") | Literal("&")) + \
                           Word(alphanums).setResultsName("name") + \
                           Optional(multi_card_token) + \
                           Optional(group_end_token)
        
        # Comment Token
        comment_token = Literal("!")
        
        # Loop through each line and parse.
        
        current_group = None
        for line in data:
            line_base = line
            line = line.strip()
            
            # blank line: do nothing
            if not line:
                continue
                
            if current_group:
                
                # Skip comment cards
                if comment_token.searchString(line):
                    pass
                
                # Process orindary cards
                elif multi_card_token.searchString(line):
                    cards = multi_card_token.parseString(line)

                    for card in cards:
                        name, value = _process_card_info(card)
                        self.cards[-1].append(Card(name, value))
                        
                # Catch 2D arrays like -> X(1,1) = 3,4,5
                elif array2D_token.searchString(line):
                    card = array2D_token.parseString(line)
                    
                    name = card[0]
                    index = card[1]
                    value = array(card[2:])
                    
                    if index > 1:
                        old_value = self.cards[-1][-1].value
                        new_value = vstack((old_value, value))
                        self.cards[-1][-1].value = new_value
                    else:
                        self.cards[-1].append(Card(name, value))
                    
                # Arrays can be continued on subsequent lines
                # The value of the most recent card must be turned into an
                # array and appended
                elif array_continuation_token.searchString(line):
                    card = array_continuation_token.parseString(line)
                    
                    if len(card) > 1:
                        element = array(card[0:])
                    else:
                        element = card.value
                        
                    if isinstance(self.cards[-1][-1].value, ndarray):
                        new_value = append(self.cards[-1][-1].value, element)
                    else:
                        new_value = array([self.cards[-1][-1].value, element])
                    
                    self.cards[-1][-1].value = new_value
                    
                # Lastly, look for the group footer
                elif group_end_token.searchString(line):
                    current_group = None
                    
                # Everything else must be a pure comment
                else:
                    print "Comment ignored: %s" % line.rstrip('\n')

                # Group ending '/' can also conclude a data line.
                if line[-1] == '/':
                    current_group = None
                    
                #print self.cards[-1][-1].name, self.cards[-1][-1].value
            else:
                group_name = group_name_token.searchString(line)
                
                # Group Header
                if group_name:
                    group_name = group_name_token.parseString(line)
                    current_group = group_name.name
                    self.add_group(current_group)
                    
                    # Sometimes, variable definitions are included on the
                    # same line as the namelist header
                    if len(group_name) > 2:
                        cards = group_name[2:]
                        
                        for card in cards:
                            # Sometimes an end card is on the same line.
                            if group_end_token.searchString(card):
                                current_group = None
                            else:
                                name, value = _process_card_info(card)
                                self.cards[-1].append(Card(name, value))

                # If there is an ungrouped card at the start, take it as the
                # title for the analysis
                elif len(self.cards) == 0 and self.title == '':
                    self.title = line
                    
                # All other ungrouped cards are saved as free-form (card-less)
                # groups.
                # Note that we can't lstrip because column spacing might be
                # important.
                else:
                    self.add_group(line_base.rstrip())
                    

    def load_model(self, rules=None, ignore=None, single_group=-1):
        """Loads the current deck into an OpenMDAO component.
        
        rules: dict of lists of strings (optional)
             An optional dictionary of rules can be passed if the component has a
             hierarchy of containers for its input variables. If no rules dictionary
             is passed, ``load_model`` will attempt to find each namelist variable in the
             top level of the model hierarchy.
        
        ignore: list of strings (optional)
             List of variable names that can safely be ignored.
        
        single_group: integer (optional)
             Group id number to use for processing one single namelist group. Useful
             if extra processing is needed or if multiple groups have the same name.
        
             Returns a tuple containing the following values:
             (empty_groups, unlisted_groups, unlinked_vars). These need to be
             examined after calling ``load_model`` to make sure you loaded every
             variable into your model.
        
        empty_groups: ordereddict( integer : string )
             Names and ID number of groups that don't have cards. This includes 
             strings found at the top level that aren't comments; these need to
             be processed by your wrapper to determine how the information fits
             into your component's variable hierarchy.
        
        unlisted_groups: ordereddict( integer : string )
             This dictionary includes the names and ID number of groups that have
             variables that couldn't be loaded because the group wasn't mentioned
             in the rules dictionary.
        
        unlinked_vars: list containing all variable names that weren't found
        in the component.
        
        """

        # See Pylint W0102 for why we do this
        if not ignore:
            ignore = []
        
        if not self.groups:
            msg = "Input file must be read with parse_file before " \
                  "load_model can be executed."
            raise RuntimeError(msg)
            
        if single_group > -1:
            use_group = {single_group : self.groups[single_group]}.iteritems()
        else:
            use_group = enumerate(self.groups)
            
        empty_groups = ordereddict.OrderedDict()
        unlisted_groups = ordereddict.OrderedDict()
        unlinked_vars = []
        used_groups = []
        for i, group_name in use_group:
            
            # Report all groups with no cards
            if len(self.cards[i]) == 0:        
                empty_groups[i] = group_name
                continue
            
            # If a group_name appears twice, we really don't know where to
            # stick the variables, and there are potential data overwrite
            # issues. Those cases have to be handled individually.
            if group_name in used_groups:
                unlisted_groups[i] = group_name
                continue
            else:
                used_groups.append(group_name)
                
            # Process the cards in this group
            for card in self.cards[i]:
                
                name = card.name
                value = card.value
                found = False
                
                if rules:
                    
                    # If the group isn't in the rules dict, we can't handle
                    # it now. A separate function call will be required.
                    try:
                        containers = rules[group_name]
                    except KeyError:
                        unlisted_groups[i] = group_name
                        #print "Group not found: %s" % group_name
                        break
                    
                    for container in containers:
                        # Note: FORTRAN is case-insensitive, OpenMDAO is not
                        varpath1 = "%s.%s" % (container, name)
                        varpath2 = "%s.%s" % (container, name.lower())
                        
                        for item in [varpath1, varpath2]:
                            if self.comp.contains(item):
                                found = True
                                varpath = item
                                break
                        
                else:
                    for item in [name, name.lower()]:
                        if item in self.comp.list_vars():
                            found = True
                            varpath = item
                            break
                
                if not found:
                    if name not in ignore and name.lower() not in ignore:
                        unlinked_vars.append(name)
                        print "Variable not found: " + \
                              "%s in group %s." % (name, group_name)

                else:
                    
                    # 1D arrays must become ndarrays
                    target = self.comp.get(varpath)

                    if isinstance(target, ndarray) and \
                       isinstance(value, (float, int)):
                        value = array([value])
                        
                    # Enum ndarrays must become lists
                    elif isinstance(target, TraitListObject):
                        if isinstance(value, ndarray):
                            value = list(value)
                        elif isinstance(value, list):
                            pass
                        else:
                            value = [value]
                        
                    self.comp.set(varpath, value)
                    
                    #print varpath, value
                
        return empty_groups, unlisted_groups, unlinked_vars

    def find_card(self, group, name):
        """Returns the value for a given namelist variable in a given group.
        
        group: string
            namelist group name.
            
        name: string
            namelist variable name."""
        
        group_id = self.groups.index(group)
        
        for card in self.cards[group_id]:
            if card.name == name:
                return card.value
            
        msg = "Variable %s" % name + \
              " not found in namelist %s." % group
        raise RuntimeError(msg)
        
