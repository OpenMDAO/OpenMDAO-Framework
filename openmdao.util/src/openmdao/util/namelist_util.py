"""
Utilities for reading and writing Fortran namelists.
"""

# pylint: disable-msg=E0611,F0401
import ordereddict

from numpy import ndarray, array, append
from enthought.traits.trait_base import not_none

from pyparsing import CaselessLiteral, Combine, ZeroOrMore, Literal, \
                      Optional, QuotedString, Suppress, Word, alphanums, \
                      oneOf, nums

from openmdao.util.filewrap import ToFloat, ToInteger


class Card(object):
    """ Data object that stores the value of a single card for a namelist."""
    
    def __init__(self, name, value, is_comment=0):
        
        self.name = name
        self.value = value
        self.is_comment = is_comment

        
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
        """Sets the title for the namelist.
        
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
        varpath is the dotted path (e.g., comp1.comp2.var1)."""
        
        paths = varpath.split('.')
        name = paths[-1]
            
        value = self.comp.get(varpath)
        
        self.cards[self.currentgroup].append(Card(name, value))
        
    def add_newvar(self, name, value):
        """Add a new private variable to the namelist.
        
        name: string
            Name of the variable to be added.
        
        value: int, float, string, ndarray, list
            Value of the variable to be added."""

        self.cards[self.currentgroup].append(Card(name, value))
        
    def add_container(self, varpath):
        """Add every variable in an OpenMDAO container to the namelist."""
        
        target_container = self.comp.get(varpath)
        for name in target_container.keys(iotype=not_none):
            self.add_var(varpath+'.'+name)
        
    def add_comment(self, comment):
        """Add a comment in the namelist.
        
        comment: string
            Comment text to be added. Text should include comment character"""
        
        self.cards[self.currentgroup].append(Card("C", comment, 1))
        
    def generate(self):
        """Create the input file. This should be called after all cards
        and groups are added to the namelist."""

        data = []
        data.append("%s\n" % self.title)
        for i, group_name in enumerate(self.groups):
            
            data.append("%s\n" % group_name)
            for card in self.cards[i]:
                
                if card.is_comment:
                    line = "  %s\n" % (card.value)
                    
                elif isinstance(card.value, int):
                    line = "  %s = %s\n" % (card.name, str(card.value))
                    
                elif isinstance(card.value, float):
                    line = "  %s = %.15g\n" % (card.name, card.value)
                    
                elif isinstance(card.value, str):
                    line = "  %s = '%s'\n" % (card.name, card.value)
                    
                elif isinstance(card.value, (ndarray)):
                    
                    # We don't need to output 0D arrays
                    if len(card.value) == 0:
                        continue
                    
                    elif len(card.value.shape) == 1:
                        line = "  %s = " % (card.name)
                        sep = ""
                        for val in card.value:
                            line += "%s%.15g" % (sep, val)
                            sep = self.delimiter
                        line += "\n"
                            
                    elif len(card.value.shape) == 2:
                        
                        line = "  "
                        for row in range(0, card.value.shape[0]):
                            line += card.name + "(1," + str(row+1) + ") ="
                            for col in range(0, card.value.shape[1]):
                                line += " %.15g%s" % (card.value[row, col], 
                                                      self.delimiter)
                            line += "\n"
                        
                    else:
                        raise RuntimeError("Don't know how to handle array" + \
                                           " of %s dimensions" \
                                           % len(card.value.shape))
                    
                # Lists are mainly supported for the Enum Array
                elif isinstance(card.value, list):
                    line = "  %s = " % (card.name)
                    sep = ""
                    for val in card.value:
                        line += sep + str(val)
                        sep = self.delimiter
                    line += "\n"

                else:
                    raise RuntimeError("Error generating input file. Don't" + \
                                       "know how to handle data in variable" + \
                                       "%s in group %s." % (card.name, \
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
        hold the data."""
        
        infile = open(self.filename, 'r')
        data = infile.readlines()
        infile.close()
        
        # Tokens for parsing the group head and tail
        group_name_token = (Literal("$") | Literal("&")) + \
                           Word(alphanums).setResultsName("name")
        group_end_token = Literal("/") | Literal("$END") | Literal("$end")
        
        # Lots of numerical tokens for recognizing various kinds of numbers
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
        
        # I don't suppose we need these, but just in case (plus it's easy)
        nan = ToFloat(oneOf("NaN Inf -Inf"))
        
        numval = num_float | mixed_exp | num_int | nan
        strval =  QuotedString(quoteChar='"') | QuotedString(quoteChar="'")
        #strval =  QuotedString
        
        # Tokens for parsing a line of data
        data_token = numval + ZeroOrMore(Suppress(',') + numval) | strval
        card_token = Word(alphanums).setResultsName("name") + Suppress('=') + \
                data_token.setResultsName("value")
        array_continuation_token = data_token.setResultsName("value")
        
        # Comment Token
        comment_token = Literal("!")
        
        # Loop through each line and parse.
        
        current_group = None
        for line in data:
            #line = line.strip()
            
            # blank line: do nothing
            if not line.strip():
                continue
                
            if current_group:
                
                # Skip comment cards
                if comment_token.searchString(line):
                    pass
                
                # Process orindary cards
                elif card_token.searchString(line):
                    card = card_token.parseString(line)
                    name = card.name
                    
                    # Comma-delimited arrays
                    if len(card) > 2:
                        value = array(card[1:])
                    else:
                        value = card.value
                        
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

                #print self.cards[-1][-1].name, self.cards[-1][-1].value
            else:
                group_name = group_name_token.searchString(line)
                
                # Group Header
                if group_name:
                    group_name = group_name_token.parseString(line)
                    current_group = group_name.name
                    self.add_group(current_group)
                    #print current_group

                # If there is an ungrouped card at the start, take it as the
                # title for the analysis
                elif len(self.cards) == 0 and self.title == '':
                    self.title = line.rstrip()
                    
                # All other ungrouped cards are saved as free-form (card-less)
                # groups.
                else:
                    self.add_group(line.rstrip())
                    

    def load_model(self, rules=None, ignore=[], single_group=-1):
        """Loads the current deck into an OpenMDAO component.
        
        rules: Dict of Lists of Strings (Optional)
        An optional dictionary of rules can be passed if the component has a
        hierarchy of containers for its input variables. If no rules dictionary
        is passed, load_model will attempt to find each namelist variable in the
        top level of the model hierarchy.
        
        ignore: List of Strings (Optional)
        List of variable names that can safely be ignored.
        
        single_group: integer (Optional)
        Just process one single namelist group. Useful if extra processing is
        needed, or if multiple groups have the same name."""

        if single_group > -1:
            use_group = {single_group : self.groups[single_group]}.iteritems()
        else:
            use_group = enumerate(self.groups)
            
        empty_groups = ordereddict.OrderedDict()
        unlisted_groups = ordereddict.OrderedDict()
        unlinked_vars = []
        for i, group_name in use_group:
            
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
                        
                        if self.comp.contains(varpath1):
                            found = True
                            varpath = varpath1
                            break
                        
                        if self.comp.contains(varpath2):
                            found = True
                            varpath = varpath2
                            break
                
                else:
                    if self.comp.contains(name):
                        found = True
                        varpath = name
                
                if not found:
                   if name not in ignore and name.lower() not in ignore:
                       unlinked_vars.append(group_name)
                       print "Variable not found: " + \
                             "%s in group %s." % (name, group_name)

                else:
                    
                    # 1D arrays must become ndarrays
                    target = self.comp.get(varpath)
                    if isinstance(target, ndarray) and \
                       isinstance(value, (float, int)):
                        value = array([value])
                        
                    # Enum ndarrays must become lists
                    elif isinstance(target, list) and \
                       isinstance(value, ndarray):
                        value = list(value)
                        
                    self.comp.set(varpath, value)
                    
                    #print varpath, value
                
            # Report all groups with no cards
            if len(self.cards[i]) == 0:        
                empty_groups[i] = group_name
                
        return empty_groups, unlisted_groups, unlinked_vars

