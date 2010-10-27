"""
Utilities for reading and writing Fortran namelists.
"""

# pylint: disable-msg=E0611,F0401
from numpy import ndarray
from enthought.traits.trait_base import not_none

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
        for i in range(0, len(self.groups)):
            
            data.append("%s\n" % self.groups[i])
            
            for card in self.cards[i]:
                
                line = "  "
                
                if card.is_comment:
                    line += "%s\n" % (card.value)
                    
                elif isinstance(card.value, (int, float)):
                    line += "%s = %s\n" % (card.name, str(card.value))
                    
                elif isinstance(card.value, (str)):
                    line += "%s = '%s'\n" % (card.name, card.value)
                    
                elif isinstance(card.value, (ndarray)):
                    
                    if len(card.value.shape) == 1:
                        line += card.name + " = "
                        sep = ""
                        for val in card.value:
                            line += sep + str(val)
                            sep = self.delimiter
                        line += "\n"
                            
                    elif len(card.value.shape) == 2:
                        
                        for row in range(0, card.value.shape[0]):
                            line += card.name + "(1," + str(row+1) + ") ="
                            for col in range(0, card.value.shape[1]):
                                line += " " + str(card.value[row, col]) + \
                                         self.delimiter
                            line += "\n"
                        
                    else:
                        raise RuntimeError("Don't know how to handle array" + \
                                           " of %s dimensions" \
                                           % len(card.value.shape))
                    
                # Lists are mainly supported for the Enum Array
                elif isinstance(card.value, list):
                    line += card.name + " = "
                    sep = ""
                    for val in card.value:
                        line += sep + str(val)
                        sep = self.delimiter
                    line += "\n"

                else:
                    raise RuntimeError("Error generating input file. Don't" + \
                                       "know how to handle data in variable" + \
                                       "%s in group %s." % (card.name, \
                                                            self.groups[i]))

                data.append(line)

            # A group with no cards is treated like a free-form entity.
            if len(self.cards[i])>0:        
                data.append("%s\n" % self.terminator)
                
            
        infile = open(self.filename, 'w')
        infile.writelines(data)
        infile.close()
