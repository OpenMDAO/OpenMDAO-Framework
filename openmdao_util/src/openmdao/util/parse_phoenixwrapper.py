"""
Parses the variable definition section of a Phoenix Integration ModelCenter
component wrapper and generates an OpenMDAO component stub.
"""

from pyparsing import Suppress, Word, alphanums, dictOf, oneOf, printables, \
                      removeQuotes, sglQuotedString, commaSeparatedList, \
                      ParseResults

#public symbols
__all__ = ["parse_phoenixwrapper"]
           
def _parse_phoenixline():
    """Parse a single line containing a variable definition."""
        
    data = ( Suppress("variable:") + \
             Word(alphanums).setResultsName("name") + \
             Word(printables).setResultsName("type") + \
             oneOf(""" input output """).setResultsName("iotype") + \
             dictOf(Word(alphanums), Suppress("=")+ \
             (Word(alphanums+","+'-'+'.')|commaSeparatedList)) )
    
    return data

def _parse_phoenixgroup():
    """Parse a single line containing a group definition."""
        
    data = ( Suppress(Word(alphanums)) + \
           sglQuotedString.setParseAction(removeQuotes).setResultsName("name") )
    
    return data

_phoenix2trait = {
    'boolean': ('Bool'),
    'double': ('Float'),
    'double[]': ('Array','float32'),
    'double[][]': ('Array','float32'),
    'file': ('File'),
    'integer': ('Int'),
    'integer[]': ('Array','int32'),
    'integer[][]': ('Array','int32'),
    'string': ('Str'),
    'string[]': ('Array','str')
}

_phoenix2default = {
    'boolean': "False",
    'double': "0.0",
    'integer': "0",
    'string': "''",
}

_phoenix2iotype = {
    'input': ('in'),
    'output': ('out')
}

# Note these are direct replacemnts, for ones that are too risky to str replace
_unit_full_replace = {
    'in': ('inch'),
    'in*in': ('inch*inch'),
    'R': ('degR'),
    'rpm': ('1/min'),
    'in2rpm2': ('(inch*inch)/(min*min)'),
    'ft-lb/lbm-R': ('(ft*lb)/(lb*degR)'),
}

# Note, these are string replacements, useful for formulas.
_unit_part_replace = {
    'mile': ('mi'),
    'hr': ('h'),
    'ft2': ('ft*ft'),
    'ft3': ('ft*ft*ft'),
    'in2': ('inch*inch'),
    'in3': ('inch*inch*inch'),
    'kt': ('nmi'),
    'years': ('year'),
    'deg F': ('degF'),
    'deg C': ('degC'),
    'lb/ft2': ('psf'),
    'mph': ('mi/h'),
    'sec': ('s'),
    'btu': ('Btu'),
    'lbm': ('lb'),
    '/engine': (''),
    'ft/s2': ('ft/(s*s)'),
}

# These units are replaced with unitless quantities.
_unit_ignore = ('%', 'EPNdB', 'dB')

def _gen_publicvar(data):
    """Generates the OpenMDAO variable line given a dictionary of info.
    """
    
    sep = ", "
    dtype = None
    
    name = data.name
    
    try:
        trait = _phoenix2trait[data.type]
    except KeyError:
        raise KeyError("Unhandled Modelcenter input type - %s" % data.type)

    iotype = "iotype='" + _phoenix2iotype[data.iotype] + "'"
    
    # Arrays use the Array trait, which has no default value
    default = ""
    if trait[0] == "Array":
        dtype = trait[1]
        trait = "Array"
    else:
        if data.default:
            
            default_value = data.default
            
            if trait == "Str":
                if default_value[-1] != default_value[0]:
                    default_value += default_value[0]
                    
            default = default_value + sep
            
            # Modelcenter doesn't capitalize True and False
            if trait == "Bool":
                default = default.replace("true","True")
                default = default.replace("false","False")
                
        elif data.type in _phoenix2default:
            default = _phoenix2default[data.type] + sep

    # Process Enums
    if data.enumValues:
        trait = "Enum"
        default = "(" +data.enumValues + "), "
        
    var = name + " = " + trait + "(" + default + iotype
    
    if trait == "Array":
        var += sep + "dtype=numpy_" + dtype
        
    if trait == "File" and _phoenix2iotype[data.iotype] == 'out':
        var += sep + "path='Insert_Filename_Here'"
        
    if data.units:
        
        unit = data.units
        unit = unit.strip("'")
        
        if unit not in _unit_ignore:
            if unit in _unit_full_replace:
                unit = _unit_full_replace[unit]
            else:
                for unit_mc, unit_op in _unit_part_replace.iteritems():
                    unit = unit.replace(unit_mc, unit_op)
                
            var += sep + "units='" + unit + "'"
    
    if data.description:
        
        # if you don't end with a quote, comma-seperated lists get
        # screwed up.
        if isinstance(data.description, ParseResults):
            desc = data.description[0]
            for item in data.description[1:]:
                desc += ', ' + item
            desc += "'"
        else:
            desc = data.description
            
        var += sep + "desc=" + desc

    
    if data.enumAliases:
        aliases = data.enumAliases
        
        # remove pesky quotes in Enum alias list
        for i, v in enumerate(aliases):
            aliases[i] = v.strip("'")
            
        var += sep + "aliases=" + str(tuple(aliases))
            
    var += ")\n"
    
    return var, trait, dtype
        
# Main Code

def parse_phoenixwrapper(infile, outfile, compname):
    """
    Generates a dummy component given a Phoenix Integration Modelcenter script
    wrapper. The first section of this wrapper is parsed, and the appropriate
    variables and containers are placed in the new OpenMDAO component.
    
    infile  - ModelCenter scriptwrapper.
    
    outfile - File containing new OpenMDAO component skeleton.
    
    compname - Name for new component.
    """
    
    # Note: special processing since Python strings don't need escaped quotes.
    phoenixdata = open(infile,'r').read().replace('"',"'").replace("\\'",'"')
    
    openmdao = open(outfile,'w')

    group = ""
    groups = []
    count_var = 0
    imports = []
    numpy_imports = []
    variables = {}
    variables[group] = ""
    tab = "    "
    
    for line in phoenixdata.split("\n"):
    
        # blank line: do nothing
        if not line: 
            continue
    
        # Section separator, no more variables
        if line[0:4] == "####":
            break
    
        # Line is commented out: do nothing
        if line[0] == "#":
            continue
    
        # Group Header
        if line[0:8] == "setGroup" or line[0:8] == "setgroup":
            group = _parse_phoenixgroup().parseString(line)
            group = group.name
            if group not in groups:
                groups.append(group)
                variables[group] = ""
                
        else:
    
            # Variable
            fields = _parse_phoenixline().parseString(line)
            count_var += 1
    
            (varline, trait, dtype) = _gen_publicvar(fields)
    
            if trait not in imports:
                imports.append(trait)
            
            if dtype and dtype not in numpy_imports:
                numpy_imports.append(dtype)
            
            variables[group] += tab + varline

    # Write output file
    
    openmdao.write("\"\"\"\n")
    openmdao.write("OpenMDAO Wrapper for %s\n" % compname)
    openmdao.write("Automatically generated from " + \
                    "%s with parse_phoenixwrapper.\n" % infile)
    openmdao.write("\"\"\"\n\n")

    text = ""
    for imp in numpy_imports:
        text += "from numpy import %s as numpy_%s\n" % (imp, imp)
        
    if len(groups) > 0:
        text += "\nfrom openmdao.main.api import Component, Container\n"
    else:
        text += "\nfrom openmdao.main.api import Component\n"
        
    sep = ""
    text += "from openmdao.main.datatypes.api import "
    for imp in imports:
        text += sep + imp
        sep = ", "
        
    text += "\n"
    
    openmdao.write(text)

    # Create all missing containers
    constructs = {}
    constructs[""] = ""
    for group in sorted(groups):
        
        constructs[group] = ""
        containers = group.split('.')
        
        fullname = ""
        sep = ""
        for level in containers[0:-1]:
            fullname += sep + level
            sep = "."
            if fullname not in groups:
                groups.append(fullname)
                variables[fullname] = ""
                constructs[fullname] = ""

                
    # Prepare the container constructor calls
    for group in sorted(groups):
                
        containers = group.split('.')
        container_name = group.replace(".","_")

        parentname = ""
        if len(containers) > 1:
            sep = ""
            for level in containers[0:-1]:
                parentname += sep + level
                sep = "."

        childname = containers[-1]
        text = tab + tab + "self.add('%s',  %s_%s())\n" % \
                            (childname, compname, container_name)

        constructs[parentname] += text

    # Write the variables.
    for group in sorted(groups, reverse=True):

        # Do top level last.
        if group != "":
        
            container_name = compname + "_" + group.replace(".","_")
            text = "\n"
            text += "class " + container_name + "(Container):\n"
            
            text += tab + '"""Container for %s"""\n\n' % str(group)
            text += tab + "# OpenMDAO Variables\n"
            openmdao.write(text)
        
            openmdao.write(variables[group])
            
            # Write the constructors.
            if constructs[group] != "":
                text = "\n"
                text += tab + "def __init__(self):\n"
                text += tab + tab + '"""Constructor for the %s component"""' \
                        % container_name
                text += "\n\n"
                text += tab + tab + "super(%s, self).__init__()" \
                        % container_name
                text += "\n\n"
                text += tab + tab + "# Variable Containers\n"
                openmdao.write(text)
                openmdao.write(constructs[group])
        
    # Top level class of the Wrapper.
    text = "\n\n"
    text += "class " + compname + "(Component):\n"
    
    text += tab + '"""Wrapper for %s"""\n\n' % compname
    text += tab + "# OpenMDAO Variables\n"
    openmdao.write(text)

    openmdao.write(variables[""])
    
    text = "\n"
    text += tab + "def __init__(self):\n"
    text += tab + tab + '"""Constructor for the %s component"""' % compname
    text += "\n\n"
    text += tab + tab + "super(%s, self).__init__()" % compname
    text += "\n\n"
    text += tab + tab + "# Variable Containers\n"
    openmdao.write(text)
    openmdao.write(constructs[""])
    
    print "Stats: %d groups, %d variables." % (len(groups), count_var)
    

