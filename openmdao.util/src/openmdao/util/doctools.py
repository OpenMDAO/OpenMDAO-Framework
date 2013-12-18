"""
A utility to extract Traits information from the code and get it into the
Sphinx documentation. 

.. note:: No traits docs will be generated unless the class containing the traits has a doc string!
"""

from sys import maxint, float_info

from traits.api import HasTraits, MetaHasTraits, Any, Python, Instance
from traits.trait_types import _InstanceArgs
from inspect import getmro, ismodule, getmembers, isfunction, isclass

from openmdao.main.datatypes.api import Slot, Event

excludes = (Any, Python, Event, type)

def _print_funct(funct, args, kw):
    arglst = []
    if args:
        for arg in args:
            if isinstance(arg, basestring):
                arglst.append("'%s'" % arg)
            else:
                arglst.append("%s" % arg)
    argstr = ', '.join(arglst)
    kwlst = []
    if kw:
        for k, v in kw.items():
            if isinstance(v, basestring):
                kwlst.append("%s='%s'" % (k, v))
            else:
                kwlst.append("%s=%s" % (k, v))
    kwstr = ', '.join(kwlst)
    if kwstr and len(argstr)>0:
        kwstr = ', ' + kwstr
    return "%s(%s%s)" % (funct.__name__, argstr, kwstr)

def _get_instance_default(trait):
    if trait.factory:
        return _print_funct(trait.factory, trait.args, trait.kw)
    elif isinstance(trait.default_value, _InstanceArgs):
        return _print_funct(trait.default_value.args[0],
                            trait.default_value.args[1:],
                            trait.default_value.kw)
    return trait.default_value

def get_traits_info(app, what, name, obj, options, lines):
    """
    Gets traits info.
    """

    #Get the API information for OpenMDAO
    if name.endswith(".api") and ismodule(obj):

        #get functions
        fns = getmembers(obj, isfunction)
        for n, v in fns:
            filename = v.__module__ + ".py"
            lines.append(":ref:`%s<%s>`" % (n, filename))
            lines.append('\n')
      
        #get classes
        cls = getmembers(obj, isclass)
        for n1, v1 in cls:
            module = v1.__module__
            if module == "traits.trait_types":
                filename2 = "http://docs.enthought.com/traits/traits_api_reference/trait_types.html"
                lines.append("`%s <%s>`_" % (n1, filename2))
            else:
                filename2 = module + ".py"
                lines.append(":ref:`%s<%s>`" % (n1, filename2))
            lines.append('\n')
    
    if not isinstance(obj, (MetaHasTraits, HasTraits)):
        return
    
    #gets a list of the class heirarchy.
    classes = getmro(obj)
    
    #gets a dict of all traits in this class, including those from base classes  
    this_class_traits = obj.class_traits()
   
    #gets a dict of all traits in this class' base class
    base_class_traits = {}
    for cls in classes[1:]:
        if hasattr(cls, 'class_traits'): 
            base_class_traits.update(cls.class_traits())

    #The things we want to keep will be those that are
    #unique to the current class, and those that appear in the
    #base but are overridden by the current class.  So keep each
    #item in this class' list that doesn't appear in the 
    #base classes' lists.
    keepers = {}
    
    for trt, trt_val in this_class_traits.items():
        if not base_class_traits.has_key(trt):
            keepers[trt] = trt_val        
        else:
            #the names are the same, so check the objects
            #to see if they are the same.  if they aren't,
            # there's an override in this class; keep it!
            if not trt_val == base_class_traits[trt]:
                keepers[trt] = trt_val
                
    keepers_in = {}
    keepers_out = {}
    keepers_instance = {}
    keepers_undefined = {}
    
    #Now we need to SORT the traits by input/output type.
    for t, val in keepers.items():
        #As long as it's not an excluded type, add it.
        if not isinstance(val.trait_type, excludes):
            if val.trait_type._metadata.has_key("iotype"):
                if val.trait_type._metadata["iotype"] == "in":
                    keepers_in[t] = val
                elif val.trait_type._metadata["iotype"] == "out":
                    keepers_out[t] = val
            elif type(val.trait_type).__name__ in ["Instance","Slot"]:
                keepers_instance[t] = val        
            else:
                keepers_undefined[t] = val
                
    dicts = (keepers_instance, keepers_in, keepers_out, keepers_undefined)
    
    dontdo_meta = set(['iotype', 'units', 'low', 'high', 'type', 
                       'desc', 'instance_handler', 'parent', 'array'])
    for dic in dicts:
        sortedDict = _sortedDictVals(dic)
        for t, val in sortedDict:
            lines.append('')
            #Now just need to spit out the traits in the proper format into the
            # documentation 
            if val.is_trait_type(Instance) or val.is_trait_type(Slot):
                lines.extend(["*%s* (%s) **%s**"
                              % (type(val.trait_type).__name__,
                                 val.trait_type.klass.__name__, t)])
            else:
                lines.extend(["*%s* **%s**" % (type(val.trait_type).__name__, t)])
            if val.desc is not None:
                lines.extend(["  %s" % val.desc])
                lines.append('')
            if val.is_trait_type(Instance) or val.is_trait_type(Slot):
                lines.extend(["  * default:  %s"
                              % _get_instance_default(val.trait_type)]) 
            else:
                lines.extend(["  * default:  '%s'"
                              % (val.trait_type).default_value]) 
            if val.iotype is not None:
                lines.extend(["  * iotype:  '%s'" % val.iotype])
            if val.units is not None:
                lines.extend(["  * units: '%s'" % val.units])    
            if val.low is not None:
                if val.low == (-1 * maxint):
                    continue
                elif val.low == (-float_info.max):
                    continue
                else:
                    lines.extend(['  * low:  %s' % val.low])
            if val.high is not None:
                if val.high is maxint:
                    continue
                elif val.high is float_info.max:
                    continue
                else:
                    lines.extend(['  * high:  %s' % val.high])
    
            #now to put in the metadata added by users, or not specially handled.
            metadata = val.trait_type._metadata.items()
            for m, v in metadata:
                if m not in dontdo_meta:
                    if isinstance(v, basestring):
                        v = "'%s'" % v
                    lines.extend(['  *  %s:  %s' % (m, v)])
                    
            lines.append('')
            
def setup(app):
    """
    Connect the doctools to the process-docstring hook.
    """
    app.connect('autodoc-process-docstring', get_traits_info)

def _sortedDictVals(unsorted_dict):
    """
    Sort a dictionary into alphabetical order by keys.
    """
    items = unsorted_dict.items()
    items.sort()
    return items

if __name__ == '__main__': # pragma no cover
    from openmdao.lib.components.api import ExternalCode
    lines = []
    get_traits_info(None, 'class', 'foo', ExternalCode, None, lines)
    for line in lines:
        print line
