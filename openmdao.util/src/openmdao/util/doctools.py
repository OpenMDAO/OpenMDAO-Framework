"""
A utility to extract Traits information from the code and get it into the Sphinx documentation.
"""

from enthought.traits.api import HasTraits, MetaHasTraits, Any, Python, Event
from enthought.traits.trait_base import not_none
from inspect import getmro
from sys import maxint, float_info
from enthought.traits.api import Instance
	    
excludes = (Any, Python, Event, type)

def get_traits_info(app, what, name, obj, options, lines):
    """
    Gets traits info.
    """
    if not (isinstance(obj, MetaHasTraits) or isinstance(obj, HasTraits)):
        return
    
    #gets a list of the class heirarchy.
    classes = getmro(obj)
    
    #gets a dict of all traits in this class, including those from base classes  
    this_class_traits = obj.class_traits()
    
    #gets a dict of all traits in this class' base class
    base_class_traits = classes[1].class_traits()
    
    #The things we want to keep will be those that are
    #unique to the current class, and those that appear in the
    #base but are overridden by the current class.  So keep each
    #item in this class' list that doesn't appear in the 
    #base classes' lists.
    keepers = {}
   
    for trt, trt_val in this_class_traits.items():
        if not (base_class_traits.has_key(trt)):
	  keepers[trt] = trt_val	
	else:
	   #the names are the same, so check the objects
	   #to see if they are the same.  if they aren't,
	   # there's an override in this class; keep it!
	   if not (trt_val == base_class_traits[trt]):
	       keepers[trt] = trt_val
    
   
    keepers_in={}
    keepers_out={}
    keepers_instance={}
    keepers_undefined={}
    
    #Now we need to SORT the traits by input/output type. 	  
    for t,val in keepers.items():
        #As long as it's not an excluded type, add it.
        if not isinstance(val.trait_type, excludes):
	    if (val.trait_type._metadata.has_key("iotype")):
	    	if (val.trait_type._metadata["iotype"] =="in"):
		   keepers_in[t]=val
	    	elif  (val.trait_type._metadata["iotype"] == "out"):
		   keepers_out[t]=val
	    elif (type(val.trait_type).__name__ == "Instance"):
		keepers_instance[t]=val	
	    else:
		keepers_undefined[t]=val
	    
    dicts = (keepers_instance, keepers_in, keepers_out, keepers_undefined)
    
    for dic in dicts:
	sortedDict = sortedDictVals(dic)
	
	for t, val in sortedDict:
	    lines.append('')
	    #Now just need to spit out the traits in the proper format into the documentation 
	    if (val.is_trait_type(Instance)):
	        lines.extend(["*%s* (%s) **%s**" %(type(val.trait_type).__name__, val.trait_type.klass.__name__, t)])
	    else:
	        lines.extend(["*%s* **%s**" %(type(val.trait_type).__name__, t)])	
	    lines.extend(["  %s" %val.desc])
	    lines.append('')
	    if (val.iotype is not None):
	        lines.extend(["  * iotype:  '%s'" %val.iotype])
	       
	    if (val.units is not None):
	        lines.extend(["  * units: '%s'" %val.units]) 
	           
	    if (val.low is not None):
	    	if (val.low == (-1 * maxint)):
		    continue
		elif (val.low == (-float_info.max)):
		    continue
		else:
		    lines.extend(['  * low:  %s' %val.low])
	    
	    if (val.high is not None):
	    	if val.high is maxint:
	            continue
		elif val.high is float_info.max:
		    continue
		else:
		    lines.extend(['  * high:  %s' %val.high])
	    
	    #now to put in the metadata added by users, or not specially handled.
	    dontdo=('iotype', 'units', 'low', 'high', 'type', 'desc', 'instance_handler', 'parent', 'array')
	    metadata = val.trait_type._metadata.items()
	    for m, v in metadata:
	    	if m not in dontdo:
		    if isinstance(v, basestring):
		    	v = "'%s'" %v  
		    lines.extend(['  *  %s:  %s' %(m, v)])	     	
	    
	    lines.append('')
	    

def setup(app):
    app.connect('autodoc-process-docstring', get_traits_info)

def sortedDictVals(unsorted_dict):
  """
  Sort a dictionary into alphabetical order by keys.
  """
  items = unsorted_dict.items()
  items.sort()
  return items

if __name__ == '__main__':
    #from openmdao.main.api import Assembly
    from openmdao.lib.api import ExternalCode
    lines = []
    #get_traits_info(None, 'class', 'foo', Assembly, None, lines)
    get_traits_info(None, 'class', 'foo', ExternalCode, None, lines)
    #print 'lines = %s', lines
    for line in lines:
    	print line
