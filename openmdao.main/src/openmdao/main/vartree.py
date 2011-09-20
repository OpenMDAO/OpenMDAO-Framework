
from openmdao.main.container import Container


class VariableTree(Container):
    
    def __init__(self, iotype=None, doc=None):
        super(VariableTree, self).__init__(doc=doc)
        self.iotype = iotype
                        
    #def add(self, name, obj):
        #if isinstance(obj, TraitType):
            #self._vars[name] = obj
            #setattr(self, name, copy.deepcopy(obj.default_value))
        #elif isinstance(obj, VarTreeData):
            #setattr(self, name, obj)
        #else:
            #raise ValueError(self._add_path(
                #"you can only add Variables or other VarTreeDatas to a VarTreeData"))

    #def remove(self, name):
        #if name in self._vars:
            #del self._vars[name]
        #if hasattr(self, name):
            #delattr(self, name)
        #else:
            #raise AttributeError(self._add_path("cannot remove '%s': not found" % name))

    #def contains(self, path):
        #obj = self
        #try:
            #parts = path.split('.')
            #for name in parts[:-1]:
                #obj = getattr(obj, name)
        #except Exception:
            #return False
        #return True
    
    #def get_metadata(self, varpath, metaname=None):
        #"""Retrieve the metadata associated with the variable found using
        #varpath.  If metaname is None, return the entire metadata dictionary
        #for the specified variable. Otherwise, just return the specified piece
        #of metadata.  If the specified piece of metadata is not part of
        #the variable, None is returned.
        #"""
        #childname, _, restofpath = varpath.partition('.')
        #if restofpath:
            #obj = getattr(self, childname, _missing)
            #return obj.get_metadata(restofpath, metaname)
            
        #v = self._vars.get(varpath)
        #if not v:
            #raise AttributeError(self._add_path(
                #"Couldn't find metadata for variable %s" % varpath))
        #if metaname is None:
            #mdict = v._metadata.copy()
            #mdict.setdefault('vartypename', v.__class__.__name__)
            #return mdict
        #else:
            #val = v._metadata.get(metaname, None)
            ## vartypename isn't present in the metadata of traits
            ## that don't inherit from Variable, so fake it out here
            ## so we'll be consistent across all traits
            #if val is None:
                #if metaname == 'vartypename':
                    #return v.__class__.__name__
            #return val
        
    #def get(self, path, index=None):
        #childname, _, restofpath = path.partition('.')
        #if restofpath:
            #try:
                #obj = getattr(self, childname)
            #except AttributeError as err:
                #raise AttributeError(self._add_path(str(err)))
            #return obj.get(restofpath, index)
        #else:
            #try:
                #obj = getattr(self, path)
            #except AttributeError as err:
                #raise AttributeError(self._add_path(str(err)))
            #if index is None:
                #return obj
            #else:
                #for idx in index:
                    #obj = process_index_entry(obj, idx)
                #return obj
    
    #def set(self, path, value, index=None, src=None, force=False):
        #childname, _, restofpath = path.partition('.')
        #if restofpath:
            #obj = getattr(self, childname, Missing)
            #if src is not None:
                #src = 'parent.'+src
            #obj.set(restofpath, value, index, src=src, force=force)
        #else:
            #iotype = self.get_metadata(path, 'iotype')
            #if iotype == 'in': # setting an input, so have to check source
                #if not force:
                    #self._check_source(path, src)
                #if index is None:
                    ## bypass input source checking (we already checked it)
                    #chk = self._input_check
                    #self._input_check = self._input_nocheck
                    #try:
                        #setattr(self, path, value)
                    #finally:
                        #self._input_check = chk
                #else:  # array index specified
                    #index_set(self, path, value, index, 'in')
            #elif index:  # array index specified for output
                #index_set(self, path, value, index, 'out')
            #else: # output
                #setattr(self, path, value)
    
    #def get_pathname(self):
        #if self._parent is not None:
            #return '.'.join([self._parent.get_pathname(), self._name])
        #else:
            #return self._name

    #def get_dyn_trait(self, pathname, iotype=None, trait=None):
        #if pathname.startswith('parent.'):
            #return None
        #cname, _, restofpath = pathname.partition('.')
        #if restofpath:
            #child = getattr(self, cname)
            #if has_interface(child, IContainer):
                #return child.get_dyn_trait(restofpath, iotype, trait)
            #else:
                #if deep_hasattr(child, restofpath):
                    #return None
        #else:
            #trait = self._vars.get(cname)
            #if trait is not None:
                #if iotype is not None and self._iotype != iotype:
                    #raise RuntimeError(self._add_path("iotype of %s must be '%s'" % 
                                         #(pathname, iotype)))
                #return trait
            #elif trait is None and self.contains(cname):
                #return None

        #raise AttributeError(self._add_path(
              #"Cannot locate variable named '%s'" % pathname))
    
    #def get_wrapped_attr(self, name):
        #"""If the variable can return an AttrWrapper, then this
        #function will return that, with the value set to the current value of
        #the variable. Otherwise, it functions like *getattr*, just
        #returning the value of the variable. Raises an exception if the
        #variable cannot be found. The value will be copied if the variable has
        #a 'copy' metadata attribute that is not None. Possible values for
        #'copy' are 'shallow' and 'deep'.
        #"""
        #scopename, _, restofpath = name.partition('.')
        #if restofpath:
            #if scopename == 'parent':
                #return self.parent.get_wrapped_attr(name[7:])
            #obj = getattr(self, scopename)
            #if has_interface(obj, IContainer):
                #return obj.get_wrapped_attr(restofpath)
            #else:
                #return getattr(obj, restofpath)
        
        #trait = self.get_trait(name)
        #if trait is None:
            #raise AttributeError(self._add_path("trait '%s' does not exist" %
                                                #name))
            
        ## trait itself is most likely a CTrait, which doesn't have
        ## access to member functions on the original trait, aside
        ## from validate and one or two others, so we need to get access 
        ## to the original trait which is held in the 'trait_type' attribute.
        #ttype = trait.trait_type
        #getwrapper = ttype.get_val_wrapper
        #val = getattr(self, name)
        ## copy value if 'copy' found in metadata
        #if ttype.copy is 'deep':
            #val = copy.deepcopy(val)
        #elif ttype.copy is 'shallow':
            #val = copy.copy(val)
        #if getwrapper is not None:
            #return getwrapper(val)
        
        #return val
    
    #def list_vars(self):
        #return self._vars.keys()

    #def _add_path(self, msg):
        #"""Adds our pathname to the beginning of the given message"""
        #return "%s: %s" % (self.get_pathname(), msg)

    #def _check_source(self, path, src):
        #"""Raise an exception if the given source variable is not the one
        #that is connected to the destination variable specified by 'path'.
        #"""
        #source = self._depgraph.get_source(path)
        #if source is not None and src != source:
            #self.raise_exception(
                #"'%s' is connected to source '%s' and cannot be "
                #"set by source '%s'" %
                #(path,source,src), RuntimeError)

    #def __setattr__(self, name, value):
        #var = self._vars.get(name)
        #if var:
            #value = var.validate(self, name, value)
            #old = getattr(self, name, _missing)
            #objsetattr(self, name, value)
            ##if isinstance(value, VarTreeData) and value is not old:
                ##_update_vt_attrs(value, self, self._iotype, name)
            #if self._iotype == 'in':
                #if value != old:
                    #self._input_modified(name)
        #else:
            #objsetattr(self, name, value)
            #if isinstance(value, VarTreeData):
                #_update_vt_attrs(value, self, self._iotype, name)

    #def _input_modified(self, name):
        #"""Notify our Container parent that we've been modified"""
        #old = None
        #p = self._parent
        #while isinstance(p, VarTreeData):
            #old = p
            #p = p._parent
        #name = old._name if old else name
        #if p:
            #p._input_trait_modified(p, name, old, old)
        
    #def __getstate__(self):
        #state = self.__dict__.copy()
        #state['_parent'] = None
        #return state
        
#def _update_vt_attrs(vt, parent, iotype, name, visited=None):
    #from openmdao.main.component import Component
    
    #if visited is None:
        #visited = set([vt, parent])
    #objsetattr(vt, '_iotype', iotype)
    #objsetattr(vt, '_parent', parent)
    #objsetattr(vt, '_name', name)
    #for k,v in vt.__dict__.items():
        #if isinstance(v, VarTreeData) and v not in visited:
            #visited.add(v)
            #_update_vt_attrs(v, vt, iotype, k, visited)
    
#def vartree_from_dict(dct):
    #"""Returns a VarTreeData built using a possibly nested dict 
    #containing Variables
    #"""
    #vt = VarTreeData()
    #for k,v in dct.items():
        #if isinstance(v, dict):
            #vt.add(k, vartree_from_dict(v))
        #else:
            #vt.add(k, copy.deepcopy(v))
    #return vt
    