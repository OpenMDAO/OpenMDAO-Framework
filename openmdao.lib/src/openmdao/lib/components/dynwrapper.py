import os

from openmdao.main.api import Component
        
#class DynWrapper(Component):
    #"""A Component wrapper for objects that contain their own internal hierarchy
    #and Component-like interface (set/get/run) but do not provide normal python 
    #attribute access.
    #"""
    
    #def __init__(self):
        #super(DynWrapper, self).__init__()

    #def get_dyn_trait(self, pathname, io):
        #"""Returns a trait if a trait with the given pathname exists, possibly
        #creating the trait 'on-the-fly'. If an attribute exists with the given
        #pathname but no trait is found or can be created, or if pathname
        #references a trait in a parent scope, None will be returned. If no
        #attribute exists with the given pathname within this scope, an
        #AttributeError will be raised.
        
        #pathname: str
            #Pathname of the desired trait.  May contain dots.
        #"""
        #pass
        
    #def __getattr__(self, name):
        ## this is called when the normal getattr fails
        #pass

    #def _get_failed(self, path, index=None):
        #pass
            
    #def _set_failed(self, path, value, index=None, force=False):
        #pass
    
    #def execute(self):
        #""" Perform operations associated with running the component. """
        #pass
    #def build_trait(self, ref_name, iotype=None, trait=None):
        ## create appropriate trait based on the name, iotype, and whatever
        ## internal information that self._top can provide
        #pass
    
    #def get_wrapped_attr(self, name):
        #pass

