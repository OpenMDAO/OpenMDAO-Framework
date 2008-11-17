
#public symbols
__all__ = ["Assembly"]

__version__ = "0.1"

import weakref

from zope.interface import implements

from openmdao.main.interfaces import IComponent, IVariable, IAssembly
from openmdao.main.component import Component
from openmdao.main.variable import Variable, INPUT, OUTPUT
from openmdao.main.tarjan import strongly_connected_components



class Assembly(Component):
    """A container for computational elements, a driver, and a strategy"""
   
    implements(IAssembly)
    
    def __init__(self, name, parent=None, desc=None):
        Component.__init__(self, name, parent, desc)
        self._connections = {} # dependencies between Components
        self.driver = self.create('openmdao.main.driver.Driver','driver')
        self.workflow = self.create('openmdao.main.workflow.Workflow','workflow')
    
        
    def _check_circular_deps(self, incomp, invar, outname):
        """Raises an exception if a circular dependency is detected"""
        dep_graph = {}
        for compname,deps in self._connections.items():
            outs = set()
            for outtuple in deps.values():
                outobj = outtuple[0]()
                out = outobj._parent.name
                outs.add(out)
                if out not in dep_graph:
                    dep_graph[out] = []
            dep_graph[compname] = list(outs)
        
        # now add the new dep
        out = outname.split('.')[0]
        
        if incomp == out:
            raise RuntimeError('Cannot connect a component ('+incomp+
                               ') to itself')
        
        if incomp not in dep_graph:
            dep_graph[incomp] = [out]
        else:
            if outname not in dep_graph[incomp]:
                dep_graph[incomp].append(out)
        if out not in dep_graph:
            dep_graph[out] = []

        sccomps = strongly_connected_components(dep_graph)
        for comp in sccomps:
            if len(comp) > 1:
                raise RuntimeError('Circular dependency would be '+
                                   'created by connecting '+
                                   incomp+'.'+invar+' to '+outname)

    def connect(self, outpath, inpath):
        """Connect one output variable to one input variable"""

        #outcomp and variable name
        outcompname, outvarname = outpath.split('.',1)
        outcomp = self.get(outcompname)
        attrname = None
        if '.' in outvarname: # it's an attribute access
            outvarname, attrname = outvarname.split('.',1)
        if attrname == 'value':
            attrname = None
        outvar = outcomp.get(outvarname)
        weakovar = weakref.ref(outvar)
        if outvar.iostatus != OUTPUT:
            raise RuntimeError(outvar.get_pathname()+' must be an OUTPUT variable')
      
        #incomp and variable name
        incompname, invarname = inpath.split('.',1)
        incomp = self.get(incompname)
        if '.' in invarname:
            raise RuntimeError('direct linking to attributes within an INPUT'+
                               ' Variable is illegal')
        invar = incomp.get(invarname)
        weakivar = weakref.ref(invar)
        
        if invar.iostatus != INPUT:
            raise RuntimeError(invar.get_pathname()+' must be an INPUT variable')
        
        if incompname in self._connections:
            for iname in self._connections[incompname].keys():
                if iname == invarname:
                    raise RuntimeError(inpath+' is already connected')
        
        # test compatability
        invar.connect(outvar, attrname)
        
        self._check_circular_deps(incompname, invarname, outpath)
        
        if incompname not in self._connections:
            self._connections[incompname] = {}
            
        self._connections[incompname][weakivar] = (weakovar, attrname)
        
      
    def disconnect(self, inpath):
        incomp, invar = inpath.split('.',1)
        removes = []
        infound = False
        
        for weakin,outtuple in self._connections[incomp].items():
            inobj = weakin()
            outobj = outtuple[0]()
            if inobj is None or outobj is None:
                removes.append(weakin)
                
            if inobj is not None and inobj.name == invar:
                infound = True
                if weakin not in removes:
                    removes.append(weakin)

        # clean up connection and any old dangling weakrefs
        for rem in removes:  
            del self._connections[incomp][rem]
            
        if infound is False:
            raise RuntimeError(inpath+' is not connected')
            
        if len(self._connections[incomp]) == 0:
            del self._connections[incomp]
                    

    def execute(self):
        """run this Assembly by handing control to the driver"""
        self.driver.run()
              
    def remove_child(self, name):
        """Remove the named object from this container and notify any 
        observers.
        
        """
        if name in self._connections:
            del self._connections[name]           
        Component.remove_child(self, name)
      
    def update_inputs(self, incomp):
        """Transfer input data to the specified component"""
        removes = []
        try:
            deps = self._connections[incomp.name]
        except KeyError:
            return
        for weakin, outtuple in deps.items():
            invar = weakin()
            outvar = outtuple[0]()
            attrname = outtuple[1]
            if invar is not None and outvar is not None:
                if attrname is not None:
                    invar.set(None, outvar.get(attrname))
                else:
                    invar.set(None, outvar)
            else:
                removes.append(weakin)
        # clean up any danglers
        for rem in removes:  
            del deps[rem]
            
      
    def list_connections(self, fullpath=False):
        conns = []
        for comp,inputs in self._connections.items():
            for inp,outinfo in inputs.items():
                ii = inp()
                oo = outinfo[0]()
                attr = outinfo[1]
                if ii is not None and oo is not None:
                    if fullpath is True:
                        inname = '.'.join([self.get(comp).get_pathname(), ii.name])
                        if attr is None:
                            outname = oo.get_pathname()
                        else:
                            outname = '.'.join([oo.get_pathname(), attr])
                    else:
                        inname = '.'.join([comp, ii.name])
                        if attr is None:
                            outname = '.'.join([oo._parent.name, oo.name])
                        else:
                            outname = '.'.join([oo._parent.name, oo.name, attr])
                    conns.append((outname, inname))
        return conns
    
    
    
