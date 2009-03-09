"""
Contains the Assembly class, which is a Component that contains
a Driver and a Workflow, and manages connections between its 
child Components.
"""

#public symbols
__all__ = ["Assembly"]

__version__ = "0.1"

from zope.interface import implements

from openmdao.main.interfaces import IAssembly
from openmdao.main.component import Component
from openmdao.main.variable import INPUT, OUTPUT
from openmdao.main.tarjan import strongly_connected_components



class Assembly(Component):
    """A container for computational elements, a driver, and a strategy"""
   
    implements(IAssembly)
    
    def __init__(self, name, parent=None, desc=None):
        super(Assembly, self).__init__(name, parent, desc)
        self._connections = {} # dependencies between Components
        self.driver = self.create('openmdao.main.driver.Driver','driver')
        self.workflow = self.create('openmdao.main.workflow.Workflow',
                                    'workflow')
    
    def _check_circular_deps(self, incomp, invar, outname):
        """Raises an exception if a circular dependency is detected"""
        dep_graph = {}
        for compname, deps in self._connections.items():
            outs = set()
            for outtuple in deps.values():
                outobj = self.get(outtuple[0])
                out = outobj.name
                outs.add(out)
                if out not in dep_graph:
                    dep_graph[out] = []
            dep_graph[compname] = list(outs)
        
        # now add the new dep
        out = outname.split('.')[0]
        
        if incomp == out:
            self.raise_exception('Cannot connect a component ('+
                                 incomp+') to itself', RuntimeError)
        
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
                self.raise_exception('Circular dependency would be '+
                                     'created by connecting '+
                                     incomp+'.'+invar+' to '+outname, 
                                     RuntimeError)

    def connect(self, outpath, inpath):
        """Connect one output variable to one input variable"""

        #outcomp and variable name
        outcompname, outvarname = outpath.split('.', 1)
        outcomp = getattr(self, outcompname)
        outvar = outcomp.getvar(outvarname)
        if outvar.iostatus != OUTPUT:
            self.raise_exception(outvar.get_pathname()+
                                 ' must be an OUTPUT variable',
                                 RuntimeError)
      
        #incomp and variable name
        incompname, invarname = inpath.split('.', 1)
        incomp = getattr(self, incompname)
        invar = incomp.getvar(invarname)
        
        if invar.iostatus != INPUT:
            self.raise_exception(invar.get_pathname()+
                                 ' must be an INPUT variable',
                                 RuntimeError)
        
        if incompname in self._connections:
            for iname in self._connections[incompname].keys():
                if iname == invarname:
                    self.raise_exception(inpath+' is already connected',
                                         RuntimeError)
        
        # test compatability
        invar.validate_var(outvar)
        
        self._check_circular_deps(incompname, invarname, outpath)
        
        if incompname not in self._connections:
            self._connections[incompname] = {}
            
        self._connections[incompname][inpath.split('.', 1)[1]] = (outcompname, 
                                                                 outvarname)
        
    def disconnect(self, inpath):
        """Remove a connection between two Components."""
        incompname, invarname = inpath.split('.', 1)
        
        if incompname not in self._connections or \
           invarname not in self._connections[incompname]:
            self.raise_exception(inpath+' is not connected', RuntimeError)
        del self._connections[incompname][invarname]
                    
        if len(self._connections[incompname]) == 0:
            del self._connections[incompname]
                    
    def execute(self):
        """run this Assembly by handing control to the driver"""
        return self.driver.run()

    def step(self):
        """Execute a single step."""
        self.driver.step()

    def remove_child(self, name):
        """Remove the named object from this container and notify any 
        observers.
        """
        
        # TODO: notify observers of removal...
        
        if name in self._connections:
            del self._connections[name]
            
        discons = []
        for incompname, indict in self._connections.items():
            for invarname, outtuple in indict.items():
                if outtuple[0] == name:
                    # disconnect any inputs reading from outputs 
                    # of this component
                    discons.append('.'.join([incompname, invarname]))

        for disc in discons:
            self.disconnect(disc)
            
        self.workflow.remove_node(getattr(self, name))           
        Component.remove_child(self, name)

    def list_connections(self, fullpath=False):
        """Return a list of tuples of the form (outvarname, invarname).
        """
        conns = []
        for comp, inputs in self._connections.items():
            for inp, outinfo in inputs.items():
                outcomp = self.getvar(outinfo[0])
                outvar = outcomp.getvar(outinfo[1])
                if fullpath is True:
                    inname = '.'.join([self.getvar(comp).get_pathname(), inp])
                    outname = outvar.get_pathname()
                else:
                    inname = '.'.join([comp, inp])
                    outname = '.'.join([outcomp.name, outvar.name])
                conns.append((outname, inname))
        return conns
    
    def update_inputs(self, incomp):
        """Transfer input data to the specified component"""
        try:
            deps = self._connections[incomp.name]
        except KeyError:
            return  # no connected inputs for this component

        for invarname, outtuple in deps.items():
            invar = incomp.getvar(invarname)
            outvar = self.getvar('.'.join(outtuple[:2]))
            invar.setvar(None, outvar)
            
