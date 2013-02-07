from openmdao.main.api import Component


class LazyComponent(Component): 
    """
    Base Component Class for situations where you want your component to calculate 
    only the output values that are connected to something else in the model. This 
    behavior makes the component 'lazy' since some of it's outputs won't be valid 
    even though it has executed. 

    The component provides an attribute which can be used in the 'execute' method
    called '_connected_outputs' which lists all the outputs that are connected to something
    in your model. You **MUST** provide values for all the outputs in that list, or an error 
    will be raised. You need not calculate any outputs that are not in that list. 

    Note that there is some extra framework overhead associated with this base class. So you 
    should only use it in the case where you have outputs that are computationally expensive 
    and you wish to only calculate them when they are relevant to the current simulation. 
    """
    
    def _anytrait_changed(self, name, old, new): 
        #note: could filter for only outputs here, but it's not necessary, so just 
        # adds extra overhead
        try: 
            self._updated_traits[name] = new
        except AttributeError:
            self._updated_traits = {}
            self._updated_traits[name] = new

        try: 
            self._old_traits_vals[name] = new
        except AttributeError: 
            self._old_traits_vals = {}
            self._old_traits_vals[name] = new

    def _pre_execute(self, force=False): 
        super(LazyComponent, self)._pre_execute()
        for k in self._updated_traits.iterkeys(): 
            self._updated_traits[k] = None
        self._connected_outputs = self.list_outputs(connected=True)
        
    def _post_execute(self): #probably not the most efficient, but it works
        super(LazyComponent, self)._post_execute()

        #check to make sure all the necessary outputs were calculated
        for name in self._connected_outputs: 
            try: 
                self._updated_traits[name]
            except KeyError: 
                self.raise_exception("output '%s' is connected to something in "
                    "your model, but was not calculated during execution"%name, RuntimeError)

        # make our output Variables valid only if they were actually changed
        valids = self._valid_dict
        for name in self.list_outputs(): 
            updated = name in self._updated_traits
            if updated and self._updated_traits[name]!=None:
                valids[name] = True
            #updated, conncected, but unchanged so the changed trigger never fired
            elif updated and (self._old_traits_vals[name]==self.get(name)) and (name in self._connected_outputs): 
                valids[name] = True        
            else:
                valids[name] = False    
