import numpy as np
from openmdao.lib.components.api import MetaModel
from openmdao.main.interfaces import IMultiFiSurrogate
from openmdao.main.datatypes.api import List, Float, Slot


class MultiFiMetaModel(MetaModel):
    """ Class that generalizes the MetaModel class to be able to train surrogates
    with multi-fidelity training inputs. For a given number of levels of fidelity
    **nfi** (given at initialization) the corresponding input variables 
    *<invar>_fi<2..nfi>* and *<outvar>_fi<2..nfi>* are automatically created 
    besides the given *<invar>* and *<outvar>* variables. Note the index starts at 2,
    the index 1 is omitted considering the simple name *<var>* is equivalent
    to *<var>_fi1* which is intended to be the data of highest fidelity.
        
    The surrogate models are trained with a list of (m samples, n inputs) 
    lists of list built from the various training input data. By convention,
    the fidelities are intended to be ordered from highest to lowest fidelity. 
    Obviously for a given level of fidelity corresponding lists 
    *<var>_fi<n>* have to be of the same size.
    
    Thus given the initialization:
    MultiFiMetaModel(params=(x1, x2,), responses=(y1, y2,), nfi=2) the following 
    supplementary input variables x1_fi2 and x2_fi2 are created together with the classic 
    ones x1 and x2 and the output variables y1_fi2 and y2_fi2 are created as well.
    The embedded surrogate for y1 will be trained with a couple (X, Y) 
    
    * where X is the list [X_fi1, X_fi2] where X is an (m1, 2) list of list \
    filled with the m1 samples [x1 value, x2 value], X_fi2 is an (m2, 2) list of \
    list filled with the m2 samples [x1_fi2 value, x2_fi2 value]     
   
    * where Y is a list [Y1_fi1, Y1_fi2] where Y1_fi1 is a list of m1 y1 values \
    and Y1_fi2 a list of m2 y1_fi2 values. 
    
    Note: when **nfi** ==1 a :class:`MultiFiMetaModel` object behaves exactly like 
    a :class:`MetaModel` object.
    """
        
    def __init__(self, params=None, responses=None, nfi=1):
        super(MultiFiMetaModel, self).__init__(params, responses)
        
        self.nfi = nfi
        
        if self.nfi > 1:
            self._param_data = [[] for i in np.arange(self.nfi)]
            for name in responses:
                self._response_data[name] = [[] for i in np.arange(self.nfi)]
            
            self.add('default_surrogate', Slot(IMultiFiSurrogate, allow_none=True,
                     desc="This surrogate will be used for all outputs that don't "
                     "have a specific surrogate assigned to them in their sur_<name> slot.")) 

        # Add params.<invar>_fi<n>
        for name in params:
            for n in np.arange(nfi):
                if n > 0:
                    input_tree = self.get('params')
                    name_with_fi = "%s_fi%d" % (name, n+1)
                    self.add(name_with_fi, Float(0.0, iotype='in', desc='metamodel param'))
                    input_tree.add(name_with_fi, List([], desc='training param'))

        # Add responses.<outvar>_fi<n>
        for name in responses:
            for n in np.arange(nfi):
                if n > 0:
                    output_tree = self.get('responses')
                    name_with_fi = "%s_fi%d" % (name, n+1)
                    self.add(name_with_fi, Float(0.0, iotype='out', desc='metamodel response'))
                    output_tree.add(name_with_fi, List([], desc='training response'))
                    self.surrogates[name] = None

    def execute(self):
        """If the training flag is set, train the metamodel. Otherwise,
        predict outputs. If **nfi** is equal to 1, it behaves as a :class:`MetaModel`.
        """

        if self.nfi < 2:
            # shortcut: fallback to base class behaviour immediatly
            super(MultiFiMetaModel, self).execute()
            return

        # Train first
        if self._train:

            input_data = self._param_data
            
            if (self.warm_restart is False):
                input_data = [[] for i in np.arange(self.nfi)]
                
            for name in self._surrogate_input_names:
                for fi_index in np.arange(self.nfi):
                    if self.warm_restart is False:
                        base = 0
                    else:
                        base = len(input_data[fi_index])
                
                    if fi_index == 0:
                        train_name = "params.%s" % name
                    else:
                        train_name = "params.%s_fi%d" % (name, fi_index+1)
                        
                    val = self.get(train_name)
                    num_sample = len(val)
    
                    for j in xrange(base, base + num_sample):
                        if j > len(input_data[fi_index]) - 1:
                            input_data[fi_index].append([])
                        input_data[fi_index][j].append(val[j-base])
                    
            for name in self._surrogate_output_names:
                if self.warm_restart is False:
                    output_data = [[] for i in np.arange(self.nfi)]
                else:
                    output_data = self._response_data[name]
                
                for fi_index in np.arange(self.nfi):    
                    if fi_index == 0:
                        train_name = "responses.%s" % name
                    else:
                        train_name = "responses.%s_fi%d" % (name, fi_index+1)
                                      
                    output_data[fi_index].extend(self.get(train_name))
                    
                surrogate = self._get_surrogate(name)
    
                if (surrogate is not None):
                    surrogate.train_multifi(input_data, output_data)
        
            self._train = False

        # Now Predict for current inputs
        super(MultiFiMetaModel, self).execute()
