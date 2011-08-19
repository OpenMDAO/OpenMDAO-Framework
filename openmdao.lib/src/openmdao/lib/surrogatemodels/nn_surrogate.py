"""Surrogate Model based on an artificial neural network.
"""

import numpy as np
from numpy import append,array
from ffnet import ffnet,mlgraph


from openmdao.main.interfaces import implements, ISurrogate
from openmdao.lib.datatypes.api import Int
from enthought.traits.api import HasTraits

class NeuralNet(HasTraits):
    implements(ISurrogate)
    
    n_hidden_nodes = Int(4, iotype='in', desc = 'Number of hidden nodes in hidden layer of network')
    
    def __init__(self, n_hidden_nodes=4):
        """Initializes neural net surrogate model.
        
            n_hidden_nodes: int
                number of hidden nodes
        """
        self.n_hidden_nodes = n_hidden_nodes
    def get_uncertain_value(self, value):
        """Returns the value iself. Neural network can provide its own uncertainty. """
        return value
    
    def train(self, X, Y):
        """ Trains the nerual network based on the given set of inputs
        and outputs. """

        inp = array(X)
        targ = array(Y)
        n_inputs = len(inp[0])
        
        # 1 Output node because Surrogate Model has only 1 output
        self._nn_surr = ffnet(mlgraph((n_inputs, self.n_hidden_nodes, 1)))
                        
        # Start the training
        #self._nn_surr.train_genetic(inp, targ, individuals=10*n_inputs, generations=500)

        #self._nn_surr.train_tnc(inp, targ,maxfun=5000)
        
        #self._nn_surr.train_momentum(inp,targ,momentum=1)
        #self._nn_surr.train_rprop(inp,targ)
        self._nn_surr.train_cg(inp,targ,disp=False)
        #self._nn_surr.train_bfgs(inp,targ)
                
    def predict(self, X):
        """ Calculates a predicted value of the response based on the weights
         determined by the current neural network. """
        
        output = self._nn_surr(X)
        return output[0]
  
if __name__ =="__main__":     
    import numpy as np    
    x = np.linspace(0, 5, 25)
    y = np.sin(x) * 4
    
    size = len(x)
    
    inp = x.reshape(size,1)
    
    nn = NeuralNet()
    nn.n_hidden_nodes = 5
    
    nn.train(inp,y)
    
    print nn.predict(inp)