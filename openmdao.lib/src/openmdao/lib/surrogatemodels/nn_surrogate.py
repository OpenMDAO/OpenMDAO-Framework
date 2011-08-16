import numpy as np
from numpy import append,array
from pybrain.tools.shortcuts import buildNetwork
from pybrain.supervised.trainers import BackpropTrainer
from pybrain.datasets import SupervisedDataSet

from ffnet import ffnet,mlgraph


from openmdao.main.interfaces import implements, ISurrogate

class NeuralNet(object):
    implements(ISurrogate)
    
    def __init__(self, n_hidden_nodes=2):
        """Initializes neural net surrogate model.
        
            n_hidden_nodes: int
                number of hidden nodes
        """
        self.n_hidden_nodes = n_hidden_nodes
    def get_uncertain_value(self, value):
        return value
    
    def train(self, X, Y):
        inp = array(X)
        self.targ = array(Y)
        n_inputs = len(inp[0])
        
        # 1 Output node because Surrogate Model has only 1 output
        self._nn_surr = ffnet(mlgraph((n_inputs, self.n_hidden_nodes, 1)))
                        
        # Start the training
        self._nn_surr.train_momentum(inp, self.targ, momentum = .1)
                
    def predict(self, X):
        
        output, regression = self._nn_surr.test(X, self.targ, iprint = 2)
        return output,regression
  
if __name__ =="__main__":     
    import numpy as np    
    x = np.linspace(0, 5, 25)
    y = np.sin(x) * 4
    
    size = len(x)
    
    inp = x.reshape(size,1)
    
    nn = NeuralNet()
    nn.n_hidden_nodes = 5
    
    nn.train(inp,y)
    
    print nn.predict(x)