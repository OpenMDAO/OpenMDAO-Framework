from pybrain.tools.shortcuts import buildNetwork
from pybrain.supervised.trainers import BackpropTrainer
from pybrain.datasets import SupervisedDataSet

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
        n_inputs = len(X[0])
        # 1 Output node because Surrogate Model has only 1 output
        self._nn_surr = buildNetwork(n_inputs, self.n_hidden_nodes, 1)
        
        ds = SupervisedDataSet(n_inputs,1)
        
        for inp,target in zip(X,Y):
            ds.addSample(inp,(target,))
               
        trainer = BackpropTrainer(self._nn_surr, ds, momentum = .1)
        
        trainer.trainUntilConvergence()
        
    def predict(self, x):
        return self._nn_surr.activate(x)[0]
    
    
if __name__ =="__main__":     
    import numpy as np    
    x = np.linspace(0, 1, 25)
    y = np.sin(x) * 0.5
    
    size = len(x)
    
    inp = x.reshape(size,1)
    
    nn = NeuralNet()
    nn.n_hidden_nodes = 5
    
    nn.train(inp,y)
    
    for x,tar in zip(inp,y):
        print nn.predict(x),tar
        
    

    

    


