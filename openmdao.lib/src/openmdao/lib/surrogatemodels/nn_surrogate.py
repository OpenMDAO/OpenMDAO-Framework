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
    
    def train(self, x, y):
        n_inputs = len(x[0])
        # 1 Output node because Surrogate Model has only 1 output
        self._nn_surr = buildNetwork(n_inputs, self.n_hidden_nodes, 1)
                
        #Scaling of exponents down to between .1 and .9
        """x_min = np.amin(x, axis=0)
        x_max = np.amax(x, axis=0)
        y_min = np.amin(y, axis=0)
        y_max = np.amax(y, axis=0)
        
        m_x = .8/(x_max-x_min)
        m_y = .8/(y_max-y_min)
        b_x = .1-(.8*x_min)/(x_max-x_min)
        b_y = .1-(.8*y_min)/(y_max-y_min)
        
        x_scaled = m_x*x+b_x
        y_scaled = m_y*y+b_y
        """
        x_scaled = x
        y_scaled = y
        
        # Creating the Dataset
        ds = SupervisedDataSet(n_inputs,1)
        for inp,target in zip(x_scaled,y_scaled):
            ds.addSample(inp,(target,))
            
        # Set the type of trainer
        trainer = BackpropTrainer(self._nn_surr, ds, momentum = .1)
        # Start the training
        trainer.trainUntilConvergence()
        
    def predict(self, x):
        return self._nn_surr.activate(x)[0]
    
    
if __name__ =="__main__":     
    import numpy as np    
    x = np.linspace(1, 10, 25)
    y = np.sin(x) * 0.5
    
    size = len(x)
    
    inp = x.reshape(size,1)
    
    nn = NeuralNet()
    nn.n_hidden_nodes = 5
    
    nn.train(inp,y)
    
    for x,tar in zip(inp,y):    
        print nn.predict(x),tar
        
    

    

    


