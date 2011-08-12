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
                
        #Scaling of inputs down to between .1 and .9, and outputs between -.9 and .9
        self.M_in=[]
        self.B_in=[]
        s_in=[]
        for row in X.T:
            in_min = np.min(row)
            in_max = np.max(row)
            m_in = .8/(in_max-in_min)
            b_in = .1-.8/(in_max-in_min)*in_min
            self.M_in.append(m_in)
            self.B_in.append(b_in)
        for m,b,row in zip(self.M_in,self.B_in,X.T):
            s_row = m*row+b
            s_in.append(s_row)
            
        S_in = s_in.T
            
        self.out_min = np.min(Y)
        self.out_max = np.max(Y)
        self.m_out = 1.8/(self.out_max-self.out_min)
        self.b_out = -.9-(1.8/(self.out_max-self.out_min))*self.out_min
        S_out = self.m_out*Y+self.b_out
        
        # Creating the Dataset
        ds = SupervisedDataSet(n_inputs,1)
        for inp,target in zip(S_in,S_out):
            ds.addSample(inp,(target,))
            
        # Set the type of trainer
        trainer = BackpropTrainer(self._nn_surr, ds, momentum = .1)
        # Start the training
        trainer.trainUntilConvergence()
        
    def predict(self, x):
        
        S_in_p=[]
        for m,b,row in zip(self.M_in,self.B_in,X.T):
            s_row = m*row+b
            self.S_in_p.append(s_row)
                        
        out=self._nn_surr.activate(S_in_p)
        
        return self.m_out*out+self.b_out
    
if __name__ =="__main__":     
    import numpy as np    
    x = np.linspace(1, 2, 25)
    y = np.sin(x) * 0.5
    
    size = len(x)
    
    inp = x.reshape(size,1)
    
    nn = NeuralNet()
    nn.n_hidden_nodes = 5
    
    nn.train(inp,y)
    
    for x,tar in zip(inp,y):    
        print nn.predict(x),tar