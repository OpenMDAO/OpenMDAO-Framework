from pybrain.structure import FeedForwardNetwork
from pybrain.structure import LinearLayer, SigmoidLayer
from pybrain.structure import FullConnection

from pylab import ion, ioff, figure, draw, contourf, clf, show, hold, plot
from scipy import diag, arange, meshgrid, where
from numpy.random import multivariate_normal

nn_surr = FeedForwardNetwork()

inLayer = LinearLayer(2)
hiddenLayer = SigmoidLayer(3)
outLayer = LinearLayer(1)

nn_surr.addInputModule(inLayer)
nn_surr.addModule(hiddenLayer)
nn_surr.addOutputModule(outLayer)

in_to_hidden = FullConnection(inLayer, hiddenLayer)
hidden_to_out = FullConnection(hiddenLayer, outLayer)

nn_surr.addConnection(in_to_hidden)
nn_surr.addConnection(hidden_to_out)

nn_surr.sortModules()