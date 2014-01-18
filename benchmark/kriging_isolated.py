# pylint: disable-msg=C0111,C0103

from time import time

import numpy as np
from numpy import pi, cos, sin

from openmdao.lib.surrogatemodels.kriging_surrogate import KrigingSurrogate

np.random.seed(12345)
N = 200

def bran(x):
    y = (x[1]-(5.1/(4.*pi**2.))*x[0]**2.+5.*x[0]/pi-6.)**2.+10.*(1.-1./(8.*pi))*cos(x[0])+10.
    return y

x = np.random.random((N, 2))*10.0
y = np.array([bran(case) for case in x])

krig1 = KrigingSurrogate()

t0 = time()
krig1.train(x, y)
print 'Training Time elapsed', time() - t0

xx = np.random.random((N, 2))*10.0
t0 = time()
for jj in range(len(xx)):
    pred = krig1.predict(xx[jj, :])
print 'predicting Time elapsed', time() - t0

