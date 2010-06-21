"""
Math related utility functions used by OpenMDAO.
"""

from __future__ import division

from random import shuffle

from numpy import zeros

def rand_latin_hypercube(n, k, edges=False):
    """Calculates a random latin hypercube set of n points in k 
    dimensions within [0,1]^k hypercube.
    n - desired number of points
    k - number of design variables (dimensions)
    edges - if Edges=True the extreme bins will have their centres on the
            edges of the domain, otherwise the bins will be entirely 
            contained within the domain (default setting).
    Returns an n by k numpy array.
    """
    #generate nxk array of random numbers from the list of range(n) choices
    X = zeros((n, k))
    row = range(1, n+1)
    for i in range(k):
        shuffle(row)
        X[:,i] = row
    if edges:
        X = (X-1.0)/float((n-1))
    else: 
        X = (X-.5)/float(n)
    return X

