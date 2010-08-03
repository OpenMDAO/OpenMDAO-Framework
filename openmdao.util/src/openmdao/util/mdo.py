"""
Math related utility functions used by OpenMDAO.
"""

from __future__ import division

from random import shuffle

from numpy import zeros, array

def rand_latin_hypercube(n, k, edges=False):
    """
    Calculates a random latin hypercube set of n points in k 
    dimensions within [0,1]^k hypercube.
    
    n : int
       Desired number of points.
    k : int
       Number of design variables (dimensions).
    edges : bool, optional
       if Edges=True, the extreme bins will have their centres on the
       edges of the domain; otherwise the bins will be entirely 
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
        return (X-1.0)/float((n-1))
 
    return (X-.5)/float(n)


def is_latin_hypercube(lh):
    """Returns True if the given array is a latin hypercube.
    The given array is assumed to be a numpy array.
    """
    n,k = lh.shape
    for j in range(k):
        colset = set()
        col = lh[:,j]
        colset.update(col)
        if len(colset) < len(col):
            return False  # something was duplicated
    return True


if __name__ == '__main__':
    lh1 = array([[1,2,3],[3,1,2],[2,3,1]])
    assert(is_latin_hypercube(lh1))
    badlh = array([[1,2,3],[1,3,2],[3,2,1]])
    assert(is_latin_hypercube(badlh) is False)

