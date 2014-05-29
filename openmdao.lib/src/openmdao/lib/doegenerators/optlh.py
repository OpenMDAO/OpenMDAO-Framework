# This implementation is based on a matlab implementation that has the
# following license:
#
# Copyright 2007 A Sobester
#
# This program is free software: you can redistribute it and/or modify  it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or any
# later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License and GNU
# Lesser General Public License along with this program. If not, see
# <http://www.gnu.org/licenses/>.

import logging
from random import randint, shuffle, seed

# pylint: disable-msg=E0611,F0401
from numpy import array, size, sum, floor, zeros
from numpy.linalg import norm

from openmdao.main.datatypes.api import Int, Enum
from openmdao.main.interfaces import implements, IDOEgenerator
from openmdao.main.api import Container


def rand_latin_hypercube(n, k, edges=False):
    """
    Calculates a random Latin hypercube set of n points in k
    dimensions within [0,1]^k hypercube.

    n: int
       Desired number of points.
    k: int
       Number of design variables (dimensions).
    edges: bool (optional)
       If Edges=True, the extreme bins will have their centres on the
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
    """Returns True if the given array is a Latin hypercube.
    The given array is assumed to be a numpy array.
    """
    n,k = lh.shape
    for j in range(k):
        col = lh[:,j]
        colset = set(col)
        if len(colset) < len(col):
            return False  # something was duplicated
    return True


class LHC_indivudal(object):

    def __init__(self, doe, q=2, p=1):
        self.q = q
        self.p = p
        self.doe = doe
        self.phi = None # Morris-Mitchell sampling criterion

    @property
    def shape(self):
        """Size of the LatinHypercube DOE (rows,cols)."""
        return self.doe.shape

    def mmphi(self):
        """Returns the Morris-Mitchell sampling criterion for this Latin hypercube."""

        if self.phi is None:
            n,m = self.doe.shape
            distdict = {}

            #calculate the norm between each pair of points in the DOE
            # TODO: This norm takes up the majority of the computation time. It
            # should be converted to C or ShedSkin.
            arr = self.doe
            for i in range(n):
                for j in range(i+1, n):
                    nrm = norm(arr[i]-arr[j], ord=self.p)
                    distdict[nrm] = distdict.get(nrm, 0) + 1

            distinct_d = array(distdict.keys())

            #mutltiplicity array with a count of how many pairs of points have a given distance
            J = array(distdict.values())

            self.phi = sum(J*(distinct_d**(-self.q)))**(1.0/self.q)

        return self.phi

    def perturb(self, mutation_count):
        """ Interchanges pairs of randomly chosen elements within randomly chosen
        columns of a DOE a number of times. The result of this operation will also
        be a Latin hypercube.
        """
        new_doe = self.doe.copy()
        n,k = self.doe.shape
        for count in range(mutation_count):
            col = randint(0, k-1)

            #choosing two distinct random points
            el1 = randint(0, n-1)
            el2 = randint(0, n-1)
            while el1==el2:
                el2 = randint(0, n-1)

            new_doe[el1, col] = self.doe[el2, col]
            new_doe[el2, col] = self.doe[el1, col]

        return LHC_indivudal(new_doe, self.q, self.p)

    def __iter__(self):
        return self._get_rows()

    def _get_rows(self):
        for row in self.doe:
            yield row

    def __repr__(self):
        return repr(self.doe)

    def __str__(self):
        return str(self.doe)

    def __getitem__(self,*args):
        return self.doe.__getitem__(*args)


_norm_map = {"1-norm":1,"2-norm":2}


class LatinHypercube(Container):
    """IDOEgenerator which provides a Latin hypercube DOE sample set.
    """
    implements(IDOEgenerator)

    num_samples = Int(20, desc="Number of sample points in the DOE sample set.")

    num_parameters = Int(2, desc="Number of parameters, or dimensions, for the DOE.")

    seed = Int(None, iotype="in",
               desc="Random seed for the optimizer. Set to a specific value "
                    "for repeatable results; otherwise leave as None for truly "
                    "random seeding.")


    def __init__(self, num_samples=None, ):
        super(LatinHypercube,self).__init__()

        if num_samples is not None:
            self.num_samples = num_samples


    def __iter__(self):
        """Return an iterator over our sets of input values."""
        if self.seed is not None:
            seed(self.seed)

        return self._get_input_values()

    def _get_input_values(self):
        rand_doe = rand_latin_hypercube(self.num_samples, self.num_parameters)

        for row in rand_doe:
            yield row

class OptLatinHypercube(Container):
    """IDOEgenerator which provides a Latin hypercube DOE sample set.
    The Morris-Mitchell sampling criterion of the DOE is optimzied
    using an evolutionary algorithm.
    """
    implements(IDOEgenerator)

    num_samples = Int(20, desc="Number of sample points in the DOE sample set.")

    num_parameters = Int(2, desc="Number of parameters, or dimensions, for the DOE.")

    population = Int(20,
        desc="Size of the population used in the evolutionary optimization.")
    generations = Int(2,
        desc="Number of generations the optimization will evolve over.")
    norm_method = Enum(["1-norm","2-norm"],
                    desc="Vector norm calculation method. '1-norm' is faster but less accurate.")

    seed = Int(None, iotype="in",
               desc="Random seed for the optimizer. Set to a specific value "
                    "for repeatable results; otherwise leave as None for truly "
                    "random seeding.")


    def __init__(self, num_samples=None, population=None,generations=None):
        super(OptLatinHypercube,self).__init__()

        self.qs = [1,2,5,10,20,50,100] #list of qs to try for Phi_q optimization
        if num_samples is not None:
            self.num_samples = num_samples
        if population is not None:
            self.population = population
        if generations is not None:
            self.generations = generations

    def __iter__(self):
        """Return an iterator over our sets of input values."""
        if self.seed is not None:
            seed(self.seed)
        return self._get_input_values()

    def _get_input_values(self):
        rand_doe = rand_latin_hypercube(self.num_samples, self.num_parameters)
        best_lhc = LHC_indivudal(rand_doe, q=1, p=_norm_map[self.norm_method])

        for q in self.qs:
            lh = LHC_indivudal(rand_doe, q, _norm_map[self.norm_method])
            lh_opt = _mmlhs(lh, self.population, self.generations)
            if lh_opt.mmphi() < best_lhc.mmphi():
                best_lhc = lh_opt

        for row in best_lhc:
            yield row


def _mmlhs(x_start, population, generations):
    """Evolutionary search for most space filling Latin-Hypercube.
    Returns a new LatinHypercube instance with an optimized set of points.
    """
    x_best = x_start
    phi_best = x_start.mmphi()
    n = x_start.shape[1]

    level_off = floor(0.85*generations)
    for it in range(generations):

        if it < level_off and level_off > 1.:
            mutations = int(round(1+(0.5*n-1)*(level_off-it)/(level_off-1)))
        else:
            mutations = 1

        x_improved = x_best
        phi_improved = phi_best

        for offspring in range(population):
            x_try = x_best.perturb(mutations)
            phi_try = x_try.mmphi()

            if phi_try < phi_improved:
                x_improved = x_try
                phi_improved = phi_try

        if phi_improved < phi_best:
            phi_best = phi_improved
            x_best = x_improved

    return x_best


if __name__== "__main__":  # pragma no cover
    import sys

    lh1 = array([[1,2,3],[3,1,2],[2,3,1]])
    assert(is_latin_hypercube(lh1))
    badlh = array([[1,2,3],[1,3,2],[3,2,1]])
    assert(is_latin_hypercube(badlh) is False)

    try:
        from matplotlib import pyplot
    except ImportError:
        print "Couldn't find matplotlib"

    test = """
x = rand_latin_hypercube(80,2)
lh = LatinHypercube(x,2,1)
print lh.mmphi()
lh_opt = _mmlhs(lh,20,20)
print lh_opt.mmphi()
"""
    if '--profile' in sys.argv:
        import cProfile
        import pstats
        cProfile.run(test,"test.prof")
        p = pstats.Stats("test.prof")
        p.sort_stats('cumulative').print_stats(10)
    else:
        exec(test)
    if 'pyplot' in globals():
        pyplot.figure(1)
        pyplot.scatter(lh[:,0],lh[:,1])
        pyplot.scatter(lh_opt[:,0],lh_opt[:,1],c='r')
        pyplot.show()
