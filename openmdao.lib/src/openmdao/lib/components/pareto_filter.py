""" Pareto Filter -- finds non-dominated cases. """

# pylint: disable-msg=E0611,F0401
from numpy import array, zeros

from openmdao.main.datatypes.api import Array, List, VarTree
from openmdao.main.api import Component
from openmdao.main.vartree import VariableTree

class ParetoFilter(Component):
    """Takes a set of cases and filters out the subset of cases which are
    pareto optimal. Assumes that smaller values for model responses are
    better, so all problems must be posed as minimization problems.

    You must pass a tuple of responses into the constructor. You can also
    optionally pass a tuple of params if you want ParetoFilter to keep track
    of the inputs for the nondominated responses.
    """

    # pylint: disable-msg=E1101
    params = VarTree(VariableTree(), iotype='in', noflat=True)

    responses = VarTree(VariableTree(), iotype='in', noflat=True)

    constraints = VarTree(
        VariableTree(), iotype='in',
        desc='VarTree of minus-definition constraints '
        '(<=0 means constaint satisfied) used to filter cases following '
        'the rule that satifisfied is better than unsatisfied and small '
        'unstatisfied constraint value is better than greater one, '
        'regardless of objective function values.'
        'Optional if None')

    pareto_inputs = Array(iotype='out', desc='Array of input values in the '
                          'Pareto frontier')

    pareto_outputs = Array(iotype='out', desc='Array of response values in the '
                          'Pareto frontier')

    pareto_outcons = Array(
        iotype='out', desc='Array of constraints values in the Pareto frontier')

    def __init__(self, params=None, responses=None, constraints=None):
        super(ParetoFilter, self).__init__()

        # Make params optional.
        if params is None:
            params = tuple()

        # Make constraints optional.
        if constraints is None:
            constraints = tuple()


        if not isinstance(params, tuple):
            msg = "ParetoFilter optional params argument needs to be a " + \
                  "tuple of variable names."
            self.raise_exception(msg, ValueError)

        if not isinstance(constraints, tuple):
            msg = 'ParetoFilter optional constraints argument '\
                  'needs to be a tuple of constraint variable names.'
            self.raise_exception(msg, ValueError)

        if responses is None or not isinstance(responses, tuple):
            msg = "ParetoFilter responses argument needs to be a tuple of " + \
                  "variable names."
            self.raise_exception(msg, ValueError)

        # Add our inputs and outputs to the vartrees.

        input_tree = self.get('params')
        self._param_data = []
        for name in params:
            input_tree.add(name, List([], desc='ParetoFilter input', noflat=True))

        output_tree = self.get('responses')
        self._response_data = {}
        for name in responses:
            output_tree.add(name, List([], desc='ParetoFilter response', noflat=True))

        constraint_tree = self.get('constraints')
        self._constraint_data= []
        for name in constraints:
            constraint_tree.add(name, List([], desc='ParetoFilter constraint', noflat=True))

        self._param_names = params
        self._response_names = responses
        self._constraint_names = constraints

        self.pareto_inputs = zeros((1, len(params)))
        self.pareto_outputs = zeros((1, len(responses)))
        self.pareto_outcons = zeros((1, len(constraints)))

    def _is_dominated(self, y1, y2):
        """Tests to see if the point y1 is dominated by the point y2.
        True if y1 is dominated by y2, False otherwise.
        """
        if y1 == y2:
            return False
        for a, b in zip(y1, y2):
            if a is None:
                return True
            if b is None:
                return False
            if a < b:
                return False

        return True

    def _is_constrained(self, c1, c2):
        """ Tests to see if the point c1 is more or less constrained than
        the point c2.
        Constrained means value <= 0.0
        return:
            isLessConstrained,
                True if some constraint values are more than c2
                while the other same, False Otherwise
            NeedCompareDominating
                True if need furthermore comparison of dominating
                False otherwise
        """
        c1 = array(c1)
        c2 = array(c2)
        b1 = (c1 <= 0).all()
        b2 = (c2 <= 0).all()

        if b1 and b2:
            return False, True
        if b1 and not b2:
            return False, False
        if not b1 and b2:
            return True, False
        if not b2 and not b1:
            c1[c1<=0] = 0
            c2[c2<=0] = 0
            if (c1 == c2).all():
                return False, True

            if (c1 >= c2).all():
                return True, False
            else:
                return False, False

    def execute(self):
        """Returns an araray of pareto optimal points and their response values.
        """

        first_name = "responses.%s" % self._response_names[0]
        n_param = len(self._param_names)
        n_response = len(self._response_names)
        n_constraint = len(self._constraint_names)
        n_points = len(self.get(first_name))

        # Get our output data once, then rearrange it after.
        data = []
        for varname in self._response_names:
            name = "responses.%s" % varname
            val = self.get(name)
            data.append(val)

        all_points = []
        for j in xrange(0, n_points):
            point = []
            for i in xrange(0, n_response):
                point.append(data[i][j])
            all_points.append(point)

        nondominated_output = list(all_points)

        # Optionally, get our inputs data once, then rearrange it after.
        if n_param > 0:

            data = []
            for varname in self._param_names:
                name = "params.%s" % varname
                val = self.get(name)
                data.append(val)

            nondominated_input = []
            for j in xrange(0, n_points):
                point = []
                for i in xrange(0, n_param):
                    point.append(data[i][j])
                nondominated_input.append(point)

        # Optionally, get our constraint data once, then rearrange it after.
        if n_constraint > 0:

            data = []
            for varname in self._constraint_names:
                name = "constraints.%s" % varname
                val = self.get(name)
                data.append(val)

            all_cons = []
            for j in xrange(0, n_points):
                point = []
                for i in xrange(0, n_constraint):
                    point.append(data[i][j])
                all_cons.append(point)
            nondominated_constraint = list(all_cons)


            # Find non-dominated points with constraint filtering
            nd_input_list = []
            for j, (point1, cons1) in enumerate(zip(all_points, all_cons)):
                for point2, cons2 in zip(nondominated_output,
                                  nondominated_constraint):
                    is_less_cons, need_comp = self._is_constrained(cons1, cons2)
                    if is_less_cons or \
                       (need_comp and self._is_dominated(point1, point2)):
                        nondominated_output.remove(point1)
                        nondominated_constraint.remove(cons1)
                        nd_input_list.append(j)
                        break
        else:
            # Find non-dominated points directly
            nd_input_list = []
            for j, point1 in enumerate(all_points):
                for point2 in nondominated_output:
                    if self._is_dominated(point1, point2):
                        nondominated_output.remove(point1)
                        nd_input_list.append(j)
                        break

        self.pareto_outputs = array(nondominated_output)

        if n_param > 0:
            data = [nondominated_input[j] for j in xrange(0, n_points) \
                    if j not in nd_input_list]
            self.pareto_inputs = array(data)
        if n_constraint > 0:
            self.pareto_outcons = array(nondominated_constraint)


