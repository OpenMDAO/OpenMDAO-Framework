"""
Case filters provide a means to select cases based on their index in the
sequence of cases or the case data values.
"""

import math
import sys

from zope.interface import implements

from openmdao.main.interfaces import ICaseFilter


# This dict will act as the local scope when we eval our expressions.
_EXPR_DICT = {'math': math}
# Add stuff from math lib directly to our locals dict so users won't have to
# put 'math.' in front of all of their calls to standard math functions.
for name in dir(math):
    if not name.startswith('_'):
        _EXPR_DICT[name] = getattr(math, name)

import numpy
_EXPR_DICT['numpy'] = numpy


class IteratorCaseFilter(object):
    """
    Select based on an iterator of case numbers.

    iterator: iterator
        Provides case numbers, assumed to be in increasing order.
    """

    implements(ICaseFilter)

    def __init__(self, iterator):
        self._iterator = iterator
        self._current = None
        self._next()

    def select(self, seqno, case):
        """
        Return True if `case` should be used.

        seqno: int
            Case sequence number.

        case: Case
            Case to be checked.
        """
        if self._current is None:
            return False

        while seqno > self._current:
            self._next()
            if self._current is None:
                return False
        if seqno < self._current:
            return False
        else:
            self._next()
            return True

    def _next(self):
        """ Set `_current` to next value in sequence or None. """
        old = self._current
        try:
            self._current = self._iterator.next()
        except StopIteration:
            self._current = None
        else:
            if old is not None and self._current <= old:
                raise RuntimeError('non-increasing sequence at %s,%s'
                                   % (old, self._current))


class SequenceCaseFilter(IteratorCaseFilter):
    """
    Select based on a sequence of case numbers.

    indices: sequence(int)
        Sequence of case numbers, assumed to be in increasing order.
    """

    def __init__(self, indices):
        super(SequenceCaseFilter, self).__init__(iter(indices))


class SliceCaseFilter(IteratorCaseFilter):
    """
    Select based on `start`, `stop`, and `step` of case numbers.

    start: int
        Starting case number.

    stop: int
        Ending case number.

    step: int
        Step between case numbers.
    """

    def __init__(self, start=0, stop=sys.maxint, step=1):
        super(SliceCaseFilter, self).__init__(iter(xrange(start, stop, step)))


class ExprCaseFilter(object):
    """
    Select based on a boolean Python expression of the case data or ``seqno``.
    Case data is accessed by ``case['name']``

    expr: string
        Boolean expression referring to the case data or ``seqno``.

    Examples:
        - Select first 3 cases: ``'seqno < 3'``.
        - Select case with 'param' between 2 and 2.5: \
        ``'case["param"] > 2 and case["param"] < 2.5'``.
    """

    implements(ICaseFilter)

    def __init__(self, expr):
        self.expr = expr

    def __getstate__(self):
        """Return dict representing this container's state."""
        
        state = self.__dict__.copy()
        
        # Compiled stuff doesn't pickle
        state['_code'] = None
        
    def __setstate__(self, state):
        """Restore this component's state."""
        
        self.__dict__.update(state)
        self._code = compile(self._expr, '<string>', 'eval')
        
    @property
    def expr(self):
        """ The expression to be evaluated. """
        return self._expr

    @expr.setter
    def expr(self, value):
        """ Set the expression and compile it. """
        self._expr = value
        try:
            self._code = compile(self._expr, '<string>', 'eval')
        except Exception as exc:
            raise type(exc)("Can't compile expression %r: %s"
                            % (self._expr, exc))

    def select(self, seqno, case):
        """
        Return True if `case` should be used.

        seqno: int
            Case sequence number.

        case: Case
            Case to be checked.
        """
        try:
            return eval(self._code, _EXPR_DICT, dict(seqno=seqno, case=case))
        except Exception as exc:
            raise type(exc)("Can't evaluate expression %r: %s"
                            % (self._expr, exc))

