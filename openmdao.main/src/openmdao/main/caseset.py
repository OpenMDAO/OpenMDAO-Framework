
from openmdao.main.case import Case

class CaseSet(object):
    """A CaseRecorder/CaseIterator containing Cases having the same set of input/output strings
    but different data.
    """
    def __init__(self, obj=None, parent_id=None, unique=False):
        """
        obj: dict, Case, or None
            if obj is a dict, it is assumed to contain all var names as keys, with
            values that are lists.  All lists are assumed to have the same length.
            
            if obj is a Case, the inputs and outputs of the Case will become those
            of the CaseSet, and any subsequent Cases that are added must have the
            same set of inputs and outputs.
            
            if obj is None, the first Case that is recorded will be used to set
            the inputs and outputs for the CaseSet.
        
        parent_id: str
            The id of the parent Case (if any)
            
        unique: bool
            If True, don't add cases with values that duplicate existing cases in the
            set.
        """
        self._parent_id = parent_id
        self._names = []
        self._values = []
        if unique: # if uniqe is True, store values as tuples in a set and force uniqueness
            self._tupset = set()
        else:
            self._tupset = None
        if isinstance(obj, dict):
            self._add_dict_cases(obj)
        elif isinstance(obj, Case):
            self.record(obj)
        elif obj is None:
            pass
        else:
            raise TypeError("obj must be a dict, a Case, or None")
                
    def _add_dict_cases(self, dct):
        length = -1
        inputs = [] # we can't tell what's an input or an output, so treat all as inputs
        self._names = dct.keys()
        biglist = []
        for key, val in dct.items():
            if not isinstance(key, basestring):
                raise TypeError("dictionary key '%s' is not a string" % key)
            if length < 0:
                length = len(val)
            if length != len(val):
                raise ValueError("number of values at key '%s' (%d) differs " % (key,len(val)) +
                                 "from number of other values (%d) in CaseSet" % length)
            biglist.append(val)
        self._values = []
        if length > 0:
            idxs = range(len(self._names))
            for i in range(length):
                lst = [biglist[j][i] for j in idxs]
                if self._tupset is None:
                    self._values.append(lst)
                else:
                    tup = tuple(lst)
                    if tup not in self._tupset:
                        self._tupset.add(tup)
                        self._values.append(tup)

    def _first_case(self, case):
        """Called the first time we record a Case"""
        self._names = case.keys(iotype='in')
        tmp = [v for k,v in case.items(iotype='in')]
        self._split_idx = len(self._names)  # index where we switch from inputs to outputs
        self._names.extend(case.keys(iotype='out'))
        tmp.extend([v for k,v in case.items(iotype='out')])
        if self._tupset is not None:
            tmp = tuple(tmp)  # if we're storing tuples in a set, store values as tuple
                              # and store the same tuple in both places to save space
            self._tupset.add(tmp)
        self._values.append(tmp)  # _values may contain either lists or tuples
        
    def record(self, case):
        """Record the given Case."""
        if not self._values:
            self._first_case(case)
        else:
            if len(self._names) != len(case):
                raise ValueError("case has different inputs/outputs than CaseSet")
            try:
                tmp = [case[n] for n in self._names]
            except KeyError, err:
                raise KeyError("input or output is missing from case: %s" % str(err))
            if self._tupset is None:
                self._values.append(tmp)
            else:
                tmp = tuple(tmp)
                if tmp not in self._tupset:
                    self._tupset.add(tmp)
                    self._values.append(tmp)
    
    def get_iter(self):
        return self._next_case()

    def _next_case(self):
        for i in range(len(self._values)):
            yield self.__getitem__(i)
    
    def __getitem__(self, key):
        """If given a string with a varname or expression, returns a list of
        all of the recorded values corresponding to that string. If given an 
        index 'i', returns a Case object containing the data for the i'th recorded 
        case.
        """
        if isinstance(key, basestring): # return all of the values for the given name
            idx = self._names.index(key)
            return [lst[idx] for lst in self._values]
        else:  # key is the case number
            lst = self._values[key]
            return Case(inputs=[(n,v) for n,v in zip(self._names[0:self._split_idx],
                                                     lst[0:self._split_idx])],
                        outputs=[(n,v) for n,v in zip(self._names[self._split_idx:],
                                                      lst[self._split_idx:])],
                        parent_id=self._parent_id)
            
            