
from openmdao.main.case import Case

class CaseArray(object):
    """A CaseRecorder/CaseIterator containing Cases having the same set of
    input/output strings but different data. Cases are not necessarily unique.
    """
    def __init__(self, obj=None, parent_id=None):
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
        """
        self._parent_id = parent_id
        self._names = []
        self._values = []
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
        self._names = dct.keys()
        self._split_idx = len(self._names) # treat all names as inputs
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
                self._add_values(lst)

    def _record_first_case(self, case):
        """Called the first time we record a Case"""
        self._names = case.keys(iotype='in')
        tmp = [v for k,v in case.items(iotype='in')]
        self._split_idx = len(self._names)  # index where we switch from inputs to outputs
        self._names.extend(case.keys(iotype='out'))
        tmp.extend([v for k,v in case.items(iotype='out')])
        self._add_values(tmp)
        
    def record(self, case):
        """Record the given Case."""
        if not self._values:
            self._record_first_case(case)
        else:
            self._add_values(self._get_case_data(case))
    
    def get_iter(self):
        return self._next_case()

    def _next_case(self):
        for i in range(len(self._values)):
            yield self.__getitem__(i)
    
    def __getitem__(self, key):
        """If key is a varname or expression, returns a list of
        all of the recorded values corresponding to that string. If key is an integer 
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
            
    def _get_case_data(self, case):
        """Return a list of values for the case in the same order as our values.
        If the keys in the given case don't match ours, raise a KeyError.
        """
        if len(self._names) != len(case):
            raise ValueError("case has different inputs/outputs than CaseSet")
        try:
            return [case[n] for n in self._names]
        except KeyError, err:
            raise KeyError("input or output is missing from case: %s" % str(err))
        
    def _add_values(self, vals):
        self._values.append(vals)

    def __len__(self):
        return len(self._values)
    
    def __contains__(self, case):
        try:
            values = self._get_case_data(case)
        except KeyError:
            return False
        for val in self._values:
            if val == values:
                return True
        return False
    
    def clear(self):
        """Remove all case values from this container, but leave list of
        variables intact.
        """
        self._values = []

    def update(self, *case_containers):
        """Add Cases from other CaseSets or CaseArrays to this one."""
        for cset in case_sets:
            for vals in cset._values:
                self._add_values(vals)
                
    def pop(self, idx=-1):
        return self._values.pop(idx)
                
    def _check_compatability(self, case_container):
        if self._names != case_container._names:
            raise ValueError("case containers have different sets of variables")
        if self._split_idx != case_container._split_idx:
            raise ValueError("case containers don't agree on input/output designations")


class CaseSet(CaseArray):
    """A CaseRecorder/CaseIterator containing Cases having the same set of
    input/output strings but different data.  All Cases in the set are unique.
    """
    def __init__(self, obj=None, parent_id=None):
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
        """
        self._tupset = set()
        super(CaseSet, self).__init__(obj, parent_id)

    def _add_values(self, vals):
        tup = tuple(vals)
        if tup not in self._tupset:
            self._tupset.add(tup)
            self._values.append(tup)

    def __contains__(self, case):
        if not isinstance(case, Case):
            return False
        try:
            values = tuple(self._get_case_data(case))
        except KeyError:
            return False
        return values in self._tupset
    
    def _make_case_set(self, tupset):
        cs = CaseSet(parent_id=self._parent_id)
        cs._names = self._names[:]
        cs._values = list(tupset)
        cs._tupset = tupset
        cs._split_idx = self._split_idx
        return cs
    
    def isdisjoint(self, case_set):
        """Return True if this CaseSet has no Cases in common with the
        given CaseSet.
        """
        self._check_compatability(case_set)
        return self._tupset.isdisjoint(case_set._tupset)
    
    def issubset(self, case_set):
        """Return True if every Case in this one is in the given CaseSet."""
        self._check_compatability(case_set)
        return self._tupset.issubset(case_set._tupset)
    
    def issuperset(self, case_set):
        """Return True if every Case in the given CaseSet is in this one."""
        self._check_compatability(case_set)
        return self._tupset.issuperset(case_set._tupset)
    
    def union(self, *case_sets):
        """Return a new CaseSet with Cases from this one
        and all others.
        """
        tupsets = []
        for cset in case_sets:
            self._check_compatability(cset)
            tupsets.append(cset._tupset)
        return self._make_case_set(self._tupset.union(*tupsets))
    
    def intersection(self, *case_sets):
        """Return a new CaseSet with Cases that are common to this
        and all others.
        """
        tupsets = []
        for cset in case_sets:
            self._check_compatability(cset)
            tupsets.append(cset._tupset)
        return self._make_case_set(self._tupset.intersection(*tupsets))
    
    def difference(self, *case_sets):
        """Return a new CaseSet with Cases in this that are not in the
        others.
        """
        tupsets = []
        for cset in case_sets:
            self._check_compatability(cset)
            tupsets.append(cset._tupset)
        return self._make_case_set(self._tupset.difference(*tupsets))
    
    def symmetric_difference(self, case_set):
        """Return a new CaseSet with Cases in either this one or the other but
        not both.
        """
        self._check_compatability(case_set)
        return self._make_case_set(self._tupset.symmetric_difference(case_set._tupset))
    
    def clear(self):
        """Remove all case values from this CaseSet, but leave list of
        variables intact.
        """
        super(CaseSet, self).clear()
        self._tupset = set()

    def pop(self, idx=-1):
        val = self._values.pop(idx)
        self._tupset.remove(val)
        return val
                
    def __eq__(self, caseset):
        self._check_compatability(caseset)
        return self._tupset == caseset._tupset
    
    def __lt__(self, caseset):
        self._check_compatability(caseset)
        return self._tupset < caseset._tupset
        
    def __le__(self, caseset):
        self._check_compatability(caseset)
        return self._tupset <= caseset._tupset
        
    def __gt__(self, caseset):
        self._check_compatability(caseset)
        return self._tupset > caseset._tupset
        
    def __ge__(self, caseset):
        self._check_compatability(caseset)
        return self._tupset >= caseset._tupset
        
    def __or__(self, caseset): return self.union(caseset)
    
    def __and__(self, caseset): return self.intersection(caseset)
    
    def __sub__(self, caseset): return self.difference(caseset)
    