
from openmdao.main.case import Case

class CaseSet(object):
    """A CaseRecorder/CaseIterator containing Cases having the same set of input/output strings
    but different data.
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
        self._inputs = None
        self._outputs = None
        self._parent_id = parent_id
        if isinstance(obj, dict):
            self._validate_dict(obj)
            self._casedata = obj.copy()
        elif isinstance(obj, Case):
            self._casedata = {}
            for name,val in obj.items():
                self._casedata[name] = [val]
            self._inputs = set(obj._inputs.keys())
            if obj._outputs:
                self._outputs = set(obj._outputs.keys())
        elif obj is None:
            self._casedata = None
        else:
            raise TypeError("obj must be a dict, a Case, or None")
                
    def _validate_dict(self, dct):
        length = -1
        for key, val in dct.items():
            if not isinstance(key, basestring):
                raise TypeError("dictionary key '%s' is not a string" % key)
            if length < 0:
                length = len(val)
            if length != len(val):
                raise ValueError("number of values at key '%s' (%d) differs " % (key,len(val)) +
                                 "from number of other values (%d) in CaseSet" % length)

    def record(self, case):
        """Record the given Case."""
        if self._casedata is None:
            for name,val in case.items():
                self._casedata[name] = [val]
            self._inputs = set(case._inputs.keys())
            if case._outputs:
                self._outputs = set(case._outputs.keys())
        else:
            try:
                for name,val in case.items():
                    self._casedata[name].append(val)
            except KeyError:
                badname = name
                for name,val in case.items():
                    if name == badname:
                        break
                    self._casedata[name].pop() # remove value we added for this bad Case
    
    def get_iter(self):
        return self._next_case()

    def _next_case(self):
        numvals = 0
        for name, lst in self._casedata.iteritems(): 
            numvals = len(lst)  # just need to get number of values, so grab first variable
            break
        
        for i in range(numvals):
            yield self.__getitem__(i)
    
    def __getitem__(self, idx):
        if isinstance(idx, basestring):
            return self._casedata[idx].copy()  # return all of the values for the given name
        else:  # return Case number idx
            for name, lst in self._casedata.items():
                inputs = []
                outputs = []
                if self._outputs and name in self._outputs:
                    outputs.append((name, lst[idx]))
                else:
                    inputs.append((name, lst[idx]))
            return Case(inputs=inputs, output=outputs, parent_id=self._parent_id)
            
            