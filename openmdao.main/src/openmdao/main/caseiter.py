from itertools import chain


def caseiter_to_dict(caseiter, varnames, include_errors=False):
    """
    Retrieve the values of specified variables from cases in a CaseIterator.
    
    Returns a dict containing a list of values for each entry, keyed on 
    variable name.
    
    Only data from cases containing ALL of the specified variables will
    be returned so that all data values with the same index will correspond
    to the same case.
    
    caseiter: CaseIterator
        A CaseIterator containing the cases of interest.

    varnames: list[str]
        Iterator of names of variables to be retrieved.
        
    include_errors: bool (optional) [False]
        If True, include data from cases that reported an error.
        
    """
    
    varnames = set(varnames)
    
    vardict = dict([(name,[]) for name in varnames])

    for case in caseiter.get_iter():
        casedict = {}
        if include_errors is False and case.msg:
            continue  # case reported an error or warning message

        for vname,idx,value in chain(case.inputs, case.outputs):
            if idx:
                vname = "%s%s" % (vname, idx)
            if vname in varnames:
                casedict[vname] = value
        
        if len(casedict) != len(varnames):
            continue   # case doesn't contain a complete set of specified vars, so skip it to avoid data mismatches
        
        for name, value in casedict.items():
            vardict[name].append(value)
            
    return vardict
    
