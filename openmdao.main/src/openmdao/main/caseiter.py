
from openmdao.main.case import Case


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
    
    vardict = dict([(name,[]) for name in varnames])

    for case in caseiter:
        if include_errors is False and case.msg:
            continue  # case reported an error or warning message
        try:
            casevals = [case[name] for name in vardict]
            idx = 0
            for name, lst in vardict.items():
                lst.append(casevals[idx])
                idx += 1
        except KeyError:
            continue # case doesn't contain a complete set of specified vars, 
                     # so skip it to avoid data mismatches
    return vardict
    
