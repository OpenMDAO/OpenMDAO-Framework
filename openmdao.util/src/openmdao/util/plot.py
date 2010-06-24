"""
Utility functions related to plotting data
"""

import sqlite3

def get_data(dbname, varnames, case_sql=None, var_sql=None):
    """
    Retrieve the values of specified variables from a sqlite DB containing
    Case data.
    
    Returns a dict containing a list of values for each entry, keyed on 
    variable name.
    
    dbname : str
        The name of the sqlite DB file
        
    varnames : list[str]
        The list of names of variables to be plotted
        
    case_sql : str, optional
        Optional SQL syntax that will be placed in the WHERE clause for Case retrieval
        
    var_sql : str, optional
        Option SQL syntax that will be placed in the WHERE clause for variable retrieval
    
    """
    connection = sqlite3.connect(dbname)
    
    sql = ["SELECT * FROM cases"]
    if case_sql:
        sql.append("WHERE %s" % case_sql)
        
    casecur = connection.cursor()
    casecur.execute(' '.join(sql))
    
    sql = ["SELECT name, value, entry from casevars WHERE case_id=%s"]
    for i,name in enumerate(varnames):
        if i==0:
            sql.append("AND (")
        else:
            sql.append("OR")
        sql.append("name='%s'" % name)
    if i>0: sql.append(")")
    
    if var_sql:
        sql.append("AND %s" % var_sql)
    combined = ' '.join(sql)
    
    varcur = connection.cursor()
    
    vardict = {}
    for case_id,name,msg,retries,model_id,timeEnter in casecur:
        varcur.execute(combined % case_id)
        for vname, value, entry in varcur:
            if entry:
                vname = "vname%s" % entry
            dval = vardict.setdefault(vname, [])
            dval.append(value)
            print vname,'=',value
            
    return vardict


def get_lines(dbname, xnames, ynames, case_sql=None, var_sql=None): 
    
    vardict = get_data(dbname, xnames+ynames, case_sql, var_sql)

    lines = []
    yvals = []
    xvals = []
    for i,name in enumerate(ynames):
        yvals.append(vardict[name])
        if len(xnames) == 0:
            xvals.append(range(len(vardict[name])))
        elif len(xnames) == 1:
            xvals.append(vardict[xnames[0]])
        else:
            xvals.append(vardict[xnames[i]])
    
    for xdata,ydata in zip(xvals, yvals):
        lines.append((xdata, ydata))
        
    return lines