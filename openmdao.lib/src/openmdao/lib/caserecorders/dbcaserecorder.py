
import sys
import sqlite3
from cPickle import dumps, loads, HIGHEST_PROTOCOL, UnpicklingError
from optparse import OptionParser

from enthought.traits.api import implements

from openmdao.main.interfaces import ICaseRecorder
from openmdao.main.case import Case
from openmdao.lib.caseiterators.dbcaseiter import DBCaseIterator


class DBCaseRecorder(object):
    """Records Cases to a relational DB (sqlite). Values other than floats
    or ints are pickled and are opaque to SQL queries.
    """
    
    implements(ICaseRecorder)
    
    def __init__(self, dbfile=':memory:', model_id='', append=False):
        self.dbfile = dbfile  # this creates the connection
        self.model_id = model_id
        
        if append:
            exstr = 'if not exists'
        else:
            exstr = ''
        
        self._connection.execute("""
        create table %s cases(
         id INTEGER PRIMARY KEY,
         cname TEXT,
         msg TEXT,
         retries INTEGER,
         model_id TEXT,
         timeEnter TEXT
         )""" % exstr)
        
        self._connection.execute("""
        create table %s casevars(
         var_id INTEGER PRIMARY KEY,
         name TEXT,
         case_id INTEGER,
         sense TEXT,
         value BLOB,
         idx TEXT
         )""" % exstr)

    @property
    def dbfile(self):
        return self._dbfile
    
    @dbfile.setter
    def dbfile(self, value):
        """Set the DB file and connect to it."""
        self._dbfile = value
        self._connection = sqlite3.connect(value)
    
    def record(self, case):
        """Record the given Case."""
        cur = self._connection.cursor()
        
        if not case.msg:
            case.msg = ''
        cur.execute("""insert into cases(id,cname,msg,retries,model_id,timeEnter) 
                           values (?,?,?,?,?,DATETIME('NOW'))""", 
                                     (None, case.ident, case.msg, case.retries, self.model_id))
        case_id = cur.lastrowid
        # insert the inputs and outputs into the vars table.  Pickle them if they're not one of the
        # built-in types int, float, or str.
        vlist = []
        for name,idx,value in case.inputs:
            if isinstance(value, (float,int,str)):
                vlist.append((None, name, case_id, 'i', value, idx))
            else:
                vlist.append((None, name, case_id, 'i', sqlite3.Binary(dumps(value,HIGHEST_PROTOCOL)), idx))
        for name,idx,value in case.outputs:
            if isinstance(value, (float,int,str)):
                vlist.append((None, name, case_id, 'o', value, idx))
            else:
                vlist.append((None, name, case_id, 'o', sqlite3.Binary(dumps(value,HIGHEST_PROTOCOL)), idx))
        for v in vlist:
            cur.execute("insert into casevars(var_id,name,case_id,sense,value,idx) values(?,?,?,?,?,?)", 
                            v)
        self._connection.commit()

    def get_iterator(self):
        """Return a DBCaseIterator that points to our current DB."""
        return DBCaseIterator(dbfile=self._dbfile, connection=self._connection)


    
"""
Utility functions related to plotting data
"""

def list_db_vars(dbname):
    """
    Return the set of the names of the variables found in the specified case DB file.
    
    dbname: str
        The name of the sqlite DB file.
    """
    connection = sqlite3.connect(dbname)
    varnames = set()
    varcur = connection.cursor()
    varcur.execute("SELECT name, idx from casevars")
    for vname, idx in varcur:
        if idx:
            vname = "vname%s" % idx
        varnames.add(vname)

    return varnames

def case_db_to_dict(dbname, varnames, case_sql='', var_sql='', include_errors=False):
    """
    Retrieve the values of specified variables from a sqlite DB containing
    Case data.
    
    Returns a dict containing a list of values for each entry, keyed on 
    variable name.
    
    Only data from cases containing ALL of the specified variables will
    be returned so that all data values with the same index will correspond
    to the same case.
    
    dbname: str
        The name of the sqlite DB file.
        
    varnames: list[str]
        iterator of names of variables to be retrieved.
        
    case_sql: str, optional
        SQL syntax that will be placed in the WHERE clause for Case retrieval.
        
    var_sql: str, optional
        SQL syntax that will be placed in the WHERE clause for variable retrieval.
    
    include_errors: bool, optional [False]
        if True, include data from cases that reported an error
        
    """
    varnames = set(varnames)
    connection = sqlite3.connect(dbname)
    sql = ["SELECT id FROM cases"]
    qlist = []
    if case_sql:
        qlist.append(case_sql)
    if not include_errors:
        qlist.append("msg = ''")
            
    if qlist:
        sql.append("WHERE %s" % ' AND '.join(qlist))
        
    casecur = connection.cursor()
    casecur.execute(' '.join(sql))
    
    sql = ["SELECT name, value, idx from casevars WHERE case_id=%s"]
    vars_added = False
    for i,name in enumerate(varnames):
        if i==0:
            sql.append("AND (")
        else:
            sql.append("OR")
        sql.append("name='%s'" % name)
        vars_added = True
    if vars_added: sql.append(")")
    
    if var_sql:
        sql.append(" AND %s" % var_sql)
    combined = ' '.join(sql)
    
    varcur = connection.cursor()
    
    vardict = {}
    for name in varnames:
        vardict[name] = []

    for case_id in casecur:
        casedict = {}
        varcur.execute(combined % case_id)
        for vname, value, idx in varcur:
            if not isinstance(value, (float,int,str)):
                try:
                    value = loads(str(value))
                except UnpicklingError as err:
                    raise UnpicklingError("can't unpickle value '%s' from database: %s" %
                                          (vname, str(err)))
            if idx:
                vname = "%s%s" % (vname,idx)
            casedict[vname] = value
        
        if len(casedict) != len(varnames):
            continue   # case doesn't contain a complete set of specified vars, so skip it to avoid data mismatches
        
        for name, value in casedict.items():
            vardict[name].append(value)
            
    return vardict


def _get_lines(dbname, xnames, ynames, case_sql=None, var_sql=None): 
    """Return a list of lines which will be fed to the plot function."""
    
    vardict = case_db_to_dict(dbname, xnames+ynames, case_sql, var_sql)

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


def displayXY(dbname, xnames, ynames, case_sql=None, var_sql=None,
              title='', grid=False, xlabel='', ylabel=''):
    """Display an XY plot using Case data from a sqlite DB.
    
    dbname: str
        Name of the database file.
        
    xnames: list[str]
        Names of X variables.
        
    ynames: list[str]
        Names of Y variables.
        
    case_sql: str, optional
        SQL syntax that will be placed in the WHERE clause for Case retrieval.
        
    var_sql: str, optional
        SQL syntax that will be placed in the WHERE clause for variable retrieval.
        
    title: str, optional
        Plot title.
        
    grid: bool, optional
        If True, a grid is drawn on the plot.
        
    xlabel: str, optional
        X axis label.
        
    ylabel: str, optional
        Y axis label.
    """
    try:
        if 'matplotlib' not in sys.modules:
            import matplotlib
            if sys.platform == 'darwin':
                matplotlib.use('MacOSX')
            else:
                try:
                    import wx
                except ImportError:
                    matplotlib.use('TkAgg')
                else:
                    matplotlib.use('WxAgg')
              
        import matplotlib.pyplot as plt
    except ImportError:
        print 'matplotlib not found'
        return
    
    fig = plt.figure()
    fig.add_subplot(111)
    
    for i,line in enumerate(_get_lines(dbname, xnames, ynames, case_sql, var_sql)):
        args = []
        kwargs = {}
        args.append(line[0])
        args.append(line[1])
        kwargs['label'] = '%s' % ynames[i]
        plt.plot(*args, **kwargs)
    if grid:
        plt.grid(True)
    if xlabel:
        plt.xlabel(xlabel)
    if ylabel:
        plt.ylabel(ylabel)
    if title:
        plt.title(title)
    plt.legend()
    plt.show()

def cmdlineXYplot():
    """Based on command line options, display an XY plot using data from a 
    sqlite Case DB.
    """
    parser = OptionParser()
    parser.add_option("-x", "", action="store", type="string", dest="xnames",
                      help="names of x variables")
    parser.add_option("-y", "", action="store", type="string", dest="ynames",
                      help="names of y variables")
    parser.add_option("-d", "--dbfile", action="store", type="string", dest="dbname",
                      help="database filename")
    parser.add_option("-t", "--title", action="store", type="string", dest="title",
                      help="plot title",)
    parser.add_option("", "--xlabel", action="store", type="string", dest="xlabel",
                      help="x axis label")
    parser.add_option("", "--ylabel", action="store", type="string", dest="ylabel",
                      help="y axis label")
    parser.add_option("-g", "--grid", action="store_true", dest="grid",
                      help="makes grid visible")
    parser.add_option("", "--cases", action="store", type="string", dest="case_sql",
                      help="sql syntax to select certain cases")
    parser.add_option("", "--vars", action="store", type="string", dest="var_sql",
                      help="sql syntax to select certain vars")
    parser.add_option("-l", "--list", action="store_true", dest="listvars",
                      help="lists names of variables found in the database")

    (options, args) = parser.parse_args(sys.argv[1:])
    
    if options.listvars:
        print
        for name in sorted(list_db_vars(options.dbname)):
            print name
        print
        sys.exit(0)

    if len(args) > 0 or not options.ynames or not options.dbname:
        parser.print_help()
        sys.exit(-1)
    
    if options.xnames:
        xs = options.xnames.split(',')
    else:
        xs = []
    ys = options.ynames.split(',')
    
    if len(xs) > 1 and len(xs) != len(ys):
        print "Number of x variables doesn't match number of y variables."
        sys.exit(-1)
    
    displayXY(options.dbname, xs, ys, options.case_sql, options.var_sql, 
              title=options.title, grid=options.grid, xlabel=options.xlabel,
              ylabel=options.ylabel)
