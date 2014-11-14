"""A CaseRecorder and CaseIterator that store the cases in a relational
DB (Python's sqlite.)
"""

import sys

try:
    import sqlite3
except ImportError:
    import logging
    logging.error('No sqlite3 support for DBCaseIterator or DBCaseRecorder')

import time
from cPickle import dumps, loads, HIGHEST_PROTOCOL, UnpicklingError
from optparse import OptionParser

from traits.trait_handlers import TraitListObject, TraitDictObject

# pylint: disable=E0611,F0401
from openmdao.main.interfaces import implements, ICaseRecorder, ICaseIterator
from openmdao.main.case import Case

from openmdao.lib.casehandlers.util import driver_map


_casetable_attrs = set(['id', 'uuid', 'parent', 'msg', 'model_id', 'timeEnter'])
_vartable_attrs = set(['var_id', 'name', 'case_id', 'sense', 'value'])

def _query_split(query):
    """Return a tuple of lhs, relation, rhs after splitting on
    a list of allowed operators.
    """
    # FIXME: make this more robust
    for op in ['<>', '<=', '>=', '==', '!=', '<', '>', '=']:
        parts = query.split(op, 1)
        if len(parts) > 1:
            return (parts[0].strip(), op, parts[1].strip())
    raise ValueError("No allowable operator found in query '%s'" % query)


class DBCaseIterator(object):
    """Pulls Cases from a relational DB (sqlite). It doesn't support
    general sql queries, but it does allow for a series of boolean
    selectors, e.g., 'x<=y', that are ANDed together.
    """

    implements(ICaseIterator)

    def __init__(self, dbfile=':memory:', selectors=None, connection=None):
        if connection is not None:
            self._dbfile = dbfile
            self._connection = connection
        else:
            self._connection = None
            self.dbfile = dbfile
        self.selectors = selectors
        self._connection.text_factory = sqlite3.OptimizedUnicode

    @property
    def dbfile(self):
        """The name of the database. This can be a filename or :memory: for
        an in-memory database.
        """
        return self._dbfile

    @dbfile.setter
    def dbfile(self, value):
        """Set the DB file and connect to it."""
        self._dbfile = value
        if self._connection:
            self._connection.close()
        self._connection = sqlite3.connect(value)

    def __iter__(self):
        return self._next_case()

    def _next_case(self):
        """ Generator which returns Cases one at a time. """
        # figure out which selectors are for cases and which are for variables
        sql = ["SELECT * FROM cases"]
        if self.selectors is not None:
            for sel in self.selectors:
                rhs, rel, lhs = _query_split(sel)
                if rhs in _casetable_attrs:
                    if len(sql) == 1:
                        sql.append("WHERE %s%s%s" % (rhs, rel, lhs))
                    else:
                        sql.append("AND %s%s%s" % (rhs, rel, lhs))

        casecur = self._connection.cursor()
        casecur.execute(' '.join(sql))

        sql = ['SELECT var_id,name,case_id,sense,value from casevars WHERE case_id=%s']
        if self.selectors is not None:
            for sel in self.selectors:
                rhs, rel, lhs = _query_split(sel)
                if rhs in _vartable_attrs:
                    sql.append("AND %s%s%s" % (rhs, rel, lhs))
        combined = ' '.join(sql)
        varcur = self._connection.cursor()

        for cid, text_id, parent, msg, model_id, timeEnter in casecur:
            varcur.execute(combined % cid)
            inputs = []
            outputs = []
            for var_id, vname, case_id, sense, value in varcur:
                if isinstance(value, (float, int, str)):
                    pass
                elif value is None:  # Result when recorded value was NaN.
                    value = float('NaN')
                else:
                    try:
                        value = loads(str(value))
                    except UnpicklingError as err:
                        print 'value', type(value), repr(value)
                        raise UnpicklingError("can't unpickle value '%s' for"
                                              " case '%s' from database: %s"
                                              % (vname, text_id, str(err)))
                if sense == 'i':
                    inputs.append((vname, value))
                elif sense == 'o':
                    outputs.append((vname, value))
            if len(inputs) > 0 or len(outputs) > 0:
                exc = Exception(msg) if msg else None
                yield Case(inputs=inputs, outputs=outputs, exc=exc,
                           case_uuid=text_id, parent_uuid=parent)


class DBCaseRecorder(object):
    """Records Cases to a relational DB (sqlite). Values other than floats,
    ints or strings are pickled and are opaque to SQL queries.
    """

    implements(ICaseRecorder)

    def __init__(self, dbfile=':memory:', model_id='', append=False):
        self.dbfile = dbfile  # this creates the connection
        self.model_id = model_id
        self._cfg_map = {}

        if append:
            exstr = 'if not exists'
        else:
            exstr = ''

        self._connection.execute("""
        create table %s cases(
         id INTEGER PRIMARY KEY,
         uuid TEXT,
         parent TEXT,
         msg TEXT,
         model_id TEXT,
         timeEnter TEXT
         )""" % exstr)

        self._connection.execute("""
        create table %s casevars(
         var_id INTEGER PRIMARY KEY,
         name TEXT,
         case_id INTEGER,
         sense TEXT,
         value BLOB
         )""" % exstr)

    @property
    def dbfile(self):
        """The name of the database. This can be a filename or :memory: for
        an in-memory database.
        """
        return self._dbfile

    @dbfile.setter
    def dbfile(self, value):
        """Set the DB file and connect to it."""
        self._dbfile = value
        self._connection = sqlite3.connect(value)
        self._iter_conn = sqlite3.connect(value)

    def startup(self):
        """ Opens the database for recording."""
        pass

    def register(self, driver, inputs, outputs):
        """Register names for later record call from `driver`."""
        self._cfg_map[driver] = driver_map(driver, inputs, outputs)

    def record_constants(self, constants):
        """Record constant data - currently ignored."""
        pass

    def record(self, driver, inputs, outputs, exc, case_uuid, parent_uuid):
        """Record the given run data."""
        if self._connection is None:
            raise RuntimeError('Attempt to record on closed recorder')

        cur = self._connection.cursor()

        msg = '' if exc is None else str(exc)
        cur.execute("""insert into cases(id,uuid,parent,msg,model_id,timeEnter)
                           values (?,?,?,?,?,DATETIME('NOW'))""",
                    (None, case_uuid, parent_uuid, msg, self.model_id))

        case_id = cur.lastrowid
        # insert the inputs and outputs into the vars table.  Pickle them if
        # they're not one of the built-in types int, float, or str.

        v = (None, 'timestamp', case_id, None, time.time())
        cur.execute("insert into casevars(var_id,name,case_id,sense,value) values(?,?,?,?,?)",
                    v)

        in_names, out_names = self._cfg_map[driver]

        for name, value in zip(in_names, inputs):
            if isinstance(value, (float, int, str)):
                v = (None, name, case_id, 'i', value)
            else:
                if isinstance(value, TraitDictObject):
                    value = dict(value)
                elif isinstance(value, TraitListObject):
                    value = list(value)
                v = (None, name, case_id, 'i',
                     sqlite3.Binary(dumps(value, HIGHEST_PROTOCOL)))
            cur.execute("insert into casevars(var_id,name,case_id,sense,value) values(?,?,?,?,?)",
                        v)
        for name, value in zip(out_names, outputs):
            if isinstance(value, (float, int, str)):
                v = (None, name, case_id, 'o', value)
            else:
                if isinstance(value, TraitDictObject):
                    value = dict(value)
                elif isinstance(value, TraitListObject):
                    value = list(value)
                v = (None, name, case_id, 'o',
                     sqlite3.Binary(dumps(value, HIGHEST_PROTOCOL)))
            cur.execute("insert into casevars(var_id,name,case_id,sense,value) values(?,?,?,?,?)",
                        v)
        self._connection.commit()

    def close(self):
        """Commit and close DB connection if not using ``:memory:``."""
        if self._connection is not None and self._dbfile != ':memory:':
            self._connection.commit()
            self._connection.close()
            self._connection = None

    def get_iterator(self):
        """Return a DBCaseIterator that points to our current DB."""
        return DBCaseIterator(dbfile=self._dbfile, connection=self._connection)


"""
Utility functions related to plotting data
"""

def list_db_vars(dbname):
    """
    Return the set of the names of the variables found in the specified case DB
    file.

    dbname: str
        The name of the sqlite DB file.
    """
    connection = sqlite3.connect(dbname)
    varcur = connection.cursor()
    varcur.execute("SELECT name from casevars")
    varnames = set([v for v in varcur])
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
        Iterator of names of variables to be retrieved.

    case_sql: str (optional)
        SQL syntax that will be placed in the WHERE clause for Case retrieval.

    var_sql: str (optional)
        SQL syntax that will be placed in the WHERE clause for variable retrieval.

    include_errors: bool (optional) [False]
        If True, include data from cases that reported an error.

    """
    connection = sqlite3.connect(dbname)
    vardict = dict([(name, []) for name in varnames])

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

    sql = ["SELECT name, value from casevars WHERE case_id=%s"]
    vars_added = False
    for i, name in enumerate(vardict.keys()):
        if i == 0:
            sql.append("AND (")
        else:
            sql.append("OR")
        sql.append("name='%s'" % name)
        vars_added = True
    if vars_added:
        sql.append(")")

    if var_sql:
        sql.append(" AND %s" % var_sql)
    combined = ' '.join(sql)

    varcur = connection.cursor()

    for case_id in casecur:
        casedict = {}
        varcur.execute(combined % case_id)
        for vname, value in varcur:
            if not isinstance(value, (float, int, str)):
                try:
                    value = loads(str(value))
                except UnpicklingError as err:
                    raise UnpicklingError("can't unpickle value '%s' from"
                                          " database: %s" % (vname, str(err)))
            casedict[vname] = value

        if len(casedict) != len(vardict):
            continue   # case doesn't contain a complete set of specified vars,
                       # so skip it to avoid data mismatches

        for name, value in casedict.items():
            vardict[name].append(value)

    return vardict


def _get_lines(dbname, xnames, ynames, case_sql=None, var_sql=None):
    """Return a list of lines which will be fed to the plot function."""

    vardict = case_db_to_dict(dbname, xnames+ynames, case_sql, var_sql)

    lines = []
    yvals = []
    xvals = []
    for i, name in enumerate(ynames):
        yvals.append(vardict[name])
        if len(xnames) == 0:
            xvals.append(range(len(vardict[name])))
        elif len(xnames) == 1:
            xvals.append(vardict[xnames[0]])
        else:
            xvals.append(vardict[xnames[i]])

    for xdata, ydata in zip(xvals, yvals):
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

    case_sql: str (optional)
        SQL syntax that will be placed in the WHERE clause for Case retrieval.

    var_sql: str (optional)
        SQL syntax that will be placed in the WHERE clause for variable retrieval.

    title: str (optional)
        Plot title.

    grid: bool (optional)
        If True, a grid is drawn on the plot.

    xlabel: str (optional)
        X axis label.

    ylabel: str (optional)
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

    for i, line in enumerate(_get_lines(dbname, xnames, ynames, case_sql, var_sql)):
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
    parser.add_option("--xlabel", action="store", type="string", dest="xlabel",
                      help="x axis label")
    parser.add_option("--ylabel", action="store", type="string", dest="ylabel",
                      help="y axis label")
    parser.add_option("-g", "--grid", action="store_true", dest="grid",
                      help="makes grid visible")
    parser.add_option("--cases", action="store", type="string", dest="case_sql",
                      help="sql syntax to select certain cases")
    parser.add_option("--vars", action="store", type="string", dest="var_sql",
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
