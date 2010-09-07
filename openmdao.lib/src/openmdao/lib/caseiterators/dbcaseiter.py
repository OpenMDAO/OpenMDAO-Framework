
import sqlite3
from cPickle import loads, UnpicklingError

from enthought.traits.api import implements

from openmdao.main.interfaces import ICaseIterator
from openmdao.main.api import Case

_casetable_attrs = set(['id','cname','msg','retries','model_id','timeEnter'])
_vartable_attrs = set(['var_id','name','case_id','sense','value','idx'])

def _query_split(query):
    """Return a tuple of lhs, relation, rhs after splitting on 
    a list of allowed operators.
    """
    # FIXME: make this more robust
    for op in ['<>', '<=', '>=', '==', '!=', '<', '>', '=']:
        parts = query.split(op, 1)
        if len(parts) > 1:
            return (parts[0].strip(), op, parts[1].strip())
    else:
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
                rhs,rel,lhs = _query_split(sel)
                if rhs in _casetable_attrs:
                    if len(sql) == 1:
                        sql.append("WHERE %s%s%s" % (rhs,rel,lhs))
                    else:
                        sql.append("AND %s%s%s" % (rhs,rel,lhs))
            
        casecur = self._connection.cursor()
        casecur.execute(' '.join(sql))
          
        sql = ['SELECT var_id,name,case_id,sense,value,idx from casevars WHERE case_id=%s']
        if self.selectors is not None:
            for sel in self.selectors:
                rhs,rel,lhs = _query_split(sel)
                if rhs in _vartable_attrs:
                    sql.append("AND %s%s%s" % (rhs,rel,lhs))
        combined = ' '.join(sql)
        varcur = self._connection.cursor()
        
        for cid,cname,msg,retries,model_id,timeEnter in casecur:
            varcur.execute(combined % cid)
            inputs = []
            outputs = []
            for var_id, vname, case_id, sense, value, idx in varcur:
                if not isinstance(value, (float,int,str)):
                    try:
                        value = loads(str(value))
                    except UnpicklingError as err:
                        raise UnpicklingError("can't unpickle value '%s' for case '%s' from database: %s" %
                                              (vname, cname, str(err)))
                if sense=='i':
                    inputs.append((vname, idx, value))
                else:
                    outputs.append((vname, idx, value))
            if len(inputs) > 0 or len(outputs) > 0:
                yield Case(inputs=inputs, outputs=outputs,retries=retries,msg=msg,ident=cname)
            
