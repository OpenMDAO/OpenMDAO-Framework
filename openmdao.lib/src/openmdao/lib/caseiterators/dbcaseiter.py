
import sqlite3
from cPickle import loads, UnpicklingError

from enthought.traits.api import implements

from openmdao.main.interfaces import ICaseIterator
from openmdao.main.api import Case

class DBCaseIterator(object):
    """Pulls Cases from a relational DB (sqlite)."""
    
    implements(ICaseIterator)
    
    def __init__(self, dbfile=':memory:', case_selector=None, var_selector=None, connection=None):
        if connection is not None:
            self._dbfile = dbfile
            self._connection = connection
        else:
            self._connection = None
            self.dbfile = dbfile
        self.case_selector = case_selector # WHERE clause for case table
        self.var_selector = var_selector   # WHERE clause for casevars table

    @property
    def dbfile(self):
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
        sql = ["SELECT * FROM cases"]
        if self.case_selector:
            sql.append("WHERE %s" % self.case_selector)
            
        casecur = self._connection.cursor()
        casecur.execute(' '.join(sql))
          
        sql = ['SELECT var_id,name,case_id,sense,value,entry from casevars WHERE case_id=%s']
        if self.var_selector:
            sql.append("AND %s" % self.var_selector)
        combined = ' '.join(sql)
        varcur = self._connection.cursor()
        
        for case_id,name,msg,retries,model_id,timeEnter in casecur:
            varcur.execute(combined % case_id)
            inputs = []
            outputs = []
            for var_id, vname, case_id, sense, value, entry in varcur:
                if not isinstance(value, (float,int,str)):
                    try:
                        value = loads(str(value))
                    except UnpicklingError as err:
                        raise UnpicklingError("can't unpickle value '%s' from database: %s" %
                                              (vname, str(err)))
                if sense=='i':
                    inputs.append((vname, entry, value))
                else:
                    outputs.append((vname, entry, value))
            if len(inputs) > 0 or len(outputs) > 0:
                yield Case(inputs=inputs, outputs=outputs,retries=retries,msg=msg,ident=name)
            
