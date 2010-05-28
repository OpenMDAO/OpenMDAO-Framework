
import sqlite3

from enthought.traits.api import implements

from openmdao.main.interfaces import ICaseIterator
from openmdao.main.api import Case

class DBCaseIterator(object):
    """"Pulls Cases from a relational DB (sqlite)"""
    
    implements(ICaseIterator)
    
    def __init__(self, dbfile=':memory:', case_selector=None, var_selector=None):
        self._dbfile = dbfile
        self.case_selector = case_selector # WHERE clause for case table
        self.var_selector = var_selector   # WHERE clause for casevars table
        self.connection = None

    def __iter__(self):
        if not self.connection:
            self.connection = sqlite3.connect(self._dbfile)
        return self._next_case()

    def _next_case(self):
        """ Generator which returns Cases one at a time. """
        sql = ["SELECT * FROM cases"]
        if self.case_selector:
            sql.append("WHERE %s" % self.case_selector)
            
        casecur = self.connection.cursor()
        casecur.execute(' '.join(sql))
        
        sql = ["SELECT * from casevars WHERE case_id=%s"]
        if self.var_selector:
            sql.append("AND %s" % self.var_selector)
        combined = ' '.join(sql)
        varcur = self.connection.cursor()
        
        for case_id,name,msg,retries,model_id,timeEnter in casecur:
            varcur.execute(combined % case_id)
            inputs = []
            outputs = []
            for var_id, vname, case_id, sense, value, entry in varcur:
                if sense=='i':
                    inputs.append((vname, entry, value))
                else:
                    outputs.append((vname, entry, value))
            yield Case(inputs=inputs, outputs=outputs,retries=retries,msg=msg,ident=name)

            