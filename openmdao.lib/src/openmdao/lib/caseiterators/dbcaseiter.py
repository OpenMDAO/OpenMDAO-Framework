
import sqlite3

from enthought.traits.api import implements

from openmdao.main.interfaces import ICaseIterator
from openmdao.main.api import Case

def _all_cases(Case):
    return True

class DBCaseIterator(object):
    """"Pulls Cases from a relational DB (sqlite)"""
    
    implements(ICaseIterator)
    
    def __init__(self, dbfile=':memory:', case_selector='', pred=None):
        self._dbfile = dbfile
        self.case_selector = case_selector # where clause for case table
        if pred is None:
            self.pred = _all_cases
        else:
            self.pred = pred  # function that takes a Case arg and returns True if Case should be used
        self.connection = None

    def __iter__(self):
        if self.connection:
            self.connection.close()
        self.connection = sqlite3.connect(self._dbfile)
        return self._next_case()

    def _next_case(self):
        """ Generator which returns Cases one at a time. """
        cur = self.connection.cursor()
        
        sql = ["SELECT * FROM cases"]
        if self.case_selector:
            sql.append("WHERE %s" % self.case_selector)
        cur.execute(' '.join(sql))
        
        for result in cur:
            print result


