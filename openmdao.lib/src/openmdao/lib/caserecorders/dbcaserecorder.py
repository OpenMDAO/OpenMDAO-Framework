
import sqlite3
from cPickle import dumps, HIGHEST_PROTOCOL

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
         case_id INTEGER PRIMARY KEY,
         name TEXT,
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
         entry TEXT
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
        
        cur.execute("""insert into cases(case_id,name,msg,retries,model_id,timeEnter) 
                           values (?,?,?,?,?,DATETIME('NOW'))""", 
                                     (None, case.ident, case.msg, case.retries, self.model_id))
        case_id = cur.lastrowid
        # insert the inputs and outputs into the vars table.  Pickle them if they're not one of the
        # built-in types int, float, or str.
        vlist = []
        for name,entry,value in case.inputs:
            if isinstance(value, (float,int,str)):
                vlist.append((None, name, case_id, 'i', value, entry))
            else:
                vlist.append((None, name, case_id, 'i', sqlite3.Binary(dumps(value,HIGHEST_PROTOCOL)), entry))
        for name,entry,value in case.outputs:
            if isinstance(value, (float,int,str)):
                vlist.append((None, name, case_id, 'o', value, entry))
            else:
                vlist.append((None, name, case_id, 'o', sqlite3.Binary(dumps(value,HIGHEST_PROTOCOL)), entry))
        for v in vlist:
            cur.execute("insert into casevars(var_id,name,case_id,sense,value,entry) values(?,?,?,?,?,?)", 
                            v)
        self._connection.commit()

    def get_iterator(self):
        """Return a DBCaseIterator that points to our current DB."""
        return DBCaseIterator(dbfile=self._dbfile, connection=self._connection)

    