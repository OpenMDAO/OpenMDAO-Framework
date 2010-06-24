
import sqlite3

from enthought.traits.api import implements

from openmdao.main.interfaces import ICaseRecorder

class DBCaseRecorder(object):
    """"Records Cases to a relational DB (sqlite)"""
    
    implements(ICaseRecorder)
    
    def __init__(self, dbfile=':memory:', model_id=''):
        self.dbfile = dbfile  # this creates the connection
        self.model_id = model_id
        
        self._connection.execute("""
        create table if not exists cases(
         case_id INTEGER PRIMARY KEY,
         name TEXT,
         msg TEXT,
         retries INTEGER,
         model_id TEXT,
         timeEnter TEXT
         )""")
        
        self._connection.execute("""
        create table if not exists casevars(
         var_id INTEGER PRIMARY KEY,
         name TEXT,
         case_id INTEGER,
         sense TEXT,
         value NUMERIC,
         entry TEXT
         )""")

    @property
    def dbfile(self):
        return self._dbfile
    
    @dbfile.setter
    def dbfile(self, value):
        """Set the DB file and connect to it."""
        self._dbfile = value
        self._connection = sqlite3.connect(value)
    
    def record(self, case):
        """Record the given Case"""
        cur = self._connection.cursor()
        
        cur.execute("""insert into cases(case_id,name,msg,retries,model_id,timeEnter) 
                           values (?,?,?,?,?,DATETIME('NOW'))""", 
                                     (None, case.ident, case.msg, case.retries, self.model_id))
        case_id = cur.lastrowid
        # insert the inputs and outputs into the vars table
        vlist = [(None, name, case_id, 'i', value, entry) for name,entry,value in case.inputs]
        vlist.extend([(None, name, case_id, 'o', value, entry) for name,entry,value in case.outputs])
        cur.executemany("insert into casevars(var_id,name,case_id,sense,value,entry) values(?,?,?,?,?,?)", 
                        vlist)
        self._connection.commit()
