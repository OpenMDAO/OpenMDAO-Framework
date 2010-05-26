
import sqlite3


class CaseDBRecorder(object):
    """"Records Cases to a relational DB"""
    
    def __init__(self, dbfile=':memory:'):
        self.dbfile = dbfile
        self.tags = []
        self.connection = sqlite3.connect(dbfile)
        self.connection.execute("""
        create table cases(
         case_id INTEGER PRIMARY KEY,
         name TEXT,
         msg TEXT,
         retries INTEGER,
         model_id INTEGER,
         tags TEXT
         timeEnter TEXT,
         )""")
        
        self.connection.execute("""
        create table vars(
         var_id INTEGER PRIMARY KEY,
         name TEXT,
         case_id INTEGER,
         sense, TEXT,
         value NUMERIC,
         entry TEXT
         )""")

    def record(self, case):
        """Record the given Case"""
        con = self.connection
        
        con.execute("insert into cases(case_id,name,msg,when,retries,model_id,tags) values (?,?,?,?,?,?,?)", 
                    (None, case.name, case.msg, ))
        # insert the inputs and outputs into the vars table
        vlist = [(None, name, case_id, 'i', value, entry) for name,entry,value in case.inputs]
        vlist.extend([(None, name, case_id, 'o', value, entry) for name,entry,value in case.outputs])
        con.executemany("insert into vars(var_id,name,case_id,sense,value,entry) values(?,?,?,?,?,?)", 
                        vlist)
        sql = []
