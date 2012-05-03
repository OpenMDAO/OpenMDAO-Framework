''' The Projects object provides a basic interface for interacting with the
project database used by the GUI. '''

import sys
import sqlite3
import os.path
from datetime import datetime

from openmdao.gui.util import ensure_dir, print_dict


def get_user_dir():
    user_dir = os.path.expanduser("~/.openmdao/gui/")
    ensure_dir(user_dir)
    return user_dir


class Projects(object):

    def __init__(self, pathname=None):
        if pathname:
            self._pathname = pathname
        else:
            self._pathname = os.path.join(get_user_dir(), 'projects.db')
        self._connection = None

        self.time_format = "%Y-%m-%d %H:%M:%S"

    def _get_connection(self):
        if self._connection is None:
            self._connection = sqlite3.connect(self._pathname)
        return self._connection

    def exists(self):
        ''' Does the database exist? '''

        return os.path.exists(self._pathname)

    def create(self):
        ''' Create a new clean database for the GUI user. '''

        try:
            con = self._get_connection()
            cur = con.cursor()
            cur.executescript("""
                CREATE TABLE "projects" (
                    "id" integer PRIMARY KEY AUTOINCREMENT NOT NULL,
                    "projectname" varchar(40) NOT NULL,
                    "version" varchar(40) NOT NULL,
                    "description" varchar(200) NOT NULL,
                    "created" datetime NOT NULL,
                    "modified" datetime NOT NULL,
                    "filename" varchar(200) NOT NULL,
                    "active" bool NOT NULL
                );
                """)
            con.commit()
            print "Creating new project database."
        except sqlite3.Error, e:
            print "Error %s:" % e.args[0]
            sys.exit(1)
        finally:
            if con:
                con.close()

    def new(self, data):
        ''' Insert a new row into the project database.

            data: dict
                Dictionary containing all fields for the new entry.
        '''

        data['created'] = str(datetime.now())
        data['modified'] = str(datetime.now())

        con = self._get_connection()
        cur = con.cursor()
        sql = '''INSERT INTO projects
                 (projectname,version,description,created,modified,filename,active)
                 VALUES ("%s","%s","%s","%s","%s","%s",%d)
              ''' % (data['projectname'],
                     data['version'],
                     data['description'],
                     data['created'],
                     data['modified'],
                     data['filename'],
                     data['active'])

        cur.execute(sql)
        project_id = cur.lastrowid
        con.commit()
        cur.close()
        return project_id

    def get(self, project_id):
        ''' Get a dictionary containing the fields for a project id.

            project_id: int
                unique id for requested project.
        '''

        con = self._get_connection()
        con.row_factory = sqlite3.Row
        cur = con.cursor()
        sql = 'SELECT * from projects WHERE id=%d' % int(project_id)

        cur.execute(sql)
        matched_projects = []
        for row in cur:
            matched_projects.append({
                'id': row['id'],
                'projectname': row['projectname'],
                'version':     row['version'],
                'description': row['description'],
                'modified':    row['modified'],
                'created':     row['created'],
                'filename':    row['filename'],
                'active':      row['active']
            })

        cur.close()

        if len(matched_projects) < 1:
            print "Error project ID not found:", id

        # This should never happen!
        elif len(matched_projects) > 1:
            print "Error: Non-unique project ID:"
            print_dict(matched_projects)
        else:
            return matched_projects[0]

    def get_by_filename(self, filename):
        ''' Get a dictionary containing the fields that belong to
            a project with a specific filename.

            filename: str (valid path)
                filename for requested project
        '''
        con = self._get_connection()
        con.row_factory = sqlite3.Row
        cur = con.cursor()
        sql = 'SELECT * from projects WHERE filename=?'

        cur.execute(sql, (filename,))
        matched_projects = []
        for row in cur:
            matched_projects.append({
                'id': row['id'],
                'projectname': row['projectname'],
                'version':     row['version'],
                'description': row['description'],
                'modified':    row['modified'],
                'filename':    row['filename'],
                'active':      row['active']
            })

        cur.close()

        if len(matched_projects) < 1:
            print "Error project ID not found:", id

        # This should never happen!
        elif len(matched_projects) > 1:
            print "Error: Non-unique project ID:"
            print_dict(matched_projects)
        else:
            return matched_projects[0]

    def predict_next_rowid(self):
        ''' Predict what the next auto-inserted rowid will be.
        This is here because the GUI handlers need to know the
        project_id even before the row is inserted.'''

        con = self._get_connection()
        con.row_factory = sqlite3.Row
        cur = con.cursor()
        sql = "SELECT * FROM SQLITE_SEQUENCE WHERE name='projects'"
        cur.execute(sql)
        row = cur.fetchone()
        try:
            next_id = row['seq'] + 1
        except TypeError:
            next_id = 1
        cur.close()
        return next_id

    def list_projects(self):
        ''' Return a list of dictionaries for all projects owned by the
        user. Each dictionary contains all fields for that project id.'''

        con = self._get_connection()
        con.row_factory = sqlite3.Row
        cur = con.cursor()
        sql = 'SELECT * from projects ORDER BY projectname'

        cur.execute(sql)

        matched_projects = []
        for row in cur:
            matched_projects.append({
                'id': row['id'],
                'projectname': row['projectname'],
                'version':     row['version'],
                'description': row['description'],
                'created':     row['created'],
                'modified':    row['modified'],
                'filename':    row['filename'],
                'active':      row['active']
            })

        cur.close()

        # Return last file modification dates too.
        for project in matched_projects:
            fullpath = os.path.join(get_user_dir(), 'projects', project['filename'])
            try:
                stamp = os.path.getmtime(fullpath)
                project['file_modified'] = datetime.fromtimestamp(stamp).strftime(self.time_format)
            except Exception, err:
                project['file_modified'] = err

        return matched_projects

    def set(self, project_id, field, value):
        ''' Set a single field in the project db

        project_id: int
            unique id for requested project.

        field: str
            Name of field to set

        value: various
            Value of field to set
        '''

        con = self._get_connection()
        cur = con.cursor()
        sql = 'UPDATE projects SET %s=? WHERE id=?' % field

        cur.execute(sql, ([value, int(project_id)]))
        con.commit()
        cur.close()

    def modified(self, project_id):
        ''' Update metadate modification time-stamp for project_id, setting
        'modified' to the current time/date.

        project_id: int
            unique id for requested project.
        '''

        modified = str(datetime.now())
        self.set(project_id, 'modified', modified)

    def remove(self, project_id):
        ''' Remove a project from the database

        project_id: int
            unique id for requested project.
        '''

        con = self._get_connection()
        cur = con.cursor()
        sql = 'DELETE from projects WHERE id=?'

        cur.execute(sql, (project_id,))
        con.commit()
        cur.close()


if __name__ == "__main__":
    pass
