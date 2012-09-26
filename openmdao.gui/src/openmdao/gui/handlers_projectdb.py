import os
import shutil
from time import strftime
from urllib2 import HTTPError
import cStringIO as StringIO
import tarfile
from tempfile import mkdtemp

from tornado import web

from openmdao.main import __version__
from openmdao.gui.handlers import ReqHandler
from openmdao.gui.projectdb import Projects
from openmdao.util.fileutil import clean_filename
from openmdao.util.log import logger
from openmdao.main.project import parse_archive_name, Project
from openmdao.main.repo import find_vcs

def _get_unique_name(dirname, basename):
    """Returns a unique pathname for a file with the given basename
    in the specified directory.
    """
    i = 1
    name = basename
    while os.path.exists(os.path.join(dirname, name)):
        name = '%s_%d' % (basename, i)
        i += 1
    return os.path.join(dirname, name)

class IndexHandler(ReqHandler):
    ''' get project list
    '''

    @web.authenticated
    def get(self):
        pdb = Projects()
        project_list = pdb.list_projects()
        self.render('projdb/project_list.html',
                     project_list=project_list,
                     version=__version__)


class DeleteHandler(ReqHandler):
    ''' delete a project
    '''

    @web.authenticated
    def post(self, project_id):
        pdb = Projects()
        project = pdb.get(project_id)

        if project['projpath']:
            dirname = str(project['projpath'])
            if os.path.isdir(dirname):
                shutil.rmtree(dirname)

        pdb.remove(project_id)
        self.redirect('/')

    @web.authenticated
    def get(self, project_id):
        self.redirect('/')


class DetailHandler(ReqHandler):
    ''' get/set project details
    '''

    @web.authenticated
    def post(self, project_id):
        forms = {}
        for field in ['projectname', 'description', 'version']:
            if field in self.request.arguments.keys():
                forms[field] = self.request.arguments[field][0]

        pdb = Projects()

        # Existing project.
        if int(project_id) != pdb.predict_next_rowid():
            project = pdb.get(project_id)
            project_is_new = False
        # New project
        else:
            project = {}
            project['active'] = 0
            project['projpath'] = None
            project_is_new = True

        if 'projectname' not in forms or \
           len(forms['projectname']) == 0:
            project['projectname'] = "Unnamed Project"
        else:
            project['projectname'] = forms['projectname'].strip()

        if 'description' in forms:
            project['description'] = forms['description'].strip()
        else:
            project['description'] = ''

        if 'version' in forms:
            project['version'] = forms['version'].strip()
        else:
            project['version'] =''
            
        directory = forms.get('directory', self.get_project_dir())

        # if there's no proj dir yet, create an empty one
        if not project['projpath']:

            version = project['version']
            pname = project['projectname']

            if len(version):
                filename = clean_filename('%s-%s' % (pname, version))
            else:
                filename = clean_filename(pname)

            unique = filename
            i = 1
            while os.path.exists(os.path.join(directory, unique)):
                unique = '%s_%s' % (filename, str(i))
                i = i+1

            project['projpath'] = os.path.join(directory, unique)

        if project_is_new:
            pdb.new(project)
            os.mkdir(project['projpath'])
        else:
            for key, value in project.iteritems():
                pdb.set(project_id, key, value)
            pdb.modified(project_id)

        self.redirect(self.request.uri)

    @web.authenticated
    def get(self, project_id):

        pdb = Projects()
        project = pdb.get(project_id)
        self.render('projdb/project_detail.html', project=project,
        delete=True)


class DownloadHandler(ReqHandler):
    ''' download a copy of the project
    '''

    @web.authenticated
    def get(self, project_id):
        ''' Browser download of a project file
        '''
        pdb = Projects()
        project = pdb.get(project_id)
        if project['projpath']:
            dirname = project['projpath']

            if os.path.isdir(dirname):
                proj = Project(dirname)
                tdir = mkdtemp()
                try:
                    filename = proj.export(destdir=tdir)
                    proj_file = open(filename, 'rb')
                    self.set_header('content_type', 'application/octet-stream')
                    self.set_header('Content-Length', str(os.path.getsize(filename)))
                    form_proj = clean_filename(project['projectname'])
                    form_ver = clean_filename(project['version'])
                    form_date = strftime('%Y-%m-%d_%H%M%S')
                    self.set_header('Content-Disposition',
                                    'attachment; filename=%s-%s-%s.proj' %
                                    (form_proj, form_ver, form_date))
    
                    try:
                        self.write(proj_file.read())
                    finally:
                        proj_file.close()
                finally:
                    try:
                        shutil.rmtree(tdir)
                    except:
                        pass
            else:
                raise HTTPError(dirname, 403, "%s is not a directory" % dirname,
                                None, None)
        else:
            raise HTTPError(filename, 403, "no file found for %s" % \
                                            project['projectname'], None, None)


class NewHandler(ReqHandler):
    ''' create a new (empty) project
    '''

    @web.authenticated
    def get(self):
        pdb = Projects()

        project = {}
        project['id'] = pdb.predict_next_rowid()
        project['projectname'] = 'New Project '+strftime("%Y-%m-%d_%H%M%S")
        project['version'] = ''
        project['description'] = ''
        project['created'] = 'New Project'
        project['modified'] = 'New Project'
        project['projpath'] = ''
        project['active'] = ''

        self.render('projdb/project_detail.html', project=project,
                    delete=False)


class AddHandler(ReqHandler):
    ''' Add a project to the project database. This extracts the project file
    into a directory under the users projects directory.
    '''

    @web.authenticated
    def post(self):

        sourcefile = self.request.files['myfile'][0]
        if sourcefile:
            filename = sourcefile['filename']
            if len(filename) > 0:

                unique = _get_unique_name(self.get_project_dir(),
                                          parse_archive_name(filename))
                
                pdb = Projects()

                project = {}
                project['id'] = pdb.predict_next_rowid()
                project['version'] = ''
                project['description'] = ''
                project['active'] = 1
                project['projectname'] = parse_archive_name(unique)
                project['projpath'] = unique

                os.mkdir(unique)
                
                buff = StringIO.StringIO(sourcefile['body'])
                
                archive = tarfile.open(fileobj=buff, mode='r:gz')
                archive.extractall(path=unique)
                
                vcslist = find_vcs()
                if vcslist:
                    vcs = vcslist[0](unique)
                else:
                    vcs = DumbVCS(unique)
                vcs.init_repo()
                
                # TODO: look for project config and retrieve project db info from it
                
                pdb.new(project)

                self.redirect('/projects/'+str(project['id']))

        self.redirect('')

    @web.authenticated
    def get(self):
        self.render('projdb/add_project.html')

handlers = [
    web.url(r'/projects/?',                              IndexHandler),
    web.url(r'/projects/(?P<project_id>\d+)/?',          DetailHandler),
    web.url(r'/projects/new/$',                          NewHandler),
    web.url(r'/projects/add/$',                          AddHandler),
    web.url(r'/projects/delete/(?P<project_id>\d+)/?',   DeleteHandler),
    web.url(r'/projects/download/(?P<project_id>\d+)/?', DownloadHandler),
]
