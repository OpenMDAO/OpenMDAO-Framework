import cStringIO as StringIO
import os.path
import shutil
import tarfile
from tempfile import mkdtemp
from time import strftime
from urllib import quote_plus
from urllib2 import HTTPError

from tornado import web

from openmdao.main import __version__
from openmdao.main.project import parse_archive_name, Project
from openmdao.main.repo import find_vcs, DumbRepo
from openmdao.gui.handlers import ReqHandler
from openmdao.gui.projectdb import Projects
from openmdao.util.fileutil import clean_filename, onerror


def _get_unique_name(dirname, basename):
    """Returns a unique 'clean' pathname with the given basename
    in the specified directory.
    """
    i = 1
    name = clean_filename(basename)
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
    ''' Delete a project.
    '''

    @web.authenticated
    def post(self, project_id):
        pdb = Projects()
        project = pdb.get(project_id)

        if project['projpath']:
            dirname = str(project['projpath'])
            if os.path.isdir(dirname):
                try:
                    shutil.rmtree(dirname, onerror=onerror)
                except Exception as err:
                    raise HTTPError(dirname, 403, err, None, None)
                else:
                    pdb.remove(project_id)
        self.redirect('/')


class DetailHandler(ReqHandler):
    ''' Get/set project details.
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
            project['version'] = ''

        # if there's no proj dir yet, create an empty one
        if not project['projpath']:
            directory = self.get_project_dir()
            pname = project['projectname']
            project['projpath'] = _get_unique_name(directory, pname)

        if project_is_new:
            pdb.new(project)
            os.mkdir(project['projpath'])
        else:
            for key, value in project.iteritems():
                pdb.set(project_id, key, value)
            pdb.modified(project_id)

        # Update project settings.
        proj = Project(project['projpath'])
        dummy = proj.get_info()  # Just to get required keys.
        info = {}
        for key in dummy:
            info[key] = project[key]
        proj.set_info(info)

        self.redirect("/workspace/project?projpath=" + project['projpath'])
        #self.redirect('/')
        #self.redirect(self.request.uri)
        #self.redirect( "/workspace/project?projpath=project['projpath']" )

    @web.authenticated
    def get(self, project_id):
        pdb = Projects()
        project = pdb.get(project_id)
        self.render('projdb/project_detail.html', project=project,
        delete=True)


class DownloadHandler(ReqHandler):
    ''' Download a copy of the project.
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
                        shutil.rmtree(tdir, onerror=onerror)
                    except:
                        pass
            else:
                raise HTTPError(dirname, 403, "%s is not a directory" % dirname,
                                None, None)
        else:
            raise HTTPError(filename, 403, "no file found for %s" %
                                            project['projectname'], None, None)


class NewHandler(ReqHandler):
    ''' Add a project to the project database. This extracts the project file
    into a directory under the project's directory of users.
    '''

    @web.authenticated
    def post(self):

        pdb = Projects()

        forms = {}
        for field in ['projectname', 'description', 'version']:
            if field in self.request.arguments.keys():
                forms[field] = self.request.arguments[field][0]

        project = {}
        project['projectname'] = forms['projectname'].strip()
        project['description'] = forms['description'].strip()
        project['version'] = forms['version'].strip()
        project['id'] = pdb.predict_next_rowid()
        project['active'] = 1

        # figure out a unique directory name for the project using
        #   the project name and version string
        directory = self.get_project_dir()
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
            i = i + 1

        project['projpath'] = os.path.join(directory, unique)

        pdb.new(project)
        os.mkdir(project['projpath'])

        # Update project settings.
        proj = Project(project['projpath'])
        dummy = proj.get_info()  # Just to get required keys.
        info = {}
        for key in dummy:
            info[key] = project[key]
        proj.set_info(info)

        self.redirect("/workspace/project?projpath=" + quote_plus(project['projpath']))


class ImportHandler(ReqHandler):
    ''' Get/set project details.
    '''

    @web.authenticated
    def get(self):
        self.render('projdb/import-metadata-fields.html',
                    projectname='someproject')

    @web.authenticated
    def post(self):
        # The project file is uploaded once to extract the metadata.
        # It is then deleted and the metadata is used to populate another
        # import dialog, giving the user an opportunity to edit the
        # info before importing or cancel the import.
        if not 'projectname' in self.request.arguments:
            # First upload
            sourcefile = self.request.files['projectfile'][0]
            if sourcefile:
                filename = sourcefile['filename']
                if len(filename) > 0:
                    unique = _get_unique_name(self.get_project_dir(),
                                              parse_archive_name(filename))
                    tdir = mkdtemp(prefix=unique)
                    buff = StringIO.StringIO(sourcefile['body'])
                    archive = tarfile.open(fileobj=buff, mode='r:gz')
                    archive.extractall(path=tdir)
                    proj = Project(tdir)
                    project_info = proj.get_info()

                    try:
                        shutil.rmtree(tdir, onerror=onerror)
                    except:
                        pass

                    self.render('projdb/import-metadata-fields.html',
                                projectname=parse_archive_name(unique),
                                description=project_info['description'],
                                version=project_info['version'])
        else:
            # second upload
            forms = {}
            for field in ['projectname', 'description', 'version']:
                if field in self.request.arguments.keys():
                    forms[field] = self.request.arguments[field][0]

            sourcefile = self.request.files['projectfile'][0]
            if sourcefile:
                filename = sourcefile['filename']
                if len(filename) > 0:

                    unique = _get_unique_name(self.get_project_dir(),
                                              parse_archive_name(filename))

                    pdb = Projects()

                    project = {}
                    project['id'] = pdb.predict_next_rowid()
                    project['active'] = 1
                    project['projectname'] = forms['projectname'].strip()
                    project['description'] = forms['description'].strip()
                    project['version'] = forms['version'].strip()
                    project['projpath'] = unique

                    os.mkdir(unique)

                    buff = StringIO.StringIO(sourcefile['body'])

                    archive = tarfile.open(fileobj=buff, mode='r:gz')
                    archive.extractall(path=unique)

                    vcslist = find_vcs()
                    if vcslist:
                        vcs = vcslist[0](unique)
                    else:
                        vcs = DumbRepo(unique)
                    vcs.init_repo()

                    # Update project settings.
                    proj = Project(project['projpath'])
                    dummy = proj.get_info()  # Just to get required keys.
                    info = {}
                    for key in dummy:
                        info[key] = project[key]
                    proj.set_info(info)

                    pdb.new(project)

                    self.redirect("/workspace/project?projpath=" + quote_plus(project['projpath']))

        self.redirect("/")


handlers = [
    web.url(r'/projects/?',                              IndexHandler),
    web.url(r'/projects/(?P<project_id>\d+)/?',          DetailHandler),
    web.url(r'/projects/new/$',                          NewHandler),
    web.url(r'/projects/import/$',                       ImportHandler),
    web.url(r'/projects/delete/(?P<project_id>\d+)/?',   DeleteHandler),
    web.url(r'/projects/download/(?P<project_id>\d+)/?', DownloadHandler),
]
