import sys
import os
import re

try:
    import simplejson as json
except ImportError:
    import json

from tornado import web, escape

from openmdao.gui.handlers import ReqHandler as BaseHandler
from openmdao.gui.projectdb import Projects


class ReqHandler(BaseHandler):
    ''' Render the base template.
    '''

    @web.authenticated
    def post(self):
        ''' Render the base template with the posted content.
        '''
        attributes = {}
        for field in ['head']:
            if field in self.request.arguments.keys():
                attributes[field] = self.request.arguments[field][0]
            else:
                attributes[field] = False
        self.render('workspace/base.html', **attributes)

    @web.authenticated
    def get(self):
        attributes = {}
        for field in ['head_script']:
            if field in self.request.arguments.keys():
                s = self.request.arguments[field][0]
                s = re.sub(r'^"|"$', '', s)  # strip leading/trailing quotes
                s = re.sub(r"^'|'$", "", s)  # strip leading/trailing quotes
                attributes[field] = s
            else:
                attributes[field] = False
        self.render('workspace/base.html', **attributes)


class AddOnsHandler(BaseHandler):
    ''' Add-on installation utility.
    Eventually we will probably wrap the OpenMDAO plugin
    functions to work through here.
    '''
    addons_url = 'http://openmdao.org/dists'

    @web.authenticated
    def post(self):
        ''' Easy_install the POSTed add-on.
        '''
        pass

    @web.authenticated
    def get(self):
        ''' Show available plugins; prompt for plugin to be installed.
        '''
        self.render('workspace/addons.html')


class CloseHandler(ReqHandler):

    @web.authenticated
    def get(self):
        self.delete_server()
        self.redirect('/')


class CommandHandler(ReqHandler):
    ''' POST: execute a command and return the console-like response.
              required arguments are:
                  command - the command to execute
    '''

    @web.authenticated
    def post(self):
        if 'command' not in self.request.arguments.keys():
            self.send_error(400)  # bad request
        else:
            command = self.get_argument('command', default=None)
            result = ''
            try:
                cserver = self.get_server()
                result = cserver.onecmd(command)
            except Exception as exc:
                print exc
                result = sys.exc_info()
            if result:
                result = str(result) + '\n'

            self.content_type = 'text/html'
            self.write(result)


class EditorHandler(ReqHandler):

    @web.authenticated
    def get(self):
        '''Code Editor
        '''
        filename = self.get_argument('filename', default=None)
        self.render('workspace/editor.html', filename=filename)


class FileHandler(ReqHandler):
    ''' GET:    Get the contents of a file.

        PUT:    Write contents to a file.

        DELETE: delete a file.

        POST:   if request contains a 'rename' argument,
                    rename file to the value of rename argument
                otherwise
                    execute the file
    '''

    @web.authenticated
    def get(self, filename):
        cserver = self.get_server()
        self.content_type = 'application/octet-stream'
        download = self.get_argument('download', default=False)
        if download:
            self.set_header('Content-Disposition',
                            'attachment; filename="' + filename + '"')
            self.set_cookie('fileDownload', 'true')  # for jQuery.fileDownload
        self.write(str(cserver.get_file(filename)))

    @web.authenticated
    def put(self, filename):
        cserver = self.get_server()
        isFolder = self.get_argument('isFolder', default=None)
        if isFolder:
            self.write(cserver.ensure_dir(filename))
        else:
            force = int(self.get_argument('force', default=0))
            if not force and cserver.file_forces_reload(filename):
                self.send_error(409)
            else:
                contents = self.get_argument('contents', default='')
                self.write(str(cserver.write_file(filename, contents)))

    @web.authenticated
    def delete(self, filename):
        cserver = self.get_server()
        self.content_type = 'text/html'
        if str(cserver.delete_file(filename)):
            self.set_status(204)  # successful, no data in response
            self.write()

    @web.authenticated
    def post(self, filename):
        cserver = self.get_server()
        result = ''
        try:
            if 'rename' in self.request.arguments.keys():
                newname = self.get_argument('rename')
                cserver.rename_file(filename, newname)
            else:
                result = cserver.execfile(filename)
        except Exception as exc:
            print exc
            result = result + str(sys.exc_info()) + '\n'
        self.content_type = 'text/html'
        self.write(result)


class FilesHandler(ReqHandler):
    ''' GET: get heirarchical list of files.

        DELETE: delete files
                required arguments are:
                    filepaths - the pathnames of the files to delete

    '''

    @web.authenticated
    def get(self):
        cserver = self.get_server()
        filedict = cserver.get_files()
        json_files = json.dumps(filedict)
        self.content_type = 'application/javascript'
        self.write(json_files)

    @web.authenticated
    def delete(self):
        if 'filepaths' not in self.request.arguments.keys():
            self.send_error(400)  # bad request
        else:
            filepaths = escape.json_decode(self.request.body)['filepaths']

            cserver = self.get_server()
            self.content_type = 'text/html'
            success = True
            for filename in filepaths:
                success = success and cserver.delete_file(filename)
            self.write(str(success))


class GeometryHandler(ReqHandler):

    @web.authenticated
    def get(self):
        ''' Geometry viewer.
        '''
        path = self.get_argument('path')
        #self.render('workspace/o3dviewer.html', filename=path)
        if path.startswith('file/'):
            path = path[4:]  # leave the '/' at the beginning of filename
        self.render('workspace/wvclient.html', geom_name=path)


class ObjectHandler(ReqHandler):
    ''' GET:    Get the attributes of an object.
                param is optional and can be one of:
                    'dataflow', 'workflow', 'events', 'passthroughs', 'connections'

        PUT:    create or replace an object.
                required arguments are:
                    type - the type of the new object
                optional arguments are:
                    args - arguments required to create the new object

        POST:   execute an object.

        DELETE: delete an object.
    '''

    @web.authenticated
    def get(self, pathname, param):
        if pathname.lower() == 'none':
            pathname = None
        cserver = self.get_server()
        attr = {}
        for retry in range(3):
            try:
                if param:
                    param = param.lower()
                    if param == 'dataflow':
                        attr = cserver.get_dataflow(pathname)
                    elif param == 'workflow':
                        attr = cserver.get_workflow(pathname)
                    elif param == 'events':
                        attr = cserver.get_available_events(pathname)
                    elif param == 'passthroughs':
                        attr = cserver.get_passthroughs(pathname)
                    elif param == 'connections':
                        source = self.get_argument('source', default=None)
                        target = self.get_argument('target', default=None)
                        attr = cserver.get_connections(pathname, source, target)
                    else:
                        self.send_error(400)  # bad request
                else:
                    attr = cserver.get_attributes(pathname)

                self.content_type = 'application/javascript'
                self.write(attr)
            except AssertionError as exc:
                # Have had issues with `attr` being ZMQ_RPC.invoke args.
                print >>sys.stderr, "ObjectHandler: Can't write %r: %s" \
                                    % (attr, str(exc) or repr(exc))
                if retry >= 2:
                    raise
            else:
                break

    @web.authenticated
    def put(self, pathname, param):
        arg_keys = self.request.arguments.keys()
        if not 'type' in arg_keys:
            self.send_error(400)  # bad request
            return

        type = self.get_argument('type')

        if 'args' in arg_keys:
            args = self.get_argument('args')
        else:
            args = ''

        result = ''
        try:
            cserver = self.get_server()
            cserver.put_object(pathname, type, args)
        except Exception as exc:
            print exc
            result = str(sys.exc_info())
        self.content_type = 'text/html'
        self.write(result)

    @web.authenticated
    def post(self, pathname, param):
        cserver = self.get_server()
        result = ''
        try:
            cserver.run(pathname)
        except Exception as exc:
            print >>sys.stderr, "ObjectHandler: Error executing %r: %s" \
                                % (pathname, str(exc) or repr(exc))
            result = result + str(sys.exc_info()) + '\n'
        if result:
            self.content_type = 'text/html'
            self.write(result)

    @web.authenticated
    def delete(self, pathname, param):
        cserver = self.get_server()
        result = ''
        try:
            result = cserver.onecmd('del ' + pathname)
        except Exception as exc:
            print >>sys.stderr, "ObjectHandler: Error deleting %r: %s" \
                                % (pathname, str(exc) or repr(exc))
            result = str(sys.exc_info())
        self.content_type = 'text/html'
        self.write(result)


class ObjectsHandler(ReqHandler):

    @web.authenticated
    def get(self):
        cserver = self.get_server()
        self.content_type = 'application/javascript'
        for retry in range(3):
            json_comps = cserver.get_components()
            try:
                self.write(json_comps)
            except AssertionError as exc:
                # Have had issues with `json` being ZMQ_RPC.invoke args.
                print >>sys.stderr, "ComponentsHandler: Can't write %r: %s" \
                                    % (json, str(exc) or repr(exc))
                if retry >= 2:
                    raise
            else:
                break


class StreamHandler(ReqHandler):
    ''' Return the url of the zmq stream server for `stream_name`.
    '''

    @web.authenticated
    def get(self, stream_name):
        url = self.application.server_manager.\
              get_websocket_url(self.get_sessionid(), stream_name,
                                '/workspace/'+stream_name+'_stream')
        self.write(url)


class ProjectLoadHandler(ReqHandler):
    ''' GET:  load model from the given project archive,
              or reload remembered project for session if no file given.
    '''
    @web.authenticated
    def get(self):
        path = self.get_argument('projpath', default=None)
        if path:
            self.set_secure_cookie('projpath', path)
        else:
            path = self.get_secure_cookie('projpath')
        if path:
            cserver = self.get_server()
            cserver.load_project(path)
            self.redirect(self.application.reverse_url('workspace'))
        else:
            self.redirect('/')


class ProjectRevertHandler(ReqHandler):
    ''' POST:  revert back to the most recent commit of the project.
    '''
    @web.authenticated
    def post(self):
        commit_id = self.get_argument('commit_id', default=None)
        cserver = self.get_server()
        ret = cserver.revert_project(commit_id)
        if isinstance(ret, Exception):
            self.send_error(500)
        else:
            self.write('Reverted.')


class ProjectHandler(ReqHandler):
    ''' GET:  start up an empty workspace and prepare to load a project.

        POST: commit the current project.
    '''

    @web.authenticated
    def post(self):
        comment = self.get_argument('comment', default='')
        cserver = self.get_server()
        cserver.commit_project(comment)
        self.write('Committed.')

    @web.authenticated
    def get(self):
        path = self.get_argument('projpath', default=None)
        if path:
            self.set_secure_cookie('projpath', path)
        else:
            path = self.get_secure_cookie('projpath')
        if path:
            self.delete_server()
            cserver = self.get_server()
            name = Projects().get_by_path(path)['projectname']
            cserver.set_current_project(name)
            path = os.path.join(self.get_project_dir(), path)
            self.redirect(self.application.reverse_url('workspace'))
        else:
            self.redirect('/')


class PublishHandler(ReqHandler):
    ''' GET: tell the server to publish the specified topic/variable.
    '''

    @web.authenticated
    def get(self):
        topic = self.get_argument('topic')
        publish = self.get_argument('publish', default=True)
        publish = publish in [True, 'true', 'True']
        cserver = self.get_server()
        cserver.add_subscriber(topic, publish)


class TypeHandler(ReqHandler):
    ''' GET:    Get the attributes of a type.
                param is required and must be one of:
                    'signature'
    '''

    @web.authenticated
    def get(self, typename, param):
        cserver = self.get_server()
        result = ''
        if param:
            param = param.lower()
            if param == 'signature':
                signature = cserver.get_signature(typename)
                result = json.dumps(signature)
            else:
                self.send_error(400)  # bad request
        else:
            self.send_error(400)  # bad request
        self.content_type = 'application/javascript'
        self.write(result)


class TypesHandler(ReqHandler):
    ''' GET: Get the list of available types.
    '''

    @web.authenticated
    def get(self):
        cserver = self.get_server()
        types = cserver.get_types()
        self.content_type = 'application/javascript'
        self.write(json.dumps(types))


class UploadHandler(ReqHandler):
    ''' File upload utility.
    '''

    @web.authenticated
    def post(self):
        path = self.get_argument('path', default=None)
        cserver = self.get_server()
        files = self.request.files['file']
        if files:
            for file_ in files:
                filename = file_['filename']
                if len(filename) > 0:
                    if path:
                        filename = os.path.sep.join([path, filename])
                    cserver.add_file(filename, file_['body'])
            self.render('closewindow.html')
        else:
            self.render('workspace/upload.html', path=path)

    @web.authenticated
    def get(self):
        path = self.get_argument('path', default=None)
        self.render('workspace/upload.html', path=path)


class ValueHandler(ReqHandler):
    ''' GET:    get a value for the given pathname.
        TODO:   combine with ComponentHandler? Handle Containers as well?
    '''

    @web.authenticated
    def get(self, name):
        cserver = self.get_server()
        value = cserver.get_value(name)
        self.content_type = 'application/javascript'
        self.write(value)


class VariableHandler(ReqHandler):
    ''' Get a command to set a variable, send it to the cserver, and return response.
    '''

    @web.authenticated
    def post(self):
        history = ''
        lhs = self.get_argument('lhs', default=None)
        rhs = self.get_argument('rhs', default=None)
        vtype = self.get_argument('type', default=None)
        if (lhs and rhs and vtype):
            obj, dot, attr = lhs.partition('.')
            if vtype == 'str':
                command = '%s.set(%r, %r)' % (obj, attr, rhs)
            else:
                command = '%s.set(%r, %s)' % (obj, attr, rhs)

        # if there is a command, execute it & get the result
        if command:
            result = ''
            try:
                cserver = self.get_server()
                result = cserver.onecmd(command)
            except Exception as exc:
                print exc
                result = sys.exc_info()
            if result:
                history = history + str(result) + '\n'

        self.content_type = 'text/html'
        self.write(history)

    @web.authenticated
    def get(self):
        self.content_type = 'text/html'
        self.write('')  # not used for now, could render a form


class WorkspaceHandler(ReqHandler):
    ''' Render the workspace.
    '''

    @web.authenticated
    def get(self):
        cserver = self.get_server()
        project = cserver.get_current_project()
        self.render('workspace/workspace.html', project=project)


handlers = [
    web.url(r'/workspace/?',                WorkspaceHandler, name='workspace'),
    web.url(r'/workspace/addons/?',         AddOnsHandler),
    web.url(r'/workspace/close/?',          CloseHandler),
    web.url(r'/workspace/variable',         VariableHandler),
    web.url(r'/workspace/editor/?',         EditorHandler),
    web.url(r'/workspace/geometry',         GeometryHandler),
    web.url(r'/workspace/project_revert/?', ProjectRevertHandler),
    web.url(r'/workspace/project_load/?',   ProjectLoadHandler),
    web.url(r'/workspace/project/?',        ProjectHandler),
    web.url(r'/workspace/publish/?',        PublishHandler),
    web.url(r'/workspace/upload/?',         UploadHandler),
    web.url(r'/workspace/value/(.*)',       ValueHandler),

    # new and improved
    web.url(r'/workspace/command',          CommandHandler),

    web.url(r'/workspace/files/?',          FilesHandler),
    web.url(r'/workspace/file/(.*)',        FileHandler),

    web.url(r'/workspace/object/(?P<pathname>[^\/]+)/?(?P<param>[^\/]+)?',  ObjectHandler),

    web.url(r'/workspace/stream/(.*)',      StreamHandler),

    web.url(r'/workspace/types/?',          TypesHandler),
    web.url(r'/workspace/type/(?P<typename>[^\/]+)/?(?P<param>[^\/]+)?',    TypeHandler),
]


"""

# POST   command, {command: command}

# GET    files
# DELETE files, {filepaths: [file1, file2, ...]}

# GET    file/name
# PUT    file/name, {contents: new_contents}
# POST   file/name, {rename: newname}
# DELETE file/name

GET    objects
POST   objects/name, {parent: parent, type: type, args: args}

# GET    object/name
# GET    object/name/connections
# GET    object/name/dataflow
# GET    object/name/workflow
# GET    object/name/events
# GET    object/name/passthroughs
# PUT    object/name, {type: new_type, args: args}
# DELETE object/name
# POST   object/name   (exec)

# GET    types
# GET    type/name/signature

# GET    stream/pub
# GET    stream/out

GET    subscription/name
DELETE subscription/name

GET    project/id
POST   project/id, {version: previous_version} # no version means commit

GET    variable/name
PUT    variable/name, {value: value}
POST   variable/name, {parent: parent} ??



/// utils

GET editor
GET upload
GET addons
GET close
GET test
GET base
"""
