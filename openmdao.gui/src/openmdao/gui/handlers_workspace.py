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
                result += str(sys.exc_info())
            if result:
                result += '\n'
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
                otherwise execute the file
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

        DELETE: delete files, arguments are:

                filepaths - the pathnames of the files to delete (required)

                returns 'True' if all files were successfully deleted
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
        # should be able to use self.get_arguments('filepaths'),
        # but it doesn't seem to work as advertised so...
        args = escape.json_decode(self.request.body)

        if 'filepaths' not in args.keys():
            self.send_error(400)  # bad request
        else:
            filepaths = args['filepaths']

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
                    dataflow
                    workflow
                    events
                    passthroughs
                    connections

        PUT:    create or replace an object. arguments are:
                    type - the type of the new object (required)
                    args - arguments required to create the new object (optional)

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


class ProjectHandler(ReqHandler):
    ''' GET:  start up an empty workspace and prepare to load a project.
              (loading a project is a two step process, this is the first step
               in which the server is initialized... this should be followed
               by a POST to project/load that will actually load the project)

        POST: perform the specified processing directive on the current project.
              param is required and must be one of:

              load:     load model from the given project archive or reload
                        the current project for session if no projpath given.

                        args: projpath (optional)

              commit    commit the current project.

                        args: comment (optional)

              revert:   revert back to the most recent commit of the project.

                        args: commit_id (optional)

              close:    close the current project
    '''

    @web.authenticated
    def get(self, param):
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

    @web.authenticated
    def post(self, param):
        cserver = self.get_server()
        if param:
            param = param.lower()
            if param == 'load':
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
            elif param == 'commit':
                comment = self.get_argument('comment', default='')
                cserver = self.get_server()
                cserver.commit_project(comment)
                self.write('Committed.')
            elif param == 'revert':
                commit_id = self.get_argument('commit_id', default=None)
                cserver = self.get_server()
                ret = cserver.revert_project(commit_id)
                if isinstance(ret, Exception):
                    self.send_error(500)
                else:
                    self.write('Reverted.')
            elif param == 'close':
                self.delete_server()
                self.clear_cookie('projpath')
                self.add_header('Location', '/')    # redirect to index
                self.flush()
            else:
                self.send_error(400)  # bad request
        else:
            self.send_error(400)  # bad request


class StreamHandler(ReqHandler):
    ''' GET: get the url of the websocket server for `stream_name`.
    '''

    @web.authenticated
    def get(self, stream_name):
        url = self.application.server_manager.\
              get_websocket_url(self.get_sessionid(), stream_name,
                                '/workspace/'+stream_name+'_stream')
        self.write(url)


class SubscriptionHandler(ReqHandler):
    ''' GET: get a subscription to a topic
             (messages will be published via the pub websocket)

        DELETE: remove a subscription to a topic
    '''

    @web.authenticated
    def get(self, topic):
        cserver = self.get_server()
        cserver.add_subscriber(escape.url_unescape(topic), True)

    @web.authenticated
    def delete(self, topic):
        cserver = self.get_server()
        cserver.add_subscriber(escape.url_unescape(topic), False)


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


class VariableHandler(ReqHandler):
    ''' GET:    get the value of a variable.

        PUT:    set the value of a variable.
    '''

    @web.authenticated
    def get(self, pathname):
        cserver = self.get_server()
        value = cserver.get_value(escape.url_unescape(pathname))
        self.content_type = 'application/javascript'
        self.write(value)

    @web.authenticated
    def post(self, pathname):
        rhs = self.get_argument('rhs', default=None)
        vtype = self.get_argument('type', default=None)
        if (rhs and vtype):
            obj, dot, var = escape.url_unescape(pathname).partition('.')
            if vtype == 'str':
                command = '%s.set(%r, %r)' % (obj, var, rhs)
            else:
                command = '%s.set(%r, %s)' % (obj, var, rhs)

            result = ''
            try:
                cserver = self.get_server()
                result = cserver.onecmd(command)
            except Exception as exc:
                print exc
                result += str(sys.exc_info())
            if result:
                result += '\n'
                self.content_type = 'text/html'
                self.write(result)

        else:
            self.send_error(400)  # bad request


class WorkspaceHandler(ReqHandler):
    ''' Render the workspace.
    '''

    @web.authenticated
    def get(self):
        cserver = self.get_server()
        project = cserver.get_current_project()
        self.render('workspace/workspace.html', project=project)


handlers = [
    web.url(r'/workspace/?',                    WorkspaceHandler, name='workspace'),

    web.url(r'/workspace/command',                                          CommandHandler),

    web.url(r'/workspace/files/?',                                          FilesHandler),
    web.url(r'/workspace/file/(.*)',                                        FileHandler),

    web.url(r'/workspace/objects/?',                                        ObjectsHandler),
    web.url(r'/workspace/object/(?P<pathname>[^\/]+)/?(?P<param>[^\/]+)?',  ObjectHandler),

    web.url(r'/workspace/project/?(?P<param>[^\/]+)?',                      ProjectHandler),

    web.url(r'/workspace/stream/(.*)',                                      StreamHandler),

    web.url(r'/workspace/subscription/(.*)',                                SubscriptionHandler),

    web.url(r'/workspace/types',                                            TypesHandler),
    web.url(r'/workspace/type/(?P<typename>[^\/]+)/(?P<param>[^\/]+)',      TypeHandler),

    web.url(r'/workspace/variable/(.*)',                                    VariableHandler),

    # tools
    web.url(r'/workspace/tools/addons/?',       AddOnsHandler),
    web.url(r'/workspace/tools/editor/?',       EditorHandler),
    web.url(r'/workspace/tools/geometry',       GeometryHandler),
    web.url(r'/workspace/tools/upload/?',       UploadHandler),
]
