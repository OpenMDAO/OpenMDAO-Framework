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
        args = {}
        for field in ['head']:
            if field in self.request.arguments.keys():
                args[field] = self.request.arguments[field][0]
            else:
                args[field] = False
        self.render('workspace/base.html', **args)

    @web.authenticated
    def get(self):
        args = {}
        for field in ['head_script']:
            if field in self.request.arguments.keys():
                s = self.request.arguments[field][0]
                s = re.sub(r'^"|"$', '', s)  # strip leading/trailing quotes
                s = re.sub(r"^'|'$", "", s)  # strip leading/trailing quotes
                args[field] = s
            else:
                args[field] = False
        self.render('workspace/base.html', **args)


class AddOnsHandler(BaseHandler):
    ''' Add-on installation utility.
        (TODO: Wrap the OpenMDAO plugin functions to work through here....)

        GET:    Render the addon installation utility template.

        POST:   Install the POSTed addon.
    '''
    addons_url = 'http://openmdao.org/dists'

    @web.authenticated
    def get(self):
        self.render('workspace/addons.html')

    @web.authenticated
    def post(self):
        pass


class CommandHandler(ReqHandler):
    ''' POST: Execute a command and return the console-like response;
              required arguments are:

              command:  Command to execute
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
                print >>sys.stderr, "CommandHandler: Error issuing command %s: %s" \
                                    % (command, str(exc) or repr(exc))
                result += str(sys.exc_info())
            if result:
                result += '\n'
                self.content_type = 'text/html'
                self.write(result)


class EditorHandler(ReqHandler):
    ''' code editor utility

        GET:    Render the code editor; arguments are:

                filename:   Full pathname of file to edit (optional).
    '''

    @web.authenticated
    def get(self):
        filename = self.get_argument('filename', default=None)
        self.render('workspace/editor.html', filename=filename)


class FileHandler(ReqHandler):
    ''' GET:    Get the contents of file `filename`.

        PUT:    Write contents to file `filename`.

        DELETE: Delete file `filename`.

        POST:   Rename file `filename` if rename argument is provided;
                otherwise execute file `filename`. Arguments are:

                rename: New name for file `filename` (optional).
    '''

    @web.authenticated
    def get(self, filename):
        cserver = self.get_server()
        download = self.get_argument('download', default=False)
        (contents, mimetype, encoding) = cserver.get_file(filename)

        if download:
            if download in ['True', 'true']:
                download = True
            else:
                download = False

        if download:
            self.set_cookie('fileDownload', 'true')  # for jQuery.fileDownload
            self.set_header('Content-Type', 'application/octet-stream')
            self.set_header('Content-Disposition',
                            'attachment; filename="' + filename + '"')
        else:
            self.set_cookie('fileDownload', 'false')
            self.set_header('Content-Disposition',
                            'inline; filename="' + filename + '"')
            if mimetype:
                self.set_header('Content-Type', mimetype)
            else:
                self.set_header('Content-Type', 'application/octet-stream')
            if encoding:
                self.set_header('Content-Encoding', encoding)

        self.write(contents)

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
            print >>sys.stderr, "FileHandler: Error %s file: %s" \
                % ('renaming' if newname else 'executing', str(exc) or repr(exc))
            result = result + str(sys.exc_info()) + '\n'
        self.content_type = 'text/html'
        self.write(result)


class FilesHandler(ReqHandler):
    ''' GET:    Get heirarchical list of files in the current project.

        DELETE: Delete files; arguments are:

                filepaths - full pathnames of files to delete (required)

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
    ''' geometry viewer utility

        GET:    Render the geometry viewer; arguments are:

                path:   Full path name of geometry object or file.
    '''

    @web.authenticated
    def get(self):
        path = self.get_argument('path')
        #self.render('workspace/o3dviewer.html', filename=path)
        if path.startswith('file/'):
            path = path[4:]  # leave the '/' at the beginning of filename
        self.render('workspace/wvclient.html', geom_name=path)


class ImagesHandler(ReqHandler):
    ''' image viewer utility

        GET:    Render the image viewer.
    '''

    @web.authenticated
    def get(self):
        path = self.get_argument('path')
        self.render('workspace/imageviewer.html', filename=path)


class ObjectHandler(ReqHandler):
    ''' GET:    Get the attributes of object `pathname`;
                `attr` is optional and can specify one of the following:
                    dataflow
		    
                    workflow
		    
                    events
		    
                    passthroughs
		    
                    connections

        PUT:    Create or replace object `pathname`; arguments are:

                type: The type of the new object (required)

                args: Arguments required to create the new object (optional).

        POST:   Execute object `pathname`

        DELETE: Delete object `pathname`
    '''

    @web.authenticated
    def get(self, pathname, attr):
        if pathname.lower() == 'none':
            pathname = None
        cserver = self.get_server()
        result = {}
        for retry in range(3):
            try:
                if attr:
                    attr = attr.lower()
                    if attr == 'dataflow':
                        result = cserver.get_dataflow(pathname)
                    elif attr == 'workflow':
                        result = cserver.get_workflow(pathname)
                    elif attr == 'events':
                        result = cserver.get_available_events(pathname)
                    elif attr == 'passthroughs':
                        result = cserver.get_passthroughs(pathname)
                    elif attr == 'connections':
                        source = self.get_argument('source', default=None)
                        target = self.get_argument('target', default=None)
                        result = cserver.get_connections(pathname, source, target)
                    else:
                        self.send_error(400)  # bad request
                else:
                    result = cserver.get_attributes(pathname)

                self.content_type = 'application/javascript'
                self.write(result)
            except AssertionError as exc:
                # Have had issues with `result` being ZMQ_RPC.invoke args.
                print >>sys.stderr, "ObjectHandler: Can't write %r: %s" \
                                    % (result, str(exc) or repr(exc))
                if retry >= 2:
                    raise
            else:
                break

    @web.authenticated
    def put(self, pathname, attr):
        if attr:
            self.send_error(400)  # bad request

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
            print >>sys.stderr, "ObjectHandler: Error putting %r: %s" \
                                % (pathname, str(exc) or repr(exc))
            result = str(sys.exc_info())
        self.content_type = 'text/html'
        self.write(result)

    @web.authenticated
    def post(self, pathname, attr):
        if attr:
            self.send_error(400)  # bad request

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
    def delete(self, pathname, attr):
        if attr:
            self.send_error(400)  # bad request

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
    ''' GET:    Get heirarchical list of objects in the current project.
                (NOTE: currently only lists 'Component' objects...)
    '''

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
    ''' GET:    Start up an empty workspace and prepare to load a project.
                (Loading a project is a two-step process. The first step is when
                the server is initialized and the workspace is loaded.
                After the workspace is loaded and websockets are connected, the next step  
                should be a POST to project/load that will actually load the project 
		into the server.)

        POST:   Perform the specified action on the current project; arguments are:

                action: One of the following (required)

                	load:   Load project into the current server;
                                if no project path is given, get from session cookie.

                                additional args: projpath (optional)

                    	commit: Commit the current project.

                                additional args: comment (optional)

                    	revert: Revert back to the most recent commit of the project.

                                additional args: commit_id (optional)

                    	close:  Close the current project
    '''

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
            proj = Projects().get_by_path(path)
            if proj is None:  # Shouldn't happen.
                print >>sys.stderr, "ProjectHandler: no project for %r" % path
                args = dict(path=path)
                self.render('workspace/oops.html', **args)
            else:
                name = proj['projectname']
                cserver.set_current_project(name)
                path = os.path.join(self.get_project_dir(), path)
                self.redirect(self.application.reverse_url('workspace'))
        else:
            self.redirect('/')

    @web.authenticated
    def post(self):
        action = self.get_argument('action', default=None)
        if action:
            cserver = self.get_server()
            action = action.lower()
            if action == 'load':
                path = self.get_argument('projpath', default=None)
                if path:
                    self.set_secure_cookie('projpath', path)
                else:
                    path = self.get_secure_cookie('projpath')
                if path:
                    cserver = self.get_server()
                    cserver.load_project(path)
                    self.set_status(204)  # successful, no data in response
                else:
                    self.send_error(400)  # bad request
            elif action == 'commit':
                comment = self.get_argument('comment', default='')
                cserver = self.get_server()
                cserver.commit_project(comment)
                self.set_status(204)  # successful, no data in response
            elif action == 'revert':
                commit_id = self.get_argument('commit_id', default=None)
                cserver = self.get_server()
                ret = cserver.revert_project(commit_id)
                if isinstance(ret, Exception):
                    self.send_error(500)
                else:
                    self.set_status(204)  # successful, no data in response
            elif action == 'close':
                self.delete_server()
                self.clear_cookie('projpath')
                self.set_status(204)  # successful, no data in response
            else:
                self.send_error(400)  # bad request
        else:
            self.send_error(400)  # bad request


class StreamHandler(ReqHandler):
    ''' GET:    Get the url of the websocket server for stream `stream_name`.
    '''

    @web.authenticated
    def get(self, stream_name):
        url = self.application.server_manager.\
              get_websocket_url(self.get_sessionid(), stream_name,
                                '/workspace/'+stream_name+'_stream')
        self.write(url)


class SubscriptionHandler(ReqHandler):
    ''' GET:    Get a subscription to `topic`.
                (Messages will be published via the pub websocket.)

        DELETE: Remove a subscription to `topic`.
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
    ''' GET:    Get attributes of type `typename`;
                `attr` is required and must be one of:

                signature:  Arguments required to create an instance of `typename`
    '''

    @web.authenticated
    def get(self, typename, attr):
        cserver = self.get_server()
        result = ''
        if attr:
            attr = attr.lower()
            if attr == 'signature':
                signature = cserver.get_signature(typename)
                result = json.dumps(signature)
            else:
                self.send_error(400)  # bad request
        else:
            self.send_error(400)  # bad request
        self.content_type = 'application/javascript'
        self.write(result)


class TypesHandler(ReqHandler):
    ''' GET:    Get the list of available types.
    '''

    @web.authenticated
    def get(self):
        cserver = self.get_server()
        types = cserver.get_types()
        self.content_type = 'application/javascript'
        self.write(json.dumps(types))


class UploadHandler(ReqHandler):
    ''' file upload utility

        GET:    Render the upload form.

        POST:   Add the POSTed files to the current project.
    '''

    @web.authenticated
    def get(self):
        path = self.get_argument('path', default=None)
        self.render('workspace/upload.html', path=path)

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


class VariableHandler(ReqHandler):
    ''' GET:    Get the value of variable `pathname`.

        PUT:    Set the value of variable `pathname`.
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
                print >>sys.stderr, "VariableHandler: Error issuing command %s: %s" \
                                    % (command, str(exc) or repr(exc))
                result += str(sys.exc_info())
            if result:
                result += '\n'
                self.content_type = 'text/html'
                self.write(result)

        else:
            self.send_error(400)  # bad request


class WorkspaceHandler(ReqHandler):
    ''' GET:    render the workspace.
    '''

    @web.authenticated
    def get(self):
        cserver = self.get_server()
        project = cserver.get_current_project()
        self.render('workspace/workspace.html', project=project)


handlers = [
    web.url(r'/workspace/?',                                                WorkspaceHandler, name='workspace'),
    web.url(r'/workspace/base/?',                                           ReqHandler),

    web.url(r'/workspace/command',                                          CommandHandler),

    web.url(r'/workspace/files/?',                                          FilesHandler),
    web.url(r'/workspace/file/(.*)',                                        FileHandler),

    web.url(r'/workspace/objects/?',                                        ObjectsHandler),
    web.url(r'/workspace/object/(?P<pathname>[^\/]+)/?(?P<attr>[^\/]+)?',   ObjectHandler),

    web.url(r'/workspace/project',                                          ProjectHandler),

    web.url(r'/workspace/stream/(.*)',                                      StreamHandler),

    web.url(r'/workspace/subscription/(.*)',                                SubscriptionHandler),

    web.url(r'/workspace/types',                                            TypesHandler),
    web.url(r'/workspace/type/(?P<typename>[^\/]+)/(?P<attr>[^\/]+)',       TypeHandler),

    web.url(r'/workspace/variable/(.*)',                                    VariableHandler),

    # tools
    web.url(r'/workspace/tools/addons/?',       AddOnsHandler),
    web.url(r'/workspace/tools/editor/?',       EditorHandler),
    web.url(r'/workspace/tools/geometry',       GeometryHandler),
    web.url(r'/workspace/tools/images',         ImagesHandler),
    web.url(r'/workspace/tools/upload/?',       UploadHandler),
]
