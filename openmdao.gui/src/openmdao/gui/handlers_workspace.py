import sys
import os
import re

import jsonpickle

from tornado import web

from openmdao.gui.handlers import ReqHandler as BaseHandler
from openmdao.gui.projectdb import Projects


class AddOnsHandler(BaseHandler):
    ''' Addon installation utility.
    Eventually we will probably wrap the OpenMDAO plugin
    functions to work through here.
    '''
    addons_url = 'http://openmdao.org/dists'

    @web.authenticated
    def post(self):
        ''' Easy_install the POSTed addon.
        '''
        pass

    @web.authenticated
    def get(self):
        ''' Show available plugins; prompt for plugin to be installed.
        '''
        self.render('workspace/addons.html')


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


class GeometryHandler(ReqHandler):

    @web.authenticated
    def get(self):
        ''' geometry viewer
        '''
        filename = self.get_argument('path')
        self.render('workspace/o3dviewer.html', filename=filename)


class CloseHandler(ReqHandler):

    @web.authenticated
    def get(self):
        self.delete_server()
        self.redirect('/')


class CommandHandler(ReqHandler):
    ''' Get the command, send it to the cserver, and return response.
    '''

    @web.authenticated
    def post(self):
        history = ''
        command = self.get_argument('command', default=None)

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
            if vtype == 'str':
                command = '%s = "%s"' % (lhs, rhs)
            else:
                command = '%s = %s' % (lhs, rhs)

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

class GetAllAtributesHandler(ReqHandler):
    ''' Get all attributes of an assembly's components
    '''
    
    @web.authenticated
    def get(self, name):
        cserver = self.get_server()
        attr = {}
        try:
            attr = cserver.get_all_attributes(name)
        except Exception as exc:
            print 'Error getting all attributes on', name, ':', exc
            attr = '"%s"' % sys.exc_info()
        self.content_type = 'application/javascript'
        self.write(attr)


class ComponentHandler(ReqHandler):
    ''' Add, get, or remove a component.
    '''

    @web.authenticated
    def post(self, name):
        type = self.get_argument('type')
        if 'parent' in self.request.arguments.keys():
            parent = self.get_argument('parent')
        else:
            parent = ''
        if 'args' in self.request.arguments.keys():
            args = self.get_argument('args')
        else:
            args = ''
        result = ''
        try:
            cserver = self.get_server()
            cserver.add_component(name, type, parent, args)
        except Exception as exc:
            print exc
            result = str(sys.exc_info())
        self.content_type = 'text/html'
        self.write(result)

    @web.authenticated
    def delete(self, name):
        cserver = self.get_server()
        result = ''
        try:
            result = cserver.onecmd('del ' + name)
        except Exception as exc:
            print exc
            result = str(sys.exc_info())
        self.content_type = 'text/html'
        self.write(result)

    @web.authenticated
    def get(self, name):
        cserver = self.get_server()
        attr = {}
        try:
            attr = cserver.get_attributes(name)
        except Exception as exc:
            print 'Error getting attributes on', name, ':', exc
            attr = '"%s"' % sys.exc_info()
        self.content_type = 'application/javascript'
        self.write(attr)


class AvailableEventsHandler(ReqHandler):
    ''' Get a list of events that are available to a driver.
    '''

    @web.authenticated
    def get(self, name):
        cserver = self.get_server()
        attr = {}
        try:
            attr = cserver.get_available_events(name)
        except Exception as exc:
            print 'Error getting events on', name, ':', exc
            attr = '"%s"' % sys.exc_info()
        self.content_type = 'application/javascript'
        self.write(attr)


class ObjectHandler(ReqHandler):
    ''' Get the data for a slotable object (including components).
    '''

    @web.authenticated
    def get(self, name):
        cserver = self.get_server()
        attr = {}
        try:
            attr = cserver.get_attributes(name)
        except Exception as exc:
            print 'Error getting attributes on', name, ':', exc
            attr = '"%s"' % sys.exc_info()
        self.content_type = 'application/javascript'
        self.write(attr)


class RenameHandler(ReqHandler):
    ''' Rename a file.
    '''

    @web.authenticated
    def post(self):
        oldpath = self.get_argument('old')
        newname = self.get_argument('new')
        result = ''
        try:
            cserver = self.get_server()
            cserver.rename_file(oldpath, newname)
        except Exception as exc:
            print exc
            result = str(sys.exc_info())
        self.content_type = 'text/html'
        self.write(result)


class ReplaceHandler(ReqHandler):
    ''' Replace a component.
    '''

    @web.authenticated
    def post(self, pathname):
        type = self.get_argument('type')
        if 'args' in self.request.arguments.keys():
            args = self.get_argument('args')
        else:
            args = ''
        result = ''
        try:
            cserver = self.get_server()
            cserver.replace_component(pathname, type, args)
        except Exception as exc:
            print exc
            result = str(sys.exc_info())
        self.content_type = 'text/html'
        self.write(result)


class ComponentsHandler(ReqHandler):

    @web.authenticated
    def get(self):
        cserver = self.get_server()
        self.content_type = 'application/javascript'
        for retry in range(3):
            json = cserver.get_components()
            try:
                self.write(json)
            except AssertionError as exc:
                # Have had issues with `json` being ZMQ_RPC.invoke args.
                print >>sys.stderr, "ComponentsHandler: Can't write %r: %s" \
                                    % (json, str(exc) or repr(exc))
                if retry >= 2:
                    raise
            else:
                break


class ConnectionsHandler(ReqHandler):
    ''' Get connections between two components in an assembly.
    '''

    @web.authenticated
    def get(self, pathname):
        cserver = self.get_server()
        connects = {}
        try:
            src_name = self.get_argument('src_name', default=None)
            dst_name = self.get_argument('dst_name', default=None)
            connects = cserver.get_connections(pathname, src_name, dst_name)
        except Exception as exc:
            print exc
            connects = '"%s"' % sys.exc_info()
        self.content_type = 'application/javascript'
        self.write(connects)


class DataflowHandler(ReqHandler):
    ''' Get the structure of the specified assembly or of the global
        namespace if no pathname is specified; consists of the list of
        components and the connections between them (i.e., the dataflow).
    '''

    @web.authenticated
    def get(self, name):
        cserver = self.get_server()
        self.content_type = 'application/javascript'
        for retry in range(3):
            json = cserver.get_dataflow(name)
            try:
                self.write(json)
            except AssertionError as exc:
                # Have had issues with `json` being ZMQ_RPC.invoke args.
                print >>sys.stderr, "DataflowHandler: Can't write %r: %s" \
                                    % (json, str(exc) or repr(exc))
                if retry >= 2:
                    raise
            else:
                break


class EditorHandler(ReqHandler):

    @web.authenticated
    def get(self):
        '''Code Editor
        '''
        filename = self.get_argument('filename', default=None)
        self.render('workspace/editor.html', filename=filename)


class ExecHandler(ReqHandler):
    ''' If a filename is POSTed, have the cserver execute the file;
        otherwise, just run() the project.
    '''

    @web.authenticated
    def post(self):
        result = ''
        cserver = self.get_server()
        filename = self.get_argument('filename', default=None)
        if filename:
            try:
                result = cserver.execfile(filename)
            except Exception as exc:
                print exc
                result = result + str(sys.exc_info()) + '\n'
        else:
            pathname = self.get_argument('pathname', default='')
            try:
                cserver.run(pathname)
            except Exception as exc:
                print exc
                result = result + str(sys.exc_info()) + '\n'
        if result:
            self.content_type = 'text/html'
            self.write(result)


class FileHandler(ReqHandler):
    ''' Get/set the specified file/folder.
    '''

    @web.authenticated
    def post(self, filename):
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
        self.write(str(cserver.delete_file(filename)))

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


class FilesHandler(ReqHandler):
    ''' Get a list of the user's files in JSON format.
    '''

    @web.authenticated
    def get(self):
        cserver = self.get_server()
        filedict = cserver.get_files()
        json = jsonpickle.encode(filedict)
        self.content_type = 'application/javascript'
        self.write(json)


class ModelHandler(ReqHandler):
    ''' POST: get a new model (delete existing console server).
        GET:  get JSON representation of the model.
    '''

    @web.authenticated
    def post(self):
        self.delete_server()
        self.redirect('/')

    @web.authenticated
    def get(self):
        cserver = self.get_server()
        json = cserver.get_JSON()
        self.content_type = 'application/javascript'
        self.write(json)


class OutstreamHandler(ReqHandler):
    ''' Return the url of the zmq outstream server.
    '''

    @web.authenticated
    def get(self):
        url = self.application.server_manager.\
              get_out_server_url(self.get_sessionid(), '/workspace/outstream')
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
            #path = os.path.join(self.get_project_dir(), path)
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


class PlotHandler(ReqHandler):
    ''' GET: open a websocket server to supply updated values for the
             specified variable.
    '''

    @web.authenticated
    def get(self, name):
        cserver = self.get_server()
        port = cserver.get_varserver(name)
        self.write(port)


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


class PubstreamHandler(ReqHandler):
    ''' Return the url of the zmq publisher server.
    '''

    @web.authenticated
    def get(self):
        url = self.application.server_manager.\
              get_pub_server_url(self.get_sessionid(), '/workspace/pubstream')
        self.write(url)


class TypesHandler(ReqHandler):
    ''' Get hierarchy of package/types to populate the Palette.
    '''

    @web.authenticated
    def get(self):
        cserver = self.get_server()
        types = cserver.get_types()
        self.content_type = 'application/javascript'
        self.write(jsonpickle.encode(types))


class SignatureHandler(ReqHandler):
    ''' Get type constructor signature.
    '''

    @web.authenticated
    def get(self):
        typename = self.get_argument('type')
        cserver = self.get_server()
        signature = cserver.get_signature(typename)
        self.content_type = 'application/javascript'
        self.write(jsonpickle.encode(signature))


class UploadHandler(ReqHandler):
    ''' File upload utility
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
    ''' GET: get a value for the given pathname.
        TODO: combine with ComponentHandler? handle Containers as well?
    '''

    @web.authenticated
    def get(self, name):
        cserver = self.get_server()
        value = cserver.get_value(name)
        self.content_type = 'application/javascript'
        self.write(value)


class WorkflowHandler(ReqHandler):

    @web.authenticated
    def get(self, name):
        cserver = self.get_server()
        self.content_type = 'application/javascript'
        for retry in range(3):
            json = cserver.get_workflow(name)
            try:
                self.write(json)
            except AssertionError as exc:
                # Have had issues with `json` being ZMQ_RPC.invoke args.
                print >>sys.stderr, "WorkflowHandler: Can't write %r: %s" \
                                    % (json, str(exc) or repr(exc))
                if retry >= 2:
                    raise
            else:
                break


class WorkspaceHandler(ReqHandler):
    ''' render the workspace
    '''

    @web.authenticated
    def get(self):
        cserver = self.get_server()
        project = cserver.get_current_project()
        self.render('workspace/workspace.html', project=project)


class TestHandler(ReqHandler):
    ''' Initialize the server manager & render the workspace.
    '''

    @web.authenticated
    def get(self):
        self.render('workspace/test.html')


handlers = [
    web.url(r'/workspace/?',                WorkspaceHandler, name='workspace'),
    web.url(r'/workspace/addons/?',         AddOnsHandler),
    web.url(r'/workspace/base/?',           ReqHandler),
    web.url(r'/workspace/close/?',          CloseHandler),
    web.url(r'/workspace/command',          CommandHandler),
    web.url(r'/workspace/variable',         VariableHandler),
    web.url(r'/workspace/components/?',     ComponentsHandler),
    web.url(r'/workspace/component/(.*)',   ComponentHandler),
    web.url(r'/workspace/connections/(.*)', ConnectionsHandler),
    web.url(r'/workspace/dataflow/(.*)/?',  DataflowHandler),
    web.url(r'/workspace/events/(.*)',      AvailableEventsHandler),
    web.url(r'/workspace/editor/?',         EditorHandler),
    web.url(r'/workspace/exec/?',           ExecHandler),
    web.url(r'/workspace/file/(.*)',        FileHandler),
    web.url(r'/workspace/files/?',          FilesHandler),
    web.url(r'/workspace/geometry',         GeometryHandler),
    web.url(r'/workspace/model/?',          ModelHandler),
    web.url(r'/workspace/object/(.*)',      ObjectHandler),
    web.url(r'/workspace/outstream/?',      OutstreamHandler),
    web.url(r'/workspace/plot/?',           PlotHandler),
    web.url(r'/workspace/project_revert/?', ProjectRevertHandler),
    web.url(r'/workspace/project_load/?',   ProjectLoadHandler),
    web.url(r'/workspace/project/?',        ProjectHandler),
    web.url(r'/workspace/publish/?',        PublishHandler),
    web.url(r'/workspace/pubstream/?',      PubstreamHandler),
    web.url(r'/workspace/rename',           RenameHandler),
    web.url(r'/workspace/replace/(.*)',     ReplaceHandler),
    web.url(r'/workspace/signature/?',      SignatureHandler),
    web.url(r'/workspace/types/?',          TypesHandler),
    web.url(r'/workspace/upload/?',         UploadHandler),
    web.url(r'/workspace/value/(.*)',       ValueHandler),
    web.url(r'/workspace/workflow/(.*)',    WorkflowHandler),
    web.url(r'/workspace/test/?',           TestHandler),
    web.url(r'/workspace/get_all_attributes/(.*)',GetAllAtributesHandler),
]
