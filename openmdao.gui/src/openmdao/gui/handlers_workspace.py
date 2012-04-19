import sys, os, traceback
import time
import jsonpickle

import zmq
from zmq.eventloop.zmqstream import ZMQStream

from tornado import httpserver, web, websocket

from openmdao.util.network import get_unused_ip_port

from openmdao.gui.util import *
from openmdao.gui.handlers import BaseHandler
from openmdao.gui.projectdb import Projects


#class AddonForm(forms.Form):
#    distribution = forms.CharField(label='Distribution')
    
class AddOnsHandler(BaseHandler):
    ''' addon installation utility
    Eventually we will probably wrap the OpenMDAO plugin
    functions to work through here.
    '''
    addons_url = 'http://openmdao.org/dists'
    
    @web.authenticated
    def post(self):
        ''' easy_install the POST'd addon
        '''
        pass
    
    @web.authenticated
    def get(self):
        ''' show available plugins, prompt for plugin to be installed
        '''
        self.render('workspace/addons.html')
        
class GeometryHandler(BaseHandler):
    @web.authenticated
    def get(self):
        ''' geometry viewer
        '''
        filename = self.get_argument('path')
        self.render('workspace/o3dviewer.html',filename=filename)
 
class CloseHandler(BaseHandler):
    @web.authenticated
    def get(self):
        self.delete_server()
        self.redirect('/')

class CommandHandler(BaseHandler):
    ''' get the command, send it to the cserver, return response
    '''
    @web.authenticated
    def post(self):
        history = ''
        command = self.get_argument('command')
        # if there is a command, execute it & get the result
        if command:
            result = ''
            try:
                cserver = self.get_server()
                result = cserver.onecmd(command)
            except Exception,e:
                print e
                result = sys.exc_info()
            if result:
                history = history + str(result) + '\n'
        self.content_type = 'text/html'
        self.write(history)
        
    @web.authenticated
    def get(self):
        self.content_type = 'text/html'
        self.write('') # not used for now, could render a form

class ComponentHandler(BaseHandler):
    ''' add, remove or get a component
    '''
    @web.authenticated
    def post(self,name):
        type = self.get_argument('type')
        if 'parent' in self.request.arguments.keys():
            parent = self.get_argument('parent')
        else:
            parent = ''
        result = ''
        try:
            cserver = self.get_server()
            cserver.add_component(name,type,parent);
        except Exception,e:
            print e
            result = sys.exc_info()
        self.content_type = 'text/html'
        self.write(result)
        
    @web.authenticated
    def delete(self,name):
        cserver = self.get_server()
        result = ''
        try:
            result = cserver.onecmd('del '+name)
        except Exception,e:
            print e
            result = sys.exc_info()
        self.content_type = 'text/html'
        self.write(result)
        
    @web.authenticated
    def get(self,name):
        cserver = self.get_server()
        attr = {}
        try:
            attr = cserver.get_attributes(name)
        except Exception, e:
            exc_type, exc_value, exc_traceback = sys.exc_info()
            print 'Exception calling get_attributes on',name
            print '-'*60
            print "*** print_exception:"
            traceback.print_exception(exc_type, exc_value, exc_traceback,file=sys.stdout)
            print "*** print_tb:"
            traceback.print_tb(exc_traceback, limit=100, file=sys.stdout)        
        self.content_type = 'application/javascript'
        self.write(attr)

class ComponentsHandler(BaseHandler):
    @web.authenticated
    def get(self):
        cserver = self.get_server()
        json = cserver.get_components()
        self.content_type = 'application/javascript'
        self.write(json)

class ConnectionsHandler(BaseHandler):
    ''' get/set connections between two components in an assembly
    '''
    @web.authenticated
    def post(self,pathname):
        result = ''
        try:
            src_name = self.get_argument('src_name')
            dst_name = self.get_argument('dst_name')
            connections = self.get_argument('connections')
            cserver = self.get_server()
            cserver.set_connections(pathname,src_name,dst_name,connections);
        except Exception,e:
            print e
            result = sys.exc_info()
        self.content_type = 'text/html'
        self.write(result)
        
    @web.authenticated
    def get(self,pathname):
        cserver = self.get_server()
        connections = {}
        try:
            src_name = self.get_argument('src_name')
            dst_name = self.get_argument('dst_name')
            connections = cserver.get_connections(pathname,src_name,dst_name);
        except Exception, e:
            print e
        self.content_type = 'application/javascript'
        self.write(connections)

class ExecHandler(BaseHandler):
    ''' if a filename is POST'd, have the cserver execute the file
        otherwise just run() the project
    '''
    @web.authenticated
    def post(self):
        result = ''
        cserver = self.get_server()
        filename = self.get_argument('filename',default=None)
        if filename:
            try:
                result = cserver.execfile(filename)
            except Exception,e:
                print e
                result = result + str(sys.exc_info()) + '\n'                
        else:
            try:
                cserver.run()
            except Exception,e:
                print e
                result = result + str(sys.exc_info()) + '\n'
        if result:
            self.content_type = 'text/html'
            self.write(result)

class FileHandler(BaseHandler):
    ''' get/set the specified file/folder
    '''
    @web.authenticated
    def post(self,filename):
        cserver = self.get_server()
        isFolder = self.get_argument('isFolder',default=None)
        if isFolder:
            self.write(cserver.ensure_dir(filename))
        else:
            contents = self.get_argument('contents',default='')
            self.write(str(cserver.write_file(filename,contents)))
            
    @web.authenticated
    def delete(self,filename):
        cserver = self.get_server()
        self.content_type = 'text/html'
        self.write(str(cserver.delete_file(filename)))
        
    @web.authenticated
    def get(self,filename):
        cserver = self.get_server()
        self.content_type = 'text/html'
        self.write(str(cserver.get_file(filename)))

class FilesHandler(BaseHandler):
    ''' get a list of the users files in JSON format
    '''
    @web.authenticated
    def get(self):
        cserver = self.get_server()
        filedict = cserver.get_files()
        json = jsonpickle.encode(filedict)
        self.content_type = 'application/javascript'
        self.write(json)
    
class ModelHandler(BaseHandler):
    ''' POST: get a new model (delete existing console server)
        GET:  get JSON representation of the model
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

class OutstreamHandler(BaseHandler):
    ''' return the url of the zmq outstream server,
    '''
    @web.authenticated
    def get(self):
        url = self.application.server_manager.get_out_server_url(self.get_sessionid(),'/workspace/outstream')
        self.write(url)

class ProjectHandler(BaseHandler):
    ''' GET:  load model fom the given project archive,
              or reload remebered project for session if no file given
              
        POST: save project archive of the current project
    '''
    @web.authenticated
    def post(self):
        cserver = self.get_server()
        cserver.save_project()
        
        # Sadly, this probably won't work because of client/server
        #filename = self.get_secure_cookie('filename')
        #if filename:
        #    pdb = Projects()
        #    project = pdb.get_by_filename(filename)
        #    pdb.modified(project['id'])
            
        self.write('Saved.')
        
    @web.authenticated
    def get(self):
        filename = self.get_argument('filename',default=None)
        if filename:
            self.set_secure_cookie('filename',filename)
        else:
            filename = self.get_secure_cookie('filename')
        if filename:
            self.delete_server()
            cserver = self.get_server()
            filename = os.path.join(self.get_project_dir(),filename)
            cserver.load_project(filename)
            self.redirect(self.application.reverse_url('workspace'))
        else:
            self.redirect('/')

class PlotHandler(BaseHandler):
    ''' GET:  open a websocket server to supply updated valaues for the specified variable        
    '''
    @web.authenticated
    def get(self,name):
        cserver = self.get_server()
        port = cserver.get_varserver(name)
        self.write(port)

class PubstreamHandler(BaseHandler):
    ''' return the url of the zmq publisher server,
    '''
    @web.authenticated
    def get(self):
        url = self.application.server_manager.get_pub_server_url(self.get_sessionid(),'/workspace/pubstream')
        self.write(url)
        
class StructureHandler(BaseHandler):
    ''' get the structure of the specified assembly, or of the global 
        namespace if no pathname is specified, consisting of the list
        of components and the connections between them
    '''
    @web.authenticated
    def get(self,name):
        cserver = self.get_server()
        json = cserver.get_structure(name)
        self.content_type = 'application/javascript'
        self.write(json)

class TypesHandler(BaseHandler):
    ''' get hierarchy of package/types to populate the Palette
    '''
    @web.authenticated
    def get(self):
        cserver = self.get_server()
        types = cserver.get_available_types()
        try:
            types['working'] = cserver.get_workingtypes()
        except Exception, err:
            print "Error adding working types:", str(err)
        self.content_type = 'application/javascript'        
        self.write(jsonpickle.encode(types))

class UploadHandler(BaseHandler):
    ''' file upload utility
    '''
    @web.authenticated
    def post(self):
        cserver = self.get_server()
        file = self.request.files['myfile'][0]
        if file:
            filename = file['filename']
            if len(filename) > 0:
                cserver.add_file(filename,file['body'])
                self.render('closewindow.html')

    @web.authenticated
    def get(self):
        self.render('workspace/upload.html')

class WorkflowHandler(BaseHandler):
    @web.authenticated
    def get(self,name):
        cserver = self.get_server()
        json = cserver.get_workflow(name)
        self.content_type = 'application/javascript'
        self.write(json)
    
class WorkspaceHandler(BaseHandler):
    ''' render the workspace
    '''
    @web.authenticated
    def get(self):
        self.render('workspace/workspace.html')

class TestHandler(BaseHandler):
    ''' initialize the server manager &  render the workspace
    '''
    @web.authenticated
    def get(self):
        self.render('workspace/test.html')


handlers = [
    web.url(r'/workspace/?',                WorkspaceHandler, name='workspace'),
    web.url(r'/workspace/components/?',     ComponentsHandler),
    web.url(r'/workspace/component/(.*)',   ComponentHandler),
    web.url(r'/workspace/connections/(.*)', ConnectionsHandler),
    web.url(r'/workspace/addons/?',         AddOnsHandler),
    web.url(r'/workspace/close/?',          CloseHandler),
    web.url(r'/workspace/command',          CommandHandler),
    web.url(r'/workspace/structure/(.*)/?', StructureHandler),
    web.url(r'/workspace/exec/?',           ExecHandler),
    web.url(r'/workspace/file/(.*)',        FileHandler),
    web.url(r'/workspace/files/?',          FilesHandler),
    web.url(r'/workspace/geometry',         GeometryHandler),
    web.url(r'/workspace/model/?',          ModelHandler),
    web.url(r'/workspace/outstream/?',      OutstreamHandler),
    web.url(r'/workspace/plot/?',           PlotHandler),
    web.url(r'/workspace/project/?',        ProjectHandler),
    web.url(r'/workspace/pubstream/?',      PubstreamHandler),
    web.url(r'/workspace/types/?',          TypesHandler),
    web.url(r'/workspace/upload/?',         UploadHandler),
    web.url(r'/workspace/workflow/(.*)',    WorkflowHandler),
    web.url(r'/workspace/test/?',           TestHandler),
]

