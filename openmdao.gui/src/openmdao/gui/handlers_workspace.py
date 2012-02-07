import sys, os, traceback
import time
import jsonpickle
import threading

import zmq
from zmq.eventloop.zmqstream import ZMQStream

from tornado import web
from tornado import websocket

from django import forms

from openmdao.util.network import get_unused_ip_port

from openmdao.gui.util import *
from openmdao.gui.settings import MEDIA_ROOT
from openmdao.gui.handlers import BaseHandler


class AddonForm(forms.Form):
    distribution = forms.CharField(label='Distribution')
    
class AddOnsHandler(BaseHandler):
    ''' addon installation utility
    '''
    addons_url = 'http://openmdao.org/dists'
    addons_url = 'http://torpedo.grc.nasa.gov:31005/'
    
    @web.authenticated
    def post(self):
        ''' easy_install the POST'd addon
        '''
        form_data = {}
        for field in ['distribution']:
            if field in self.request.arguments.keys():
                form_data[field]=self.request.arguments[field][0]
        form = AddonForm(form_data)
        if form.is_valid():
            distribution = form.cleaned_data['distribution']
            cserver = self.get_server()
            cserver.install_addon(self.addons_url, distribution)
            self.render('closewindow.html')
            
    @web.authenticated
    def get(self):
        ''' show available addons, prompt for addon to be installed
        '''
        form = AddonForm()
        self.render('workspace/addons.html', 
                     addons_url=self.addons_url, addon_form=form)
        
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
            history = history + '>>> '+str(command) + '\n'
            result = ''
            try:
                cserver = self.get_server()
                result = cserver.onecmd(command)
            except Exception,e:
                print e
                result = sys.exc_info()
            if result:
                history = history + str(result) + '\n'
        self.write(history)
        
    @web.authenticated
    def get(self):
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
        self.write(attr)

class ComponentsHandler(BaseHandler):
    @web.authenticated
    def get(self):
        cserver = self.get_server()
        json = cserver.get_components()
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
        self.write(connections)

class StructureHandler(BaseHandler):
    ''' get the structure of the specified assembly, or of the global 
        namespace if no pathname is specified, consisting of the list
        of components and the connections between them
    '''
    @web.authenticated
    def get(self,name):
        cserver = self.get_server()
        json = cserver.get_structure(name)
        self.write(json)

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
            self.write(result)

class ExitHandler(BaseHandler):
    ''' close the browser window and shut down the server    
        (unfortunately neither of these things actually work)
        TODO: kill server based on PID per example at:
        http://blog.perplexedlabs.com/2010/07/01/pythons-tornado-has-swept-me-off-my-feet/
    '''
    @web.authenticated
    def get(self):
        self.delete_server()        
        self.render('closewindow.html')
        time.sleep(2)
        quit()

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
        self.write(str(cserver.delete_file(filename)))
        
    @web.authenticated
    def get(self,filename):
        cserver = self.get_server()
        self.write(str(cserver.get_file(filename)))

class FilesHandler(BaseHandler):
    ''' get a list of the users files in JSON format
    '''
    @web.authenticated
    def get(self):
        cserver = self.get_server()
        filedict = cserver.get_files()
        json = jsonpickle.encode(filedict)
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
        self.write(json)

class OutputHandler(BaseHandler):
    ''' get any outstanding output from the model
    '''
    @web.authenticated
    def get(self):
        cserver = self.get_server()
        self.write(cserver.get_output())

class ProjectHandler(BaseHandler):
    ''' GET:  load model fom the given project archive,
              or reload remebered project for session if no file given
              
        POST: save project archive of the current project
    '''
    @web.authenticated
    def post(self):
        cserver = self.get_server()
        cserver.save_project()
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
            cserver.load_project(MEDIA_ROOT+'/'+filename)
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

class OutStreamHandler(websocket.WebSocketHandler):
    def open(self):
        sessionid = self.get_cookie("sessionid")
        cserver = self.application.server_mgr.console_server(sessionid)
        try:
            context = zmq.Context()
            socket = context.socket(zmq.SUB)
            addr = cserver.get_output_port()
            socket.connect(addr)
            socket.setsockopt(zmq.SUBSCRIBE, "")
            print 'OutStreamHandler.open() creating stream on',addr
            self.stream = ZMQStream(socket)
        except Exception, err:
            print '    error getting outstream:',err
            exc_type, exc_value, exc_traceback = sys.exc_info()
            traceback.print_exception(exc_type, exc_value, exc_traceback)
            traceback.print_tb(exc_traceback, limit=30)   
            if self.stream and not self.stream.closed():
                self.stream.close()
            self.close()
        else:
            self.stream.on_recv(self._write_message)
            cserver.publish_output()

    def _write_message(self, message):
        print 'OutStreamHandler._write_message()', message
        for part in message:
            self.write_message(part)
        
    def on_message(self, message):
        print 'outstream message received: %s' % message

    def on_close(self):
        self.timer.stop()
        print 'outstream connection closed'

class PlotStreamHandler(websocket.WebSocketHandler):
    def open(self):
        # FIXME: variables are hard coded for demo purposes
        var_names = [ 'prob.z1_t', 'prob.z2_t', 'prob.dis1.x1' ]
        
        sessionid = self.get_cookie("sessionid")
        cserver = self.application.server_mgr.console_server(sessionid)
        try:
            context = zmq.Context()
            socket = context.socket(zmq.SUB)
            addr = cserver.get_variables_port(var_names)
            socket.connect(addr)
            socket.setsockopt(zmq.SUBSCRIBE, "")
            print 'PlotStreamHandler.open() creating stream on',addr
            self.stream = ZMQStream(socket)
        except Exception, err:
            print '    error getting plot stream:',err
            exc_type, exc_value, exc_traceback = sys.exc_info()
            traceback.print_exception(exc_type, exc_value, exc_traceback)
            traceback.print_tb(exc_traceback, limit=30)   
            if self.stream and not self.stream.closed():
                self.stream.close()
            self.close()
        else:
            self.stream.on_recv(self._write_message)

    def _write_message(self, message):
        print 'PlotStreamHandler._write_message()', message
        for part in message:
            self.write_message(part)
        
    def on_message(self, message):
        print 'PlotStreamHandler message received: %s' % message

    def on_close(self):
        self.timer.stop()
        print 'PlotStreamHandler connection closed'

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
    web.url(r'/workspace/exit/?',           ExitHandler),
    web.url(r'/workspace/file/(.*)',        FileHandler),
    web.url(r'/workspace/files/?',          FilesHandler),
    web.url(r'/workspace/geometry',         GeometryHandler),
    web.url(r'/workspace/model/?',          ModelHandler),
    web.url(r'/workspace/output/?',         OutputHandler),
    web.url(r'/workspace/plot/?',           PlotHandler),
    web.url(r'/workspace/project/?',        ProjectHandler),
    web.url(r'/workspace/types/?',          TypesHandler),
    web.url(r'/workspace/upload/?',         UploadHandler),
    web.url(r'/workspace/workflow/(.*)',    WorkflowHandler),
    web.url(r'/workspace/test/?',           TestHandler),
    
    web.url(r'/workspace/outputWS/?',       OutStreamHandler),
    web.url(r'/workspace/plotWS/?',         PlotStreamHandler),
]
