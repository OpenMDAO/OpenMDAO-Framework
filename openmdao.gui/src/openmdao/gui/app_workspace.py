from django import forms

import sys, os, traceback
import zipfile, jsonpickle
from threading import Timer

import tornado.httpserver
import tornado.ioloop
import tornado.web

from openmdao.gui.util import *

from openmdao.gui.consoleserverfactory import ConsoleServerFactory
server_mgr = ConsoleServerFactory()

class AddonForm(forms.Form):
    distribution = forms.CharField(label='Distribution')
    
class AddOnsHandler(tornado.web.RequestHandler):
    ''' addon installation utility
    '''
    addons_url = 'http://openmdao.org/dists'
    addons_url = 'http://torpedo.grc.nasa.gov:31005/'
    
    def post(self):
        ''' easy_install the POST'd addon
        '''
        form = AddonForm(request.POST)
        if form.is_valid():
            distribution = form.cleaned_data['distribution']
            cserver = server_mgr.console_server(request.session.session_key)
            cserver.install_addon(addons_url, distribution)
            self.render('tmpl/workspace/closewindow.html')
            
    def get(self):
        ''' show available addons, prompt for addon to be installed
        '''
        form = AddonForm()
        self.render('tmpl/workspace/addons.html', 
                                  {'addons_url': addons_url, 'addon_form': form })
        
class GeometryHandler(tornado.web.RequestHandler):
    def get(self):
        ''' geometry viewer
        '''
        self.render('tmpl/workspace/o3dviewer.html',
                                  {'filename': request.GET['path'] })
 
class CloseHandler(tornado.web.RequestHandler):
    def get(self):
        server_mgr.delete_server(request.session.session_key)
        self.writeRedirect('/')
    
class CommandHandler(tornado.web.RequestHandler):
    ''' get the command, send it to the cserver, return response
    '''
    def post(self):
        cserver = server_mgr.console_server(request.session.session_key)
        history = ''
        # if there is a command, execute it & get the result
        if 'command' in request.POST:
            history = history + '>>> '+str(request.POST['command']) + '\n'
            result = ''
            try:
                result = cserver.onecmd(request.POST['command'])
            except Exception,e:
                print e
                result = sys.exc_info()
            if result:
                history = history + str(result) + '\n'
        self.write(history)
        
    def get(self):
        self.write('') # not used for now, could render a form

class ComponentHandler(tornado.web.RequestHandler):
    ''' add, remove or get a component
    '''
    def post(self,name):
        cserver = server_mgr.console_server(request.session.session_key)
        result = ''
        try:
            cserver.add_component(name,request.POST['type'],request.POST['parent']);
        except Exception,e:
            print e
            result = sys.exc_info()
        self.write(result)
        
    def delete(self,name):
        cserver = server_mgr.console_server(request.session.session_key)
        result = ''
        try:
            result = cserver.onecmd('del '+request.name)
        except Exception,e:
            print e
            result = sys.exc_info()
        self.write(result)
        
    def get(self,name):
        cserver = server_mgr.console_server(request.session.session_key)
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
        self.write(attr,mimetype='application/json')

class ComponentsHandler(tornado.web.RequestHandler):
    def get(self):
        cserver = server_mgr.console_server(request.session.session_key)
        json = cserver.get_components()
        self.write(json,mimetype='application/json')

class ConnectionsHandler(tornado.web.RequestHandler):
    ''' get/set connections between two components in an assembly
    '''
    def post(self,pathname):
        cserver = server_mgr.console_server(request.session.session_key)
        result = ''
        try:
            src_name = request.POST['src_name'];
            dst_name = request.POST['dst_name'];
            connections = request.POST['connections'];
            cserver.set_connections(pathname,src_name,dst_name,connections);
        except Exception,e:
            print e
            result = sys.exc_info()
        self.write(result)
        
    def get(self,pathname):
        cserver = server_mgr.console_server(request.session.session_key)
        connections = {}
        try:
            src_name = request.GET['src_name'];
            dst_name = request.GET['dst_name'];
            connections = cserver.get_connections(pathname,src_name,dst_name);
        except Exception, e:
            print e
        self.write(connections,mimetype='application/json')

class StructureHandler(tornado.web.RequestHandler):
    ''' get the structure of the specified assembly, or of the global 
        namespace if no pathname is specified, consisting of the list
        of components and the connections between them
    '''
    def get(self,name):
        cserver = server_mgr.console_server(request.session.session_key)
        json = cserver.get_structure(name)
        self.write(json,mimetype='application/json')

class ExecHandler(tornado.web.RequestHandler):
    ''' if a filename is POST'd, have the cserver execute the file
        otherwise just run() the project
    '''
    def post(self):
        result = ''
        cserver = server_mgr.console_server(request.session.session_key)
        # if there is a filename, execute it & get the result
        if 'filename' in request.POST:
            try:
                result = cserver.execfile(request.POST['filename'])
            except Exception,e:
                print e
                result = result + str(sys.exc_info()) + '\n'
                
        else:
            try:
                cserver.run()
            except Exception,e:
                print e
                result = result + str(sys.exc_info()) + '\n'
        self.write(result)

class ExitHandler(tornado.web.RequestHandler):
    ''' close the browser window and shut down the server
        (unfortunately neither of these things actually work)
    '''
    def get(self):
        server_mgr.delete_server(request.session.session_key)
        t = Timer(5, end_process) # Quit after 5 seconds
        self.render('templates/closewindow.html')
    
class FileHandler(tornado.web.RequestHandler):
    ''' get/set the specified file/folder
    '''
    def post(self,filename):
        cserver = server_mgr.console_server(request.session.session_key)
        if 'isFolder' in request.POST:
            self.write(cserver.ensure_dir(filename))
        else:
            self.write(cserver.write_file(filename,request.POST['contents']))
            
    def delete(self,filename):
        self.write(cserver.delete_file(filename))
        
    def get(self):
        self.write(cserver.get_file(filename))

class FilesHandler(tornado.web.RequestHandler):
    ''' get a list of the users files in JSON format
    '''
    def get(self):
        cserver = server_mgr.console_server(request.session.session_key)
        filedict = cserver.get_files()
        json = jsonpickle.encode(filedict)
        self.write(json,mimetype='application/json')

class LogoutHandler(tornado.web.RequestHandler):
    def get(self):
        server_mgr.delete_server(request.session.session_key)
        logout(request)
        self.writeRedirect('/')
    
class ModelHandler(tornado.web.RequestHandler):
    ''' POST: get a new model (delete existing console server)
        GET:  get JSON representation of the model
    '''
    def post(self):
        server_mgr.delete_server(session.session.session_key)
        self.writeRedirect('/')
        
    def get(self):
        cserver = server_mgr.console_server(request.session.session_key)
        json = cserver.get_JSON()
        self.write(json,mimetype='application/json')

class OutputHandler(tornado.web.RequestHandler):
    ''' get any outstanding output from the model
    '''
    def get(self):
        cserver = server_mgr.console_server(request.session.session_key)
        self.write(cserver.get_output())

from openmdao.gui.settings import MEDIA_ROOT

class ProjectHandler(tornado.web.RequestHandler):
    ''' GET:  load model fom the given project archive,
              or reload remebered project for session if no file given
              
        POST: save project archive of the current project
    '''
    def post(self):
        cserver = server_mgr.console_server(request.session.session_key)
        cserver.save_project()
        self.write('Saved.')
        
    def get(self):
        filename = None
        if 'filename' in request.GET:
            filename = request.GET['filename']
            request.session['filename'] = filename
        elif 'filename' in request.session.keys():
            filename = request.session.get('filename')
        if filename:
            print "Loading project into workspace:",filename
            server_mgr.delete_server(request.session.session_key) # delete old server
            cserver = server_mgr.console_server(request.session.session_key)        
            cserver.load_project(MEDIA_ROOT+'/'+filename)
            self.writeSeeOther(reverse('workspace.views.Workspace'))
        else:
            self.writeRedirect('/')

class PlotHandler(tornado.web.RequestHandler):
    ''' GET:  open a websocket server to supply updated valaues for the specified variable        
    '''
    def get(self,name):
        cserver = server_mgr.console_server(request.session.session_key)
        port = cserver.get_varserver(name)
        self.write(port)

class TopHandler(tornado.web.RequestHandler):
    ''' GET:  hmmm...
        POST: set top to the named assembly
    '''
    def post(self):
        cserver = server_mgr.console_server(request.session.session_key)
        if 'name' in request.POST:
            name = request.POST['name']
            try:
                cserver.set_top(name)
                print 'Top is now '+name
                self.write('Top is now '+name)
            except Exception,e:
                print 'Error settign top:',e
                self.write('Error setting top: '+e)
                
    def get(self):
        self.writeSeeOther(reverse('workspace.views.Workspace'))
        
class TypesHandler(tornado.web.RequestHandler):
    ''' get hierarchy of package/types to populate the Palette
    '''
    def get(self):
        cserver = server_mgr.console_server(request.session.session_key)
        types = cserver.get_available_types()
        try:
            types['working'] = cserver.get_workingtypes()
        except Exception, err:
            print "Error adding working types:", str(err)        
        self.write(jsonpickle.encode(types),mimetype='application/json')

class UploadHandler(tornado.web.RequestHandler):
    ''' file upload utility
    '''
    def post(self):
        cserver = server_mgr.console_server(request.session.session_key)
        if 'myfile' in request.FILES:
            file = request.FILES['myfile']
            filename = file.name
            if len(filename) > 0:
                contents = file.read()
                cserver.add_file(filename,contents)
                self.render('templates/closewindow.html')

    def get(self):
        self.render('tmpl/workspace/upload.html', 
                                  context_instance=RequestContext(request))

class WorkflowHandler(tornado.web.RequestHandler):
    def get(self,name):
        cserver = server_mgr.console_server(request.session.session_key)
        json = cserver.get_workflow(name)
        self.write(json,mimetype='application/json')
    
class WorkspaceHandler(tornado.web.RequestHandler):
    ''' render the workspace
    '''
    def get(self):
        self.render('tmpl/workspace/workspace.html')

class TestHandler(tornado.web.RequestHandler):
    ''' initialize the server manager &  render the workspace
    '''
    def get(self):
        self.render('tmpl/workspace/test.html')

                                  
handlers = [
    (r'/workspace/',                  WorkspaceHandler),
    (r'/workspace/components',        ComponentsHandler),
    (r'/workspace/component/(.*)',    ComponentHandler),
    (r'/workspace/connections/(.*)',  ConnectionsHandler),
    (r'/workspace/addons',            AddOnsHandler),
    (r'/workspace/close',             CloseHandler),
    (r'/workspace/command',           CommandHandler),
    (r'/workspace/structure/(.*)',    StructureHandler),
    (r'/workspace/exec',              ExecHandler),
    (r'/workspace/exit',              ExitHandler),
    (r'/workspace/file/(.*)',         FileHandler),
    (r'/workspace/files',             FilesHandler),
    (r'/workspace/geometry',          GeometryHandler),
    (r'/workspace/logout',            LogoutHandler),
    (r'/workspace/model',             ModelHandler),
    (r'/workspace/output',            OutputHandler),
    (r'/workspace/plot/(.*)',         PlotHandler),
    (r'/workspace/project',           ProjectHandler),
    (r'/workspace/top',               TopHandler),
    (r'/workspace/types',             TypesHandler),
    (r'/workspace/upload',            UploadHandler),
    (r'/workspace/workflow/(.*)',     WorkflowHandler),
    (r'/workspace/test',              TestHandler)
]


##
## START THE SERVER
##                                  

if __name__ == "__main__":
    app_settings = { 
        'debug': True,
        'static_path': os.path.join(os.path.dirname(__file__), "static"),
    }
    application = tornado.web.Application(handlers, **app_settings)
    http_server = tornado.httpserver.HTTPServer(application)
    http_server.listen(8888)
    tornado.ioloop.IOLoop.instance().start()