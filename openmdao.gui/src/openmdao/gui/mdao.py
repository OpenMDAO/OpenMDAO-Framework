import web
from web import form
from web import template

import sys, os, getpass
import zipfile, jsonpickle

from setuptools.command import easy_install

from server_manager import ServerManager
from openmdao.main.factorymanager import get_available_types
from mdao_util import *

# from mdao_setup import createDB
# db = 'mdao.db'

PREFIX = '<<<'+str(os.getpid())+'>>> '

web.config.debug = False
web.webapi.internalerror = web.debugerror 

single_user = True

def run_server(port):
    ''' run the web server
    '''
    
    # URL mapping
    global urls
    urls = ('/',                'Workspace',
            '/component/(.*)',  'Component',
            '/addons',          'AddOns',
            '/command',         'Command',
            '/exec',            'Exec',
            '/exit',            'Exit',
            '/favicon.ico',     'Favicon',
            '/file/(.*)',       'File',
            '/files',           'Files',
            '/cwd',             'CWD',
            '/login',           'Login',
            '/model',           'Model',
            '/output',          'Output',
            '/types',           'Types',
            '/upload',          'Upload')

    # create the app
    global app
    app = web.application(urls, globals(), True)

    # the singleton server manager handles console servers, temporary files, etc.
    global server_mgr
    server_mgr = ServerManager()

    # workaround for debug mode... so multiples sessions are not created on server restart
    global session
    if web.config.get('_session') is None:
        session_dir = server_mgr.get_tempdir('session')
        session = web.session.Session(app, web.session.DiskStore(session_dir))
        web.config._session = session
    else:
        session = web.config._session
        
    # renderer, pointing to html directory
    global render
    render = web.template.render('static/html')

    # custom 404 NOT FOUND message
    def notfound():
        #return web.notfound(render.notfound())
        return web.notfound("Sorry, OpenMDAO does not recognize your request.")
    app.notfound = notfound
    
    # run server, clean up on exit
    try:
        web.httpserver.runsimple(app.wsgifunc(), ("0.0.0.0", port))     
    except Exception,e:
        web.debug(prefix+'mdao shutdown after exception: '+e)
    finally:
        server_mgr.cleanup()    
    
class Favicon:
    ''' return the favicon from the images folder
    '''
    def GET(self):
        ico = open('static/images/favicon.ico','r')
        return ico.read();

        
class Workspace:
    ''' if user is not logged in & not in single user mode, redirect to login form
        otherwise, make sure we have a server then render the GUI 
    '''
    def GET(self):
        if single_user:
            session.user = getpass.getuser()
            
        if not hasattr(session, "user"):
            # if not os.path.exists(db):
                # web.debug(PREFIX+'User/Project database not found, creating...')
                # createDB(db)
            web.redirect('/login')
        else:
            userdir = server_mgr.get_tempdir('files') +'/'+ session.user;
            ensure_dir(userdir)
            server_mgr.console_server(session.session_id).chdir(userdir)
            return render.mdao("OpenMDAO: "+session.user)

class Login:
    ''' login 
    '''
    loginForm = form.Form(
        form.Textbox('username'),
        form.Password('password'),
        form.Button('submit'),
    )
   
    ''' clear the session and render the login form 
    '''
    def GET(self):
        if session.has_key('user'):
            web.setcookie('user', '', 'Mon, 01-Jan-2000 00:00:00 GMT')
            server_mgr.delete_server(session.session_id)
        form = self.loginForm()
        return render.login(form)

    ''' get login information, set user and go to main page 
    '''
    def POST(self):
        x = web.input()
        session.user = x.username
        web.redirect('/')

class Component:
    ''' component 
    '''
    addForm = form.Form(
        form.Textbox('type'),
        form.Textbox('name'),
        form.Button('submit'),
    )
    
    ''' render the add component form
        TODO: this is not a RESTful get... figure out a better way
    '''
    def GET(self,name):
        form = self.addForm()
        x = web.input()
        if hasattr(x, "type"):
            form['type'].value = str(x.type)
        return render.addcomponent(form)

    ''' add new component to model
    '''
    def POST(self,name):
        x = web.input()
        cserver = server_mgr.console_server(session.session_id)
        try:
            cserver.create(str(x.type),name);
        except Exception,e:
            print e
            result = sys.exc_info()
            
    ''' remove the specified component
    '''
    def DELETE(self,name):
        x = web.input()
        if hasattr(x,'objname'):
            cserver = server_mgr.console_server(session.session_id)
            result = ''
            try:
                result = cserver.onecmd('del '+name)
            except Exception,e:
                print e
                result = sys.exc_info()
            return result
            
class Command:
    ''' command 
    '''
    commandForm = form.Form( 
        form.Textbox('command'),
        form.Button('submit'),
    )
    
    ''' render the command form
        TODO: this is not a RESTful get... probabaly ok though?
    '''
    def GET(self):
        form = self.commandForm()
        return render.commandline(form)

    ''' get the command, send it to the cserver, return response
    '''
    def POST(self):
        cserver = server_mgr.console_server(session.session_id)
        x = web.input()
        history = ''
        result = ''
        # if there is a command, execute it & get the result
        if x.command:
            try:
                result = cserver.onecmd(x.command)
            except Exception,e:
                print e
                result = sys.exc_info()
            history = history + '>>> '+str(x.command) + '\n'
            if result:
                history = history + str(result) + '\n'
        else:
            command = ''
        return history

class Exec:
    ''' have the cserver execute a file, return response
    '''
    def PUT(self):
        cserver = server_mgr.console_server(session.session_id)
        x = web.input()
        history = ''
        result = ''
        # if there is a filename, execute it & get the result
        if x.filename:
            try:
                result = cserver.execfile(x.filename)
            except Exception,e:
                print e
                result = sys.exc_info()
            if result:
                history = history + str(result) + '\n'
        else:
            result = ''
        return history

class Output:
    ''' get any outstanding output from the model
    '''
    def GET(self):
        cserver = server_mgr.console_server(session.session_id)
        return cserver.get_output()
        
class Model:
    ''' delete existing console server and get a new one
    '''
    def POST(self):
        server_mgr.delete_server(session.session_id)
        web.redirect('/')

class Exit:
    ''' exit
    '''
    def PUT(self):
        render.closewindow()
        server_mgr.delete_server(session.session_id)
        session.kill()
        sys.exit(0)

class Upload:
    ''' display the upload form (file chooser)
        this is meant to be rendered in a new window
    '''
    def GET(self):
        return render.upload()

    ''' save the contents of the posted file to working directory
    '''
    def POST(self):
        x = web.input(myfile={})
        if 'myfile' in x:
            # first upload the file
            userdir = server_mgr.get_tempdir('files') +'/'+ session.user;
            ensure_dir(userdir)
            filepath=x.myfile.filename.replace('\\','/') # replace the back-slashes with slashes
            filename=filepath.split('/')[-1]             # get the last part (filename with extension)
            filename = userdir +'/'+ filename
            fout = open(filename,'wb')                   # create the file
            fout.write(x.myfile.file.read())             # write uploaded data to the file.
            fout.close()                                 # close the file, upload complete.
            
            # now if the file was a zip file, extract the contents
            if zipfile.is_zipfile(filename):
                zfile = zipfile.ZipFile( filename, "r" )
                zfile.printdir()
                for fname in zfile.namelist():
                    if fname.endswith('/'):
                        dirname = userdir+'/'+fname
                        if not os.path.exists(dirname):
                            os.makedirs(dirname)
                for fname in zfile.namelist():
                    if not fname.endswith('/'):
                        data = zfile.read(fname)
                        fname = userdir+'/'+fname
                        fname = fname.replace('\\','/')
                        fout = open(fname, "wb")
                        fout.write(data)
                        fout.close()
                zfile.close()
                os.remove(filename)
                    
        return render.closewindow()

class Model:
    ''' get a JSON representation of the model
    '''
    def GET(self):
        cserver = server_mgr.console_server(session.session_id)
        json = cserver.get_JSON()
        web.header('Content-Type', 'application/json')
        return json
        
    ''' delete existing console server and get a new one
    '''
    def POST(self):
        server_mgr.delete_server(session.session_id)
        web.redirect('/')

class Files:
    ''' get a list of the users files in JSON format
    '''
    def GET(self):
        cserver = server_mgr.console_server(session.session_id)
        root = cserver.getcwd()
        web.debug("Files: root="+root)
        x = web.input()
        print_dict(x)
        if False: # TODO: check the requested datatype, maybe they wany XML?
            doc = Document()
            doc.appendChild(makenode(doc,root))
            return doc.toprettyxml().replace(root,'')
        else:
            dict = filepathdict(root)
            json = jsonpickle.encode(dict)
            root = root.replace('\\','\\\\')  # TODO: investigate this
            json = json.replace(root,'')
            web.header('Content-Type', 'application/json')
            return json

class CWD:
    ''' get/set the current working directory for the cserver
    '''
    def GET(self):
        cserver = server_mgr.console_server(session.session_id)
        return cserver.getcwd()
        
    def PUT(self):
        cserver = server_mgr.console_server(session.session_id)
        x = web.input()
        userdir = server_mgr.get_tempdir('files') +'/'+ session.user;
        ensure_dir(userdir+x.folder)
        cserver.chdir(userdir+x.folder)

class File:
    ''' get/set the specified file
    '''
    def GET(self,filename):
        web.debug("FILE GET "+filename)
        cserver = server_mgr.console_server(session.session_id)
        filepath = cserver.getcwd()+'/'+str(filename)
        if os.path.exists(filepath):
            f=open(filepath, 'r')
            return f.read()
        else:
            return web.notfound("Sorry, the file was not found.")

    ''' if "isFolder" is specified, create the folder, else
        create and write the posted contents to the specified file
    '''
    def POST(self,filename):
        web.debug("FILE POST "+filename)
        userdir = server_mgr.get_tempdir('files') +'/'+ session.user;
        ensure_dir(userdir)
        filepath = userdir +'/'+ filename
        x = web.input()
        if hasattr(x, "isFolder"):
            ensure_dir(filepath)
        else:
            fout = open(filepath,'wb')
            if hasattr(x, "contents"):
                fout.write(x.contents)
            fout.close()
            
    '''  remove the specified file
    '''
    def DELETE(self,filename):
        web.debug("FILE DELETE "+filename)
        userdir = server_mgr.get_tempdir('files') +'/'+ session.user;
        filepath = userdir+'/'+str(filename)
        web.debug("DELETING FILE"+filepath)
        if os.path.exists(filepath):
            web.debug("DELETING FILE (exists)"+filepath)
            if os.path.isdir(filepath):
                    web.debug("DELETING FILE (folder)"+filepath)
                    os.rmdir(filepath)
            else:
                    web.debug("DELETING FILE (file)"+filepath)
                    os.remove(filepath)
        else:
            return web.notfound("Sorry, the file was not found.")

class Types:
    ''' get a list of object types that the user can create
    '''
    def GET(self):
        types = get_available_types()
        types = packagedict(types)
        
        cserver = server_mgr.console_server(session.session_id)
        types['working'] = packagedict(cserver.get_workingtypes())
        
        web.header('Content-Type', 'application/json')
        return jsonpickle.encode(types)

class AddOns:
    ''' addon installation utility
    '''
    addonForm = form.Form( 
        form.Textbox('Distribution'),
        form.Button('Install'),
    )
    
    ''' show available addons, prompt for addon to be installed
    '''
    def GET(self):
        form = self.addonForm()
        return render.addon(form)

    ''' easy_install the POST'd addon
    '''
    def POST(self):
        x = web.input()
        url = 'http://openmdao.org/dists'
        easy_install.main( ["-U","-f",url,x.Distribution] )
        return render.closewindow()

if __name__ == "__main__":
    ''' pick an open port, launch web browser and run the server on that port
    '''
    port = PickUnusedPort()    
    launch_browser(port)
    run_server(port)
