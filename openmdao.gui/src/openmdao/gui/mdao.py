import web
from web import form
from web import template

import sys, os
import zipfile, jsonpickle

from xml.etree.ElementTree import Element, SubElement, tostring
from xml.dom.minidom import Document

from setuptools.command import easy_install

from server_manager import ServerManager
from openmdao.main.factorymanager import get_available_types
from mdao_util import *

prefix = '<<<'+str(os.getpid())+'>>> '

web.config.debug = False
web.webapi.internalerror = web.debugerror 

def run_server(port):
    ''' run the web server
    '''
    
    # URL mapping
    global urls
    urls = ('/',            'MDAO',
            '/add',         'AddComponent',
            '/addon',       'AddOn',
            '/command',     'Command',
            '/exec',        'Exec',
            '/exit',        'Exit',
            '/favicon.ico', 'Favicon',
            '/file',        'File',
            '/files.xml',   'FilesXML',
            '/files.json',  'FilesJSON',
            '/folder',      'Folder',
            '/login',       'Login',
            '/model.json',  'ModelJSON',
            '/new',         'NewModel',
            '/output',      'Output',
            '/remove',      'Remove',
            '/types',       'Types',
            '/upload',      'Upload')

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

class MDAO:
    ''' if user is not logged in, redirect to the login form
        otherwise, make sure we have a server then render the GUI 
    '''
    def GET(self):
        if not hasattr(session, "user") :
            web.redirect('/login')
        else:
            userdir = server_mgr.get_tempdir('files') +'/'+ session.user;
            ensure_dir(userdir)
            server_mgr.console_server(session.session_id).chdir(userdir)
            return render.mdao("OpenMDAO: "+session.user)

class Login:
    ''' login controller
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

class AddComponent:
    ''' add component controller
    '''
    addForm = form.Form(
        form.Textbox('type'),
        form.Textbox('name'),
        form.Button('submit'),
    )
    
    ''' render the add component form 
    '''
    def GET(self):
        form = self.addForm()
        x = web.input()
        if hasattr(x, "type"):
            form['type'].value = str(x.type)
        return render.addcomponent(form)

    ''' get component type and name, add to model
    '''
    def POST(self):
        x = web.input()
        cserver = server_mgr.console_server(session.session_id)
        try:
            cserver.create(str(x.type),x.name);
        except Exception,e:
            print e
            result = sys.exc_info()
            
class Command:
    ''' command controller
    '''
    commandForm = form.Form( 
        form.Textbox('command'),
        form.Button('submit'),
    )
    
    ''' render the command form
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
    def POST(self):
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
        
class NewModel:
    ''' delete existing console server and get a new one
    '''
    def GET(self):
        server_mgr.delete_server(session.session_id)
        web.redirect('/')

class Exit:
    ''' exit
    '''
    def GET(self):
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

class ModelJSON:
    ''' get a JSON representation of the model
    '''
    def GET(self):
        cserver = server_mgr.console_server(session.session_id)
        json = cserver.get_JSON()
        web.header('Content-Type', 'application/json')
        return json

class FilesXML:
    ''' get a list of the users files in XML format
    '''
    def GET(self):
        cserver = server_mgr.console_server(session.session_id)
        root = cserver.getcwd()
        doc = Document()
        doc.appendChild(makenode(doc,root))
        return doc.toprettyxml().replace(root,'')
    
class FilesJSON:
    ''' get a list of the users files in JSON format
    '''
    def GET(self):
        cserver = server_mgr.console_server(session.session_id)
        root = cserver.getcwd()
        dict = filepathdict(root)
        json = jsonpickle.encode(dict)
        root = root.replace('\\','\\\\')  # TODO: investigate this
        json = json.replace(root,'')
        web.header('Content-Type', 'application/json')
        return json

class Folder:
    ''' get/set the current working directory for the cserver
    '''
    def GET(self):
        cserver = server_mgr.console_server(session.session_id)
        return cserver.getcwd()
        
    def POST(self):
        cserver = server_mgr.console_server(session.session_id)
        x = web.input()
        userdir = server_mgr.get_tempdir('files') +'/'+ session.user;
        ensure_dir(userdir+x.folder)
        cserver.chdir(userdir+x.folder)

class File:
    ''' get/set the specified file
    '''
    def GET(self):
        cserver = server_mgr.console_server(session.session_id)
        x = web.input()
        filepath = cserver.getcwd()+'/'+str(x.file)
        if os.path.exists(filepath):
            f=open(filepath, 'r')
            return f.read()
        else:
            return web.notfound("Sorry, the file was not found.")

    ''' if "isFolder" is specified, create the folder, else
        create and write the posted contents to the specified file
    '''
    def POST(self):
        x = web.input()
        userdir = server_mgr.get_tempdir('files') +'/'+ session.user;
        ensure_dir(userdir)
        filepath=x.filename.replace('\\','/')
        filepath = userdir +'/'+ filepath
        if hasattr(x, "isFolder"):
            ensure_dir(filepath)
        else:
            fout = open(filepath,'wb')
            if hasattr(x, "contents"):
                fout.write(x.contents)
            fout.close()

class Remove:
    ''' remove the specified file or object
    '''
    def POST(self):
        x = web.input()
        if hasattr(x,'file'):
            userdir = server_mgr.get_tempdir('files') +'/'+ session.user;
            filepath = userdir+'/'+str(x.file)
            if os.path.exists(filepath):
                if os.path.isdir(filepath):
                    os.rmdir(filepath)
                else:
                    os.remove(filepath)
            else:
                return web.notfound("Sorry, the file was not found.")
        elif hasattr(x,'objname'):
            cserver = server_mgr.console_server(session.session_id)
            result = ''
            try:
                result = cserver.onecmd('del '+x.objname)
            except Exception,e:
                print e
                result = sys.exc_info()
            return result
        else:
            web.debug(prefix+"what am I suppsed to remove?!")
            print_dict(x)

class Types:
    ''' get a list of object types that the user can create
    '''
    def GET(self):
        xml = '<?xml version=\"1.0\"?>\n'
        xml = xml + '<response>\n'
        typeTree = Element("Types")
        # get the installed types
        server_mgr.console_server(session.session_id)
        types = get_available_types()
        for t in types:
            path = t[0].split('.');
            last = path[len(path)-1]
            parent = typeTree
            for node in path:
                if not node==last:
                    # it's a package name, see if we have it already
                    existingElem = None
                    packages = parent.findall('Package')
                    for p in packages:
                        if p.get("name") == node:
                            existingElem = p
                    # set the parent to this package
                    if existingElem is None:
                        pkgElem = SubElement(parent,"Package")
                        pkgElem.set("name",node)
                        parent = pkgElem
                    else:
                        parent = existingElem
                else:
                    # it's the class name, add it under current package
                    typeElem = SubElement(parent,"Type")
                    typeElem.set("name",node)
                    typeElem.set("path",t[0])
        # get the "working" types
        pkgElem = SubElement(typeTree,"Package")
        pkgElem.set("name","working")
        parent = pkgElem
        cserver = server_mgr.console_server(session.session_id)
        types = cserver.get_workingtypes()
        for t in types:
            typeElem = SubElement(parent,"Type")
            typeElem.set("name",t)
            typeElem.set("path",t)
        xml = xml + tostring(typeTree)
        xml = xml + '</response>\n'
        return xml

class AddOn:
    ''' addon installation utility
    '''
    addonForm = form.Form( 
        form.Textbox('Distribution'),
        form.Button('Install'),
    )
    
    ''' prompt for addon to be installed
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
