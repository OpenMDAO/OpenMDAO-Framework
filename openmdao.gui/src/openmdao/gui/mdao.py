import web
from web import form
from web import template

import webbrowser
import sys
import os
import time
import zipfile
import jsonpickle

from xml.etree.ElementTree import Element, SubElement, tostring
from xml.dom.minidom import Document

from multiprocessing import Process

from setuptools.command import easy_install

from consoleserverfactory import ConsoleServerFactory
from openmdao.main.factorymanager import *
from mdao_util import *


web.config.debug = False

# URL mapping
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
app = web.application(urls, globals(), True)

# workaround for debug mode... so multiples sessions are not created on server restart
if web.config.get('_session') is None:
    session = web.session.Session(app, web.session.DiskStore('sessions'))
    web.config._session = session
else:
    session = web.config._session

# renderer, pointing to html directory
render = web.template.render('static/html')

# custom 404 NOT FOUND message
def notfound():
    #return web.notfound(render.notfound())
    return web.notfound("Sorry, OpenMDAO does not recognize your request.")
app.notfound = notfound

# directory where files will be stored
# (there will be a subdirectory for each user)
filedir = os.getcwd()+'/files'
ensure_dir(filedir)

# start up a global mdao factory server
try:
    factory = ConsoleServerFactory()
except Exception, e:
    web.debug("failed to create mdao factory ")
    print e
    
# create a dictionary to keep track of cservers
# (since the references can't be pickled into the session)
cserver_dict = {}

#
# supply the OpenMDAO favicon
#
class Favicon:
    ''' return the icon from the images folder
    '''
    def GET(self):
        ico = open('static/images/favicon.ico','r')
        return ico.read();

#
# mdao controller
#
class MDAO:
    ''' if user is not logged in, redirect to the login form
        otherwise, make sure we have a server then render the GUI 
    '''
    def GET(self):
        if not hasattr(session, "user") :
            web.redirect('/login')
        else:
            web.debug("MDAO user: " + str(session.user))
            if not cserver_dict.has_key(session.session_id):
                cserver = factory.create('mdao-'+str(session.user)+'-'+(session.session_id))
                cserver_dict[session.session_id] = cserver;
                web.debug("created mdao cserver: " + str(cserver_dict[session.session_id].get_pid()))
            else:
                cserver = cserver_dict[session.session_id]
            userdir = filedir +'/'+ session.user
            ensure_dir(userdir)
            cserver.chdir(userdir)
            return render.mdao("OpenMDAO: "+session.user)

#			
# login controller
#
class Login:
    loginForm = form.Form(
        form.Textbox('username'),
        form.Password('password'),
        form.Button('submit'),
    )
    
    ''' clear the session and render the login form 
    '''
    def GET(self):
        if session.has_key('user'):
            web.debug("logging out user: " + str(session.user))
            web.setcookie('user', '', 'Mon, 01-Jan-2000 00:00:00 GMT')
            if cserver_dict.has_key(session.session_id):
                cserver = cserver_dict[session.session_id]
                del cserver_dict[session.session_id]
                del cserver
        form = self.loginForm()
        return render.login(form)

    ''' get login information, set user and go to main page 
    '''
    def POST(self):
        x = web.input()
        web.debug("getting login info")
        web.debug("Username: " + str(x.username))
        web.debug("Password: " + str(x.password))
        session.user = x.username		
        web.redirect('/')

#			
# add component controller
#
class AddComponent:
    addForm = form.Form(
        form.Textbox('type'),
        form.Textbox('name'),
        form.Button('submit'),
    )
    
    ''' render the add component form 
    '''
    def GET(self):
        web.debug("adding component")
        form = self.addForm()
        x = web.input()
        if hasattr(x, "type"):
            web.debug("type: " + str(x.type))
            form['type'].value = str(x.type)
        return render.addcomponent(form)

    ''' get component type and name, add to model
    '''
    def POST(self):
        x = web.input()
        web.debug("adding component")
        web.debug("type: " + str(x.type))
        web.debug("name: " + str(x.name))
        web.debug("x: " + str(x.x))
        web.debug("y: " + str(x.y))
        cserver = cserver_dict[session.session_id]
        try:
            cserver.create(str(x.type),x.name);
        except Exception,e:
            print e
            result = sys.exc_info()
            web.debug("result: " + str(result))
            
#
# command controller		
#
class Command:
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
        cserver = cserver_dict[session.session_id]
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

#
# exec controller		
#
class Exec:
    ''' have the cserver execute a file, return response
    '''
    def POST(self):
        cserver = cserver_dict[session.session_id]
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

#
# output controller		
#
class Output:
    ''' get any outstanding output from the model
    '''
    def GET(self):
        cserver = cserver_dict[session.session_id]
        return cserver.get_output()
        
#
# new model controller		
#
class NewModel:
    ''' quit() existing server and get a new one
    '''
    def GET(self):
        cserver = cserver_dict[session.session_id]
        cserver.cleanup()
        del cserver_dict[session.session_id]
        del cserver
        web.redirect('/')

#
# exit controller		
#
class Exit:
    ''' quit() server 
    '''
    def GET(self):
        render.closewindow()
        if cserver_dict.has_key(session.session_id):
            cserver = cserver_dict[session.session_id]
            web.debug("cleanup on aisle " + str(cserver.get_pid()))
            cserver.cleanup();
        session.kill()
        quit(0)
        
#
# upload controller		
#
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
            ensure_dir(filedir)
            userdir = filedir +'/'+ session.user;
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

#
# modeljson controller		
#
class ModelJSON:
    ''' get a JSON representation of the model
    '''
    def GET(self):
        cserver = cserver_dict[session.session_id]
        json = cserver.get_JSON()
        # web.debug("Model JSON:")
        # web.debug(json)
        web.header('Content-Type', 'application/json')
        return json

#
# files controller		
#
class FilesXML:
    ''' get a list of the users files
    '''
    def GET(self):
        cserver = cserver_dict[session.session_id]
        root = cserver.getcwd()
        doc = Document()
        doc.appendChild(makenode(doc,root))
        return doc.toprettyxml().replace(root,'')
    
#
# files controller		
#
class FilesJSON:
    ''' get a list of the users files
    '''
    def GET(self):
        cserver = cserver_dict[session.session_id]
        root = cserver.getcwd()
        dict = filepathdict(root)
        json = jsonpickle.encode(dict)
        root = root.replace('\\','\\\\')  # TODO: investigate this
        json = json.replace(root,'')
        web.header('Content-Type', 'application/json')
        return json

#
# folder controller
#
class Folder:
    ''' get/set the current working directory for the cserver
    '''
    def GET(self):
        cserver = cserver_dict[session.session_id]
        return cserver.getcwd()
    def POST(self):
        cserver = cserver_dict[session.session_id]
        x = web.input()
        web.debug("Folder: " + str(x.folder))
        userdir = filedir +'/'+ session.user
        ensure_dir(userdir)
        ensure_dir(userdir+x.folder)
        cserver.chdir(userdir+x.folder)

#
# file controller
#
class File:
    ''' get/set the specified file
    '''
    def GET(self):
        cserver = cserver_dict[session.session_id]
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
        ensure_dir(filedir)
        userdir = filedir +'/'+ session.user;
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

#
# remove controller (remove a file or an object)
#
class Remove:
    ''' remove the specified file or object
    '''
    def GET(self):
        x = web.input()
        if hasattr(x,'file'):
            web.debug("removing file: " + str(x.file))
            userdir = filedir+'/'+session.user
            filepath = userdir+'/'+str(x.file)
            if os.path.exists(filepath):
                if os.path.isdir(filepath):
                    os.rmdir(filepath)
                else:
                    os.remove(filepath)
            else:
                return web.notfound("Sorry, the file was not found.")
        elif hasattr(x,'objname'):
            web.debug("removing object: " + str(x.objname))
            cserver = cserver_dict[session.session_id]
            result = ''
            try:
                result = cserver.onecmd('del '+x.objname)
            except Exception,e:
                print e
                result = sys.exc_info()
            return result
        else:
            web.debug("what am I suppsed to remove?!")
            print_dict(x)

#
# types controller		
#
class Types:
    ''' get a list of object types that the user can create
    '''
    def GET(self):
        xml = '<?xml version=\"1.0\"?>\n'
        xml = xml + '<response>\n'
        typeTree = Element("Types")
        # get the installed types
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
        cserver = cserver_dict[session.session_id]
        types = cserver.get_workingtypes()
        for t in types:
            typeElem = SubElement(parent,"Type")
            typeElem.set("name",t)
            typeElem.set("path",t)
        xml = xml + tostring(typeTree)
        xml = xml + '</response>\n'
        return xml

#			
# addon controller
#
class AddOn:
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
        web.debug("adding: " + str(x.Distribution))
        url = 'http://openmdao.org/dists'
        easy_install.main( ["-U","-f",url,x.Distribution] )
        return render.closewindow()

#
# launch server on specified port
#
def launch_server(port):
    ''' run the web server
    '''
    pid = os.getpid()
    web.debug('Running server on port: '+str(port)+' (pid='+str(pid)+')')
    web.httpserver.runsimple(app.wsgifunc(), ("0.0.0.0", port)) 

#
# launch browser on specified port
#
def launch_browser(port):
    ''' try to use preferred browser, fall back to default 
        TODO: use config/preferences to select browser
    '''
    pid = os.getpid()
    url = 'http://localhost:'+str(port)    
    web.debug('Opening URL in browser: '+url+' (pid='+str(pid)+')')
    
    #preferred_browser = None;
    #preferred_browser = 'firefox';
    preferred_browser = 'chrome';
    
    # webbrowser doesn't know about chrome, so try to find it (this is for win7)
    if preferred_browser and preferred_browser.lower() == 'chrome':
        web.debug('Trying to find Google Chrome...')
        USERPROFILE = os.getenv("USERPROFILE").replace('\\','/')
        CHROMEPATH = USERPROFILE+'/AppData/Local/Google/Chrome/Application/chrome.exe'
        if os.path.isfile(CHROMEPATH):
            preferred_browser = CHROMEPATH+' %s'
    
    # try to get preferred browser, fall back to default
    try:
        browser = webbrowser.get(preferred_browser);
    except:
        print "Couldn't launch preferred browser ("+preferred_browser+"), using default..."
        browser = webbrowser.get()
    
    # open new browser window (may open in a tab depending on user preferences, etc.)
    if browser:
            browser.open(url,1,True)
    else:
        print "Couldn't launch browser: "+str(browser)
        
web.webapi.internalerror = web.debugerror 

#
# main.  run the server & open the page in web browser.
#
if __name__ == "__main__":
    port = PickUnusedPort()    
    server = Process(target=launch_server,args=(port,)).start()
    client = Process(target=launch_browser,args=(port,)).start()

