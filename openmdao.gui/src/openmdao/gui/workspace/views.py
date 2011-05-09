from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response, get_object_or_404
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt                                          
from django.contrib.auth import logout

import sys, os
import zipfile, jsonpickle

from setuptools.command import easy_install

from server_manager import ServerManager
from openmdao.main.factorymanager import get_available_types
from mdao_util import *

# TODO:
prefix = '<<<'+str(os.getpid())+'>>> '
print prefix+'workspace.views() -------------------------------------'
from server_manager import ServerManager
server_mgr = ServerManager()

@csrf_exempt
def Command(request):
    ''' get the command, send it to the cserver, return response
    '''
    if request.POST:
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
        return HttpResponse(history)
    else:
        return HttpResponse('') # not used for now, could render a form

@csrf_exempt
def Component(request,name):
    ''' add or remove a component
    '''
    print request
    if request.POST:
        cserver = server_mgr.console_server(request.session.session_key)
        result = ''
        try:
            cserver.create(request.POST['type'],name);
        except Exception,e:
            print e
            result = sys.exc_info()
        return HttpResponse(result)
    elif request.DELETE:
        cserver = server_mgr.console_server(request.session.session_key)
        result = ''
        try:
            result = cserver.onecmd('del '+request.name)
        except Exception,e:
            print e
            result = sys.exc_info()
        return result

def CWD(request):
    ''' get/set the current working directory for the cserver
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    if request.PUT:
        userdir = server_mgr.get_tempdir('files');
        ensure_dir(userdir+request.PUT['folder'])
        cserver.chdir(userdir+request.PUT['folder'])
    return HttpResponse(cserver.getcwd())

def Exec(request):
    ''' have the cserver execute a file, return result
    '''
    history = ''
    if request.PUT:
        cserver = server_mgr.console_server(request.session.session_key)
        result = ''
        # if there is a filename, execute it & get the result
        if 'filename' in request.PUT:
            try:
                result = cserver.execfile(request.PUT['filename'])
            except Exception,e:
                print e
                result = sys.exc_info()
            if result:
                history = history + str(result) + '\n'
        else:
            result = ''
    return HttpResponse(history)

def Exit(request):
    server_mgr.delete_server(request.session.session_key)
    logout(request)
    return HttpResponseRedirect('/')

@csrf_exempt
def File(request,filename):
    ''' get/set the specified file/folder
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    filepath = cserver.getcwd()+'/'+str(filename)
    if request.POST:
        if "isFolder" in request.POST:
            ensure_dir(filepath)
            return HttpResponse("Folder created")
        else:
            fout = open(filepath,'wb')
            if "contents" in request.POST:
                fout.write(request.POST["contents"])
            fout.close()
            return HttpResponse("File created")
    elif request.method=='DELETE':
        if os.path.exists(filepath):
            if os.path.isdir(filepath):
                    os.rmdir(filepath)
                    return HttpResponse("File deleted")
            else:
                    os.remove(filepath)
                    return HttpResponse("Folder deleted")
        else:
            return HttpResponse("Sorry, the file was not found.")
    elif request.GET:
        if os.path.exists(filepath):
            f=open(filepath, 'r')
            return HttpResponse(f.read())
        else:
            return HttpResponse("Sorry, the file was not found.")

def Files(request):
    ''' get a list of the users files in JSON format
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    root = cserver.getcwd()
    print "Files: root="+root
    dict = filepathdict(root)
    json = jsonpickle.encode(dict)
    root = root.replace('\\','\\\\')  # TODO: investigate this
    json = json.replace(root,'')
    return HttpResponse(json,mimetype='application/json')

def Model(request):
    ''' GET: get JSON representation of the model
        POST: get a new model (delete existing console server)
    '''
    if request.POST:
        server_mgr.delete_server(session.session.session_key)
        return HttpResponseRedirect('/')
    else:
        cserver = server_mgr.console_server(request.session.session_key)
        json = cserver.get_JSON()
        return HttpResponse(json,mimetype='application/json')

def Output(request):
    ''' get any outstanding output from the model
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    return HttpResponse(cserver.get_output())

from openmdao.gui.settings import MEDIA_ROOT
def Project(request):
    ''' GET: get a project archive of the current model
        POST: load model fom the given project archive
    '''
    server_mgr.delete_server(request.session.session_key) # delete old server
    cserver = server_mgr.console_server(request.session.session_key)
    print prefix+'workspace.views() Project: ------------------'
    if request.POST:
        print "workspace.views() - POST Project: "+request.POST['filename']
        filepath = MEDIA_ROOT+'/'+request.POST['filename']
        print prefix+'workspace.views() Project: loading '+filepath
        cserver.load_project(filepath)
        print "workspace.views() - Project: done, redirecting to workspace..."
        return HttpResponseRedirect('workspace')
    else:
        print "workspace.views() - GET Project"
        proj = cserver.get_project()
        response = HttpResponse(proj, mimetype='application/openmdao')
        response['Content-Disposition'] = 'attachment; filename='+proj.name()+'.proj'
        return response
    
def Types(request):
    ''' get hierarchy of package/types to populate the Palette
    '''
    types = get_available_types()
    types = packagedict(types)
    cserver = server_mgr.console_server(request.session.session_key)
    types['working'] = packagedict(cserver.get_workingtypes())
    json = jsonpickle.encode(types)
    return HttpResponse(json,mimetype='application/json')

@login_required()
def Workspace(request):
    ''' initialize the server manager &  render the workspace
    '''
    return render_to_response('workspace.html',
                              context_instance=RequestContext(request))
