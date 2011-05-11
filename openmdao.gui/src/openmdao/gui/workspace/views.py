from django.http import HttpResponse, HttpResponseRedirect, HttpResponsePermanentRedirect
from django.shortcuts import render_to_response, get_object_or_404
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt                                          
from django.contrib.auth import logout

import sys, os
import zipfile, jsonpickle

from setuptools.command import easy_install

from openmdao.main.factorymanager import get_available_types
from mdao_util import *

# TODO:
prefix = '<<<'+str(os.getpid())+'>>> '
print prefix+'workspace.views() -------------------------------------'
from consoleserverfactory import ConsoleServerFactory
server_mgr = ConsoleServerFactory()

@csrf_exempt
@login_required()
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
@login_required()
def Component(request,name):
    ''' add or remove a component
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    if request.POST:
        result = ''
        try:
            cserver.create(request.POST['type'],name);
        except Exception,e:
            print e
            result = sys.exc_info()
        return HttpResponse(result)
    elif request.method=='DELETE':
        result = ''
        try:
            result = cserver.onecmd('del '+request.name)
        except Exception,e:
            print e
            result = sys.exc_info()
        return HttpResponse(result)
    else:
        attr = cserver.get_attributes(name)
        json = jsonpickle.encode(attr)
        return HttpResponse(json,mimetype='application/json')

def Components(request):
    cserver = server_mgr.console_server(request.session.session_key)
    json = jsonpickle.encode(cserver.get_components())
    return HttpResponse(json,mimetype='application/json')

@login_required()
def Exec(request):
    ''' have the cserver execute a file, return result
    '''
    result = ''
    if request.POST:
        cserver = server_mgr.console_server(request.session.session_key)
        # if there is a filename, execute it & get the result
        if 'filename' in request.PUT:
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
    return HttpResponse(result)

@login_required()
def Exit(request):
    server_mgr.delete_server(request.session.session_key)
    logout(request)
    return HttpResponseRedirect('/')

@csrf_exempt
@login_required()
def File(request,filename):
    ''' get/set the specified file/folder
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    if request.method=='POST':
        if "isFolder" in request.POST:
            return HttpResponse(cserver.ensure_dir(filename))
        else:
            return HttpResponse(cserver.write_file(filename,request.POST["contents"]))
    elif request.method=='DELETE':
        return HttpResponse(cserver.delete_file(filename))
    elif request.method=='GET':
        return HttpResponse(cserver.get_file(filename))
    return HttpResponse("How did I get here?")

@login_required()
def Files(request):
    ''' get a list of the users files in JSON format
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    filedict = cserver.get_files()
    json = jsonpickle.encode(filedict)
    return HttpResponse(json,mimetype='application/json')

@csrf_exempt
@login_required()
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

@login_required()
def Output(request):
    ''' get any outstanding output from the model
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    return HttpResponse(cserver.get_output())

from openmdao.gui.settings import MEDIA_ROOT
@login_required()
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
        return HttpResponsePermanentRedirect('/workspace/')
    else:
        print "workspace.views() - GET Project"
        proj = cserver.get_project()
        response = HttpResponse(proj, mimetype='application/openmdao')
        response['Content-Disposition'] = 'attachment; filename='+proj.name+'.proj'
        return response
    
@login_required()
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
