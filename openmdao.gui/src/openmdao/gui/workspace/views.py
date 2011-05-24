from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response, get_object_or_404
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt                                          
from django.contrib.auth import logout
from django.views.decorators.cache import never_cache
from django.core.urlresolvers import reverse

import sys, os
import zipfile, jsonpickle

from setuptools.command import easy_install

from mdao_util import *

# TODO:
prefix = '<<<'+str(os.getpid())+'>>> '
print prefix+'workspace.views() -------------------------------------'
from consoleserverfactory import ConsoleServerFactory
server_mgr = ConsoleServerFactory()

@never_cache
@csrf_exempt
@login_required()
def Command(request):
    ''' get the command, send it to the cserver, return response
    '''
    if request.method=='POST':
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

@never_cache
@csrf_exempt
@login_required()
def Component(request,name):
    ''' add, remove or get a component
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    if request.method=='POST':
        result = ''
        try:
            cserver.add_component(name,request.POST['type']);
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

@never_cache
def Components(request):
    cserver = server_mgr.console_server(request.session.session_key)
    json = jsonpickle.encode(cserver.get_components())
    return HttpResponse(json,mimetype='application/json')

@never_cache
@csrf_exempt
@login_required()
def Exec(request):
    ''' if a filename is POST'd, have the cserver execute the file
        otherwise just run() the project
    '''
    result = ''
    if request.method=='POST':
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
    return HttpResponse(result)

@never_cache
@login_required()
def Exit(request):
    server_mgr.delete_server(request.session.session_key)
    return HttpResponseRedirect('/')

@never_cache
@csrf_exempt
@login_required()
def File(request,filename):
    ''' get/set the specified file/folder
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    if request.method=='POST':
        if 'isFolder' in request.POST:
            return HttpResponse(cserver.ensure_dir(filename))
        else:
            return HttpResponse(cserver.write_file(filename,request.POST['contents']))
    elif request.method=='DELETE':
        return HttpResponse(cserver.delete_file(filename))
    elif request.method=='GET':
        return HttpResponse(cserver.get_file(filename))
    return HttpResponse("How did I get here?")

@never_cache
@login_required()
def Files(request):
    ''' get a list of the users files in JSON format
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    filedict = cserver.get_files()
    json = jsonpickle.encode(filedict)
    return HttpResponse(json,mimetype='application/json')

@never_cache
@login_required()
def Logout(request):
    server_mgr.delete_server(request.session.session_key)
    logout(request)
    return HttpResponseRedirect('/')
    
@never_cache
@csrf_exempt
@login_required()
def Model(request):
    ''' POST: get a new model (delete existing console server)
        GET:  get JSON representation of the model
    '''
    if request.method=='POST':
        server_mgr.delete_server(session.session.session_key)
        return HttpResponseRedirect('/')
    else:
        cserver = server_mgr.console_server(request.session.session_key)
        json = cserver.get_JSON()
        return HttpResponse(json,mimetype='application/json')

@never_cache
@login_required()
def Output(request):
    ''' get any outstanding output from the model
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    return HttpResponse(cserver.get_output())

from openmdao.gui.settings import MEDIA_ROOT
@never_cache
@csrf_exempt
@login_required()
def Project(request):
    ''' GET:  load model fom the given project archive
        POST: save project archive of the current project
    '''
    if request.method=='POST':
        cserver = server_mgr.console_server(request.session.session_key)
        cserver.save_project()
        return HttpResponse('Saved.')
    else:
        print "Loading project into workspace:",request.GET['filename']
        server_mgr.delete_server(request.session.session_key) # delete old server
        cserver = server_mgr.console_server(request.session.session_key)        
        cserver.load_project(MEDIA_ROOT+'/'+request.GET['filename'])
        return HttpResponseRedirect(reverse('workspace.views.Workspace'))
    
@never_cache
@login_required()
def Types(request):
    ''' get hierarchy of package/types to populate the Palette
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    types = cserver.get_available_types()
    types = packagedict(types)
    types['working'] = packagedict(cserver.get_workingtypes())
    json = jsonpickle.encode(types)
    return HttpResponse(json,mimetype='application/json')

@never_cache
@login_required()
def Workspace(request):
    ''' initialize the server manager &  render the workspace
    '''
    return render_to_response('workspace.html',
                              context_instance=RequestContext(request))
