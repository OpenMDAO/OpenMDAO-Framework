from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response, get_object_or_404
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt                                          
from django.contrib.auth import logout
from django.views.decorators.cache import never_cache
from django.core.urlresolvers import reverse
from django import forms

import sys, os, traceback
import zipfile, jsonpickle

from openmdao.gui.mdao_util import *

class HttpResponseSeeOther(HttpResponseRedirect):
    status_code = 303

# TODO:
prefix = '<<<'+str(os.getpid())+'>>> '
print prefix+'workspace.views() -------------------------------------'
from openmdao.gui.consoleserverfactory import ConsoleServerFactory
server_mgr = ConsoleServerFactory()

class AddonForm(forms.Form):
    distribution = forms.CharField(label='Distribution')
    
@never_cache
@login_required()
def AddOns(request):
    ''' addon installation utility
    '''
    addons_url = 'http://openmdao.org/dists'
    addons_url = 'http://torpedo.grc.nasa.gov:31005/'
    
    if request.method=='POST':
        ''' easy_install the POST'd addon
        '''
        form = AddonForm(request.POST)
        if form.is_valid():
            distribution = form.cleaned_data['distribution']
            cserver = server_mgr.console_server(request.session.session_key)
            cserver.install_addon(addons_url, distribution)
            return render_to_response('closewindow.html')
            
    ''' show available addons, prompt for addon to be installed
    '''
    form = AddonForm()
    return render_to_response('addons.html', 
                              {'addons_url': addons_url, 'addon_form': form },
                              context_instance=RequestContext(request))
        
@never_cache
@login_required()
def Geometry(request):
    ''' geometry viewer
    '''
    return render_to_response('o3dviewer.html',
                              {'filename': request.GET['path'] },
                              context_instance=RequestContext(request))
 
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
        return HttpResponse(attr,mimetype='application/json')

@never_cache
def Components(request):
    cserver = server_mgr.console_server(request.session.session_key)
    json = cserver.get_components()
    return HttpResponse(json,mimetype='application/json')

@never_cache
def Dataflow(request,name):
    cserver = server_mgr.console_server(request.session.session_key)
    json = cserver.get_dataflow(name)
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
        return HttpResponseSeeOther(reverse('workspace.views.Workspace'))

@never_cache
@csrf_exempt
@login_required()
def Top(request):
    ''' GET:  hmmm...
        POST: set top to the named assembly
    '''
    if request.method=='POST':
        cserver = server_mgr.console_server(request.session.session_key)
        if 'name' in request.POST:
            name = request.POST['name']
            try:
                cserver.set_top(name)
                print 'Top is now '+name
                return HttpResponse('Top is now '+name)
            except Exception,e:
                print 'Error settign top:',e
                return HttpResponse('Error setting top: '+e)
    else:
        return HttpResponseSeeOther(reverse('workspace.views.Workspace'))
        
@never_cache
@login_required()
def Types(request):
    ''' get hierarchy of package/types to populate the Palette
    '''
    cserver = server_mgr.console_server(request.session.session_key)
    types = cserver.get_available_types()
    try:
        types['working'] = cserver.get_workingtypes()
    except Exception, err:
        print "Error adding working types:", str(err)        
    return HttpResponse(jsonpickle.encode(types),mimetype='application/json')

@never_cache
@csrf_exempt
@login_required()
def Upload(request):
    ''' file upload utility
    '''
    if request.method=='POST':
        cserver = server_mgr.console_server(request.session.session_key)
        if 'myfile' in request.FILES:
            file = request.FILES['myfile']
            filename = file.name
            if len(filename) > 0:
                contents = file.read()
                cserver.add_file(filename,contents)
                return render_to_response('closewindow.html')

    return render_to_response('upload.html', 
                              context_instance=RequestContext(request))

@never_cache
def Workflow(request,name):
    cserver = server_mgr.console_server(request.session.session_key)
    json = cserver.get_workflow(name)
    return HttpResponse(json,mimetype='application/json')
    
@never_cache
@login_required()
def Workspace(request):
    ''' render the workspace
    '''
    return render_to_response('workspace.html',
                              context_instance=RequestContext(request))

@never_cache
@login_required()
def Test(request):
    ''' initialize the server manager &  render the workspace
    '''
    return render_to_response('test.html',
                              context_instance=RequestContext(request))
