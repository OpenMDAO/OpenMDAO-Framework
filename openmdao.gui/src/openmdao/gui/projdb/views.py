from django.http import HttpResponse, HttpResponseRedirect,HttpResponsePermanentRedirect
from django.shortcuts import render_to_response, get_object_or_404
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User
from django.template import RequestContext
from django import forms
from django.core.files.base import ContentFile
from django.core.urlresolvers import reverse

from openmdao.gui.settings import MEDIA_ROOT
from openmdao.gui.projdb.models import Project

from time import strftime

import os.path

class ProjectForm(forms.Form):
    projectname = forms.CharField(label='Project Name')
    description = forms.CharField(label='Description', required=False)
    version     = forms.CharField(label='Version',     required=False, max_length=5)
    shared      = forms.BooleanField(label='Shared',   required=False)

class ProjectFileForm(forms.Form):
    filename    = forms.HiddenInput()
    
#
# project list
#
@login_required()
def index(request):
    project_list = Project.objects.filter(user=request.user)
    return render_to_response('project_list.html', 
                              {'project_list': project_list, 'user': request.user})

#
# project delete
#                              
@login_required()
def delete(request, project_id):
    p = get_object_or_404(Project, pk=project_id)
    if request.POST:
        if p.filename:
            dir = 'projects/'+request.user.username
            filename = MEDIA_ROOT+'/'+str(p.filename)
            if os.path.exists(filename):
                os.remove(filename)
        p.delete()
        return HttpResponseRedirect(reverse('projdb.views.index'))

    return HttpResponseRedirect('')

#
# project detail
#                              
@login_required()
def detail(request, project_id):
    p = get_object_or_404(Project, pk=project_id)
    if request.POST:
        form = ProjectForm(request.POST)
        if form.is_valid():
            p.projectname   = form.cleaned_data['projectname'].strip()
            p.description   = form.cleaned_data['description'].strip()
            p.version       = form.cleaned_data['version'].strip()
            p.shared        = form.cleaned_data['shared']
            
            # if there's no proj file yet, create en empty one
            if not p.filename:
                dir = 'projects/'+request.user.username
                if not os.path.isdir(dir):
                    os.makedirs(dir)
                if len(p.version):
                    filename = p.projectname+'-'+p.version+'.proj'
                else:
                    filename = p.projectname+'.proj'  
                i=1
                while os.path.exists(MEDIA_ROOT+'/'+dir+'/'+filename):
                    filename = filename+str(i)
                    i = i+1
                file_content = ContentFile('')
                p.filename.save(filename, file_content)                
            p.save()
            return HttpResponseRedirect('')
            
    # not a POST or validation failed
    proj_form = ProjectForm({
        'projectname': p.projectname,
        'description': p.description,
        'version':     p.version,
        'shared':      p.shared,
    })
    file_form = ProjectFileForm({
        'filename':    p.filename
    })
    return render_to_response('project_detail.html', {
                              'project':      p, 
                              'project_form': proj_form,
                              'file_form':    file_form},
                              context_instance=RequestContext(request))


#
# project download
#                              
@login_required()
def download(request, project_id):
    p = get_object_or_404(Project, pk=project_id)
    if p.filename:
        dir = 'projects/'+request.user.username
        filename = MEDIA_ROOT+'/'+str(p.filename)
        if os.path.exists(filename):
            proj_file = file(filename,'rb')
            from django.core.servers.basehttp import FileWrapper
            response = HttpResponse(FileWrapper(proj_file), content_type='application/octet-stream')
            response['Content-Length'] = os.path.getsize(filename)
            response['Content-Disposition'] = 'attachment; filename='+p.projectname+'.proj'
            return response
    return HttpResponse('Sorry, file is not available.')

#
# new (empty) project
#                              
@login_required()
def new(request):
    p = Project(user=request.user)
    p.projectname   = 'New Project '+strftime("%Y-%m-%d %H%M%S")
    p.save()
    proj_form = ProjectForm({
        'projectname': p.projectname,
        'description': p.description,
        'version':     p.version,
        'shared':      p.shared,
    })
    file_form = ProjectFileForm({
        'filename':    p.filename
    })
    return render_to_response('project_detail.html', {
                              'project':      p, 
                              'project_form': proj_form,
                              'file_form':    file_form},
                              context_instance=RequestContext(request))

#
# add existing project
#                              
@login_required()
def add(request):
    ''' upload a file and add it to the project database
    '''
    if request.method=='POST':
        if 'myfile' in request.FILES:
            file = request.FILES['myfile']
            filename = file.name
            if len(filename) > 0:
                p = Project(user=request.user)
                p.projectname   = 'Added ' + filename + strftime(" %Y-%m-%d %H%M%S")
                p.save()
            
                dir = 'projects/'+request.user.username
                if not os.path.isdir(dir):
                    os.makedirs(dir)
                file_content = ContentFile(file.read())
                p.filename.save(filename, file_content)
                
                return HttpResponseRedirect('/projects/'+str(p.id))


    return render_to_response('add_project.html', 
                              context_instance=RequestContext(request))
#
# register user
#
from django import forms
from django.contrib.auth.forms import UserCreationForm
from django.contrib.auth import authenticate, login

def register(request):
    if request.method == 'POST':
        form = UserCreationForm(request.POST)
        if form.is_valid():
            new_user = form.save()
            new_user = authenticate(username=request.POST['username'],
                                    password=request.POST['password1'])
            login(request, new_user)
            return HttpResponseRedirect("/")
    else:
        form = UserCreationForm()
    return render_to_response("register.html", {'form': form,},
                              context_instance=RequestContext(request))    
    
#
# display meta data from browser
#    
def display_meta(request):
    html = []
    
    html.append('<h3>GET Data:</h3>')
    values = request.GET.values()
    values.sort()
    for k, v in values:
        html.append('<tr><td>%s</td><td>%s</td></tr>' % (k, v))
    html.append('<p>')
    
    html.append('<h3>POST Data:</h3>')
    values = request.POST.values()
    values.sort()
    for k, v in values:
        html.append('<tr><td>%s</td><td>%s</td></tr>' % (k, v))
    html.append('<p>')
    
    html.append('<h3>META Data:</h3>')
    values = request.META.items()
    values.sort()
    for k, v in values:
        html.append('<tr><td>%s</td><td>%s</td></tr>' % (k, v))
    return HttpResponse('<table>%s</table>' % '\n'.join(html))
