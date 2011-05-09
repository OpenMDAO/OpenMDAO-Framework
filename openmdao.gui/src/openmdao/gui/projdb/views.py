from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response, get_object_or_404
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User
from django.template import RequestContext

from projdb.models import Project

#
# project list
#
@login_required()
def index(request):
    project_list = Project.objects.filter(user=request.user)
    return render_to_response('project_list.html', 
                              {'project_list': project_list, 'user': request.user})

#
# project detail
#                              
@login_required()
def detail(request, project_id):
    p = get_object_or_404(Project, pk=project_id)
    if request.POST:
        p.projectname   = request.POST['projectname']
        p.description   = request.POST['description']
        if 'shared' in request.POST:
            p.shared = True
        else:
            p.shared = False
        p.save()
        return HttpResponseRedirect('')
    else:
        return render_to_response('project_detail.html', {'project': p},
                                  context_instance=RequestContext(request))

#
# new (empty) project
#                              
@login_required()
def new(request):
    return HttpResponse('New project - Not yet implemented')

#
# add existing project
#                              
@login_required()
def add(request):
    return HttpResponse('Add project - Not yet implemented')

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
        