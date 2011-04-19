from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response, get_object_or_404
from django.contrib.auth.decorators import login_required
from django.template import RequestContext
from django.contrib.auth.models import User
from projdb.models import Project

@login_required()
def index(request):
    project_list = Project.objects.filter(user=request.user)
    return render_to_response('project_list.html', 
                              {'project_list': project_list, 'user': request.user})
    
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