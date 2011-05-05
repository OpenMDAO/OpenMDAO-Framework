from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response, get_object_or_404
from django.contrib.auth.decorators import login_required
from django.contrib.auth.models import User
from django.template import RequestContext

import sys, os
import zipfile, jsonpickle

from xml.etree.ElementTree import Element, SubElement, tostring
from xml.dom.minidom import Document

from setuptools.command import easy_install

from server_manager import ServerManager
from openmdao.main.factorymanager import get_available_types
from mdao_util import *

# TODO:
prefix = '<<<'+str(os.getpid())+'>>> '
print prefix
from server_manager import ServerManager
server_mgr = ServerManager()


@login_required()
def Workspace(request):
    ''' initialize the server manager &  render the workspace
    '''
    return render_to_response('workspace.html',
                              context_instance=RequestContext(request))
                              

from xml.etree.ElementTree import Element, SubElement, tostring
from xml.dom.minidom import Document

def Types(request):
    cserver = server_mgr.console_server(request.session.session_key)
    type_list = get_available_types()
    type_list.extend(cserver.get_workingtypes())
    print_list(type_list)
    type_dict = packagedict(type_list)
    json = jsonpickle.encode(type_dict)
    print json
    return HttpResponse(json,mimetype='application/json')
    
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
            