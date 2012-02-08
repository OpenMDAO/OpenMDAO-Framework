import os, sys
import os.path
from time import strftime

# tornado
from tornado import web

# using django ORM, etc
os.environ['DJANGO_SETTINGS_MODULE'] = 'settings'
from django import forms
from django.core.files.base import ContentFile
from django.contrib.auth.models import User
from openmdao.gui.settings import MEDIA_ROOT
from openmdao.gui.projdb.models import Project
from django.shortcuts import get_object_or_404

from openmdao.gui.handlers import BaseHandler

class ProjectForm(forms.Form):
    projectname = forms.CharField(label='Project Name')
    description = forms.CharField(label='Description', required=False)
    version     = forms.CharField(label='Version',     required=False, max_length=5)
    shared      = forms.BooleanField(label='Shared',   required=False)

class ProjectFileForm(forms.Form):
    filename    = forms.HiddenInput()

    
class IndexHandler(BaseHandler):
    ''' get project list
    '''
    @web.authenticated
    def get(self):
        dbuser = User.objects.get(username__exact=self.current_user)
        project_list = Project.objects.filter(user=dbuser)
        self.render('projdb/project_list.html', 
                     project_list=project_list, user=self.current_user)

class DeleteHandler(BaseHandler):
    ''' delete a project
    '''
    @web.authenticated
    def post(self, project_id):
        p = get_object_or_404(Project, pk=project_id)
        if p.filename:
            dir = 'projects/'+self.current_user
            filename = MEDIA_ROOT+'/'+str(p.filename)
            if os.path.exists(filename):
                os.remove(filename)
        p.delete()
        self.redirect('/')

    @web.authenticated
    def get(self, project_id):
        self.redirect('/')

class DetailHandler(BaseHandler):
    ''' get/set project details
    '''
    @web.authenticated
    def post(self, project_id):
        form_data = {}
        for field in ['projectname', 'description', 'version', 'shared']:
            if field in self.request.arguments.keys():
                form_data[field]=self.request.arguments[field][0]
        form = ProjectForm(form_data)
        if form.is_valid():
            p = get_object_or_404(Project, pk=project_id)
            p.projectname   = form.cleaned_data['projectname'].strip()
            p.description   = form.cleaned_data['description'].strip()
            p.version       = form.cleaned_data['version'].strip()
            p.shared        = form.cleaned_data['shared']
            
            # if there's no proj file yet, create en empty one
            if not p.filename:
                dir = 'projects/'+self.current_user
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
            self.redirect(self.request.uri)
        else:
            print 'FORM NOT VALID'  # TODO: better error handling
            self.redirect(self.request.uri)

    @web.authenticated
    def get(self, project_id):
        p = get_object_or_404(Project, pk=project_id)
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
        self.render('projdb/project_detail.html',
                    project=p, project_form=proj_form, file_form=file_form)

# FIXME: returns an error even though it works
class DownloadHandler(BaseHandler):
    ''' download a copy of the project
    '''
    @web.authenticated
    def get(self, project_id):
        p = get_object_or_404(Project, pk=project_id)
        if p.filename:
            dir = 'projects/'+self.current_user
            filename = MEDIA_ROOT+'/'+str(p.filename)
            if os.path.exists(filename):
                proj_file = file(filename,'rb')
                from django.core.servers.basehttp import FileWrapper
                self.set_header('content_type','application/octet-stream')
                self.set_header('Content-Length',str(os.path.getsize(filename)))
                self.set_header('Content-Disposition','attachment; filename='+p.projectname+strftime(' %Y-%m-%d %H%M%S')+'.proj')
                try:
                    self.write(proj_file.read())
                finally:
                    proj_file.close()
            else:
                raise HTTPError(403, "%s is not a file", filename)
        else:
            raise HTTPError(403, "no file found for %s", p.projectname)

class NewHandler(BaseHandler):
    ''' create a new (empty) project
    '''
    @web.authenticated
    def get(self):
        dbuser = User.objects.get(username__exact=self.current_user)
        p = Project(user=dbuser)
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
        self.render('projdb/project_detail.html',
                    project=p, project_form=proj_form, file_form=file_form)

class AddHandler(BaseHandler):
    ''' upload a file and add it to the project database
    '''
    @web.authenticated
    def post(self):
        file = self.request.files['myfile'][0]
        if file:
            filename = file['filename']
            if len(filename) > 0:
                dbuser = User.objects.get(username__exact=self.current_user)
                p = Project(user=dbuser)
                p.projectname   = 'Added ' + filename + strftime(" %Y-%m-%d %H%M%S")
                p.save()
            
                dir = 'projects/'+self.current_user
                if not os.path.isdir(dir):
                    os.makedirs(dir)
                file_content = ContentFile(file['body'])
                p.filename.save(filename, file_content)
                
                self.redirect('/projects/'+str(p.id))
        self.redirect('')

    @web.authenticated
    def get(self):
        self.render('projdb/add_project.html')

handlers = [            
    web.url(r'/projects/?',                              IndexHandler),
    web.url(r'/projects/(?P<project_id>\d+)/?',          DetailHandler),
    web.url(r'/projects/new/$',                          NewHandler),
    web.url(r'/projects/add/$',                          AddHandler),
    web.url(r'/projects/delete/(?P<project_id>\d+)/?',   DeleteHandler),
    web.url(r'/projects/download/(?P<project_id>\d+)/?', DownloadHandler),
]
