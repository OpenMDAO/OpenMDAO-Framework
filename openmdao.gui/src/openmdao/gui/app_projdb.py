import os, sys
import os.path
from time import strftime

# tornado
import tornado.httpserver
import tornado.ioloop
import tornado.web

# TODO: remove django stuff
os.environ['DJANGO_SETTINGS_MODULE'] = 'settings'
from django import forms
from django.core.files.base import ContentFile
from django.contrib.auth.models import User
from openmdao.gui.settings import MEDIA_ROOT
from openmdao.gui.projdb.models import Project
from django.shortcuts import get_object_or_404

class ProjectForm(forms.Form):
    projectname = forms.CharField(label='Project Name')
    description = forms.CharField(label='Description', required=False)
    version     = forms.CharField(label='Version',     required=False, max_length=5)
    shared      = forms.BooleanField(label='Shared',   required=False)

class ProjectFileForm(forms.Form):
    filename    = forms.HiddenInput()
    
#
# override the get_current_user() method in your request handlers to determine
# the current user based on the value of a cookie.
#
class BaseHandler(tornado.web.RequestHandler):
    def get_current_user(self):
        return self.get_secure_cookie("user")

#
# lets users log into the application simply by specifying a nickname,
# which is then saved in a cookie:
#
class LoginHandler(BaseHandler):
    def get(self):
        self.write('<html><body><form action="/login" method="post">'
                   'Name: <input type="text" name="name">'
                   '<input type="submit" value="Sign in">'
                   '</form></body></html>')

    def post(self):
        self.set_secure_cookie("user", self.get_argument("name"))
        self.redirect("/")

#
# project list
#
class IndexHandler(BaseHandler):
    @tornado.web.authenticated
    def get(self):
        dbuser = User.objects.get(username__exact=self.current_user)
        project_list = Project.objects.filter(user=dbuser)
        self.render('tmpl/projdb/project_list.html', 
                     project_list=project_list, user=self.current_user)

#
# project delete
#                              
class DeleteHandler(BaseHandler):
    @tornado.web.authenticated
    def post(self, project_id):
        p = get_object_or_404(Project, pk=project_id)
        if p.filename:
            dir = 'projects/'+self.current_user
            filename = MEDIA_ROOT+'/'+str(p.filename)
            if os.path.exists(filename):
                os.remove(filename)
        p.delete()
        self.redirect('/')

    @tornado.web.authenticated
    def get(self, project_id):
        self.redirect('/')

#
# project detail
#                              
class DetailHandler(BaseHandler):
    @tornado.web.authenticated
    def post(self, project_id):
        p = get_object_or_404(Project, pk=project_id)
        form = ProjectForm(request.POST)
        if form.is_valid():
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
            self.writeRedirect('')
            
    @tornado.web.authenticated
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
        self.render('tmpl/projdb/project_detail.html', project=p,project_form=proj_form,file_form=file_form)

#
# project download
#                              
class DownloadHandler(BaseHandler):
    @tornado.web.authenticated
    def get(self, project_id):
        p = get_object_or_404(Project, pk=project_id)
        if p.filename:
            dir = 'projects/'+self.current_user
            filename = MEDIA_ROOT+'/'+str(p.filename)
            if os.path.exists(filename):
                proj_file = file(filename,'rb')
                from django.core.servers.basehttp import FileWrapper
                response = HttpResponse(FileWrapper(proj_file), content_type='application/octet-stream')
                response['Content-Length'] = os.path.getsize(filename)
                response['Content-Disposition'] = 'attachment; filename='+p.projectname+strftime(' %Y-%m-%d %H%M%S')+'.proj'
                return response
        self.write('Sorry, file is not available.')

#
# new (empty) project
#                              
class NewHandler(BaseHandler):
    @tornado.web.authenticated
    def post(self):
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
        self.render('tmpl/projdb/project_detail.html', {
                                  'project':      p, 
                                  'project_form': proj_form,
                                  'file_form':    file_form})

#
# add existing project
#                              
class AddHandler(BaseHandler):
    ''' upload a file and add it to the project database
    '''
    @tornado.web.authenticated
    def post(self):
        if 'myfile' in request.FILES:
            file = request.FILES['myfile']
            filename = file.name
            if len(filename) > 0:
                dbuser = User.objects.get(username__exact=self.current_user)
                p = Project(user=dbuser)
                p.projectname   = 'Added ' + filename + strftime(" %Y-%m-%d %H%M%S")
                p.save()
            
                dir = 'projects/'+self.current_user
                if not os.path.isdir(dir):
                    os.makedirs(dir)
                file_content = ContentFile(file.read())
                p.filename.save(filename, file_content)
                
                self.redirect('/projects/'+str(p.id))

    @tornado.web.authenticated
    def get(self):
        self.render('tmpl/projdb/add_project.html')


handlers = [
    (r'/projects/',                               IndexHandler),
    (r'/projects/(?P<project_id>\d+)/$',          DetailHandler),
    (r'/projects/new/$',                          NewHandler),
    (r'/projects/add/$',                          AddHandler),
    (r'/projects/delete/(?P<project_id>\d+)/$',   DeleteHandler),
    (r'/projects/download/(?P<project_id>\d+)/$', DownloadHandler),
]
    
##
## START THE SERVER
##                                  

if __name__ == "__main__":
    app_settings = { 
        'debug': True,
        'static_path': os.path.join(os.path.dirname(__file__), 'static'),
        'login_url': '/login',
        'cookie_secret': '61oETzKXQAGaYdkL5gEmGeJJFuYh7EQnp2XdTP1o/Vo=',
    }
    application = tornado.web.Application(handlers, **app_settings)
    http_server = tornado.httpserver.HTTPServer(application)
    http_server.listen(8887)
    tornado.ioloop.IOLoop.instance().start()