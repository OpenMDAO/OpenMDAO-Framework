import os, sys
import os.path
from time import strftime
import datetime

# tornado
from tornado import web

from openmdao.gui.handlers import BaseHandler
from openmdao.gui.projectdb import Projects

    
class IndexHandler(BaseHandler):
    ''' get project list
    '''
    @web.authenticated
    def get(self):
            
        projects = Projects()
        project_list = projects.for_user()
        print project_list
        self.render('projdb/project_list.html', 
                     project_list=project_list)

class DeleteHandler(BaseHandler):
    ''' delete a project
    '''
    @web.authenticated
    def post(self, project_id):

        projects = Projects()
        pfields = projects.get(project_id)
        
        if pfields['filename']:
            filename = os.path.join(self.get_project_dir(), 
                                    str(pfields['filename']))
            if os.path.exists(filename):
                os.remove(filename)
        projects.remove(project_id)
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
        for field in ['projectname', 'description', 'version']:
            if field in self.request.arguments.keys():
                form_data[field] = self.request.arguments[field][0]
                
        project = Projects()
        
        # Existing project.
        if int(project_id) != project.predict_next_rowid():
            project_fields = project.get(project_id)
            project_is_new = False
        # New project
        else:
            project_fields = {}
            project_fields['active'] = 0
            project_fields['filename'] = None
            project_is_new = True
        
        # TODO: better error handling
        if 'projectname' in form_data:
            
            if len(form_data['projectname']) == 0:
                print 'ERROR: Project Name is Required'  
                self.redirect(self.request.uri)
                return
            
            project_fields['projectname'] = \
                form_data['projectname'].strip()
            
        if 'description' in form_data:
            project_fields['description'] = \
                form_data['description'].strip()
            
        if 'version' in form_data:
            project_fields['version'] = \
                form_data['version'].strip()
        
        # if there's no proj file yet, create en empty one
        if not project_fields['filename']:
            
            version = project_fields['version']
            pname = project_fields['projectname']
            
            if len(version):
                filename = '%s-%s.proj' % (pname, version)
            else:
                filename = '%s.proj' % pname

            dir = self.get_project_dir()
            i = 1
            while os.path.exists(os.path.join(dir, filename)):
                filename = filename + str(i)
                i = i+1
                
            with open(filename, 'w') as out:
                out.write('')
                out.close()
            
            print 'created file:', pname, filename

        project_fields['modified'] = str(datetime.datetime.now())
        
        if project_is_new:
            project.new(project_fields)
        else:
            for key, value in project_fields.iteritems():
                project.set(project_id, key, value)

        self.redirect(self.request.uri)

    @web.authenticated
    def get(self, project_id):
        
        project = Projects()
        project_fields = project.get(project_id)
        self.render('projdb/project_detail.html', project=project_fields)

# FIXME: returns an error even though it works
class DownloadHandler(BaseHandler):
    ''' download a copy of the project
    '''
    @web.authenticated
    def get(self, project_id):
        pass
        #p = get_object_or_404(Project, pk=project_id)
        #if p.filename:
            #filename = os.path.join(self.get_project_dir(),str(p.filename))
            #if os.path.exists(filename):
                #proj_file = file(filename,'rb')
                #from django.core.servers.basehttp import FileWrapper
                #self.set_header('content_type','application/octet-stream')
                #self.set_header('Content-Length',str(os.path.getsize(filename)))
                #self.set_header('Content-Disposition','attachment; filename='+p.projectname+strftime(' %Y-%m-%d %H%M%S')+'.proj')
                #try:
                    #self.write(proj_file.read())
                #finally:
                    #proj_file.close()
            #else:
                #raise HTTPError(403, "%s is not a file", filename)
        #else:
            #raise HTTPError(403, "no file found for %s", p.projectname)

class NewHandler(BaseHandler):
    ''' create a new (empty) project
    '''
    @web.authenticated
    def get(self):
        
        project = Projects()
        
        p = {}
        p['id'] = project.predict_next_rowid()
        p['projectname']   = 'New Project '+strftime("%Y-%m-%d %H%M%S")
        p['version'] = ''
        p['description'] = ''
        p['modified'] = str(datetime.datetime.now())
        p['filename'] = ''
        p['active'] = ''
        
        self.render('projdb/project_detail.html', project=p)

class AddHandler(BaseHandler):
    ''' upload a file and add it to the project database
    '''
    @web.authenticated
    def post(self):
        file = self.request.files['myfile'][0]
        
        if file:
            filename = file['filename']
            if len(filename) > 0:
                
                project = Projects()
                
                project_fields = {}
                project_fields['id'] = project.predict_next_rowid()
                project_fields['projectname']   = 'Added ' + filename + strftime(" %Y-%m-%d %H%M%S")
                project_fields['version'] = ''
                project_fields['description'] = ''
                project_fields['modified'] = str(datetime.datetime.now())
                project_fields['filename'] = filename
                project_fields['active'] = 1

                with open(filename, 'w') as out:
                    out.write(file['body'])
                    out.close()
                    
                project.new(project_fields)
                
                self.redirect('/projects/'+str(project_fields.id))
                
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
