import os, sys
import os.path
from time import strftime
import Cookie   # in py3 this becomes http.cookies

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


import openmdao.gui.app_projdb as app_projdb
import openmdao.gui.app_workspace as app_workspace

#
#
class BaseHandler(tornado.web.RequestHandler):
    ''' override the get_current_user() method in your request handlers to determine
        the current user based on the value of a cookie.
    '''
    def get_current_user(self):
        return self.get_secure_cookie("user")

class LoginHandler(BaseHandler):
    ''' lets users log into the application simply by specifying a nickname,
        which is then saved in a cookie.
    '''
    def get(self):
        self.write('<html><body><form action="/login" method="post">'
                   'Name: <input type="text" name="name">'
                   '<input type="submit" value="Sign in">'
                   '</form></body></html>')

    def post(self):
        self.set_secure_cookie("user", self.get_argument("name"))
        self.redirect("/")

class LogoutHandler(BaseHandler):
    ''' lets users log out of the application simply by deleting the nickname cookie
    '''
    def get(self):
        self.clear_cookie("user")
        self.redirect("/")

    def post(self):
        self.clear_cookie("user")
        self.redirect("/")

def main():
    # map URLs to handlers
    handlers = [
        tornado.web.url(r'/login',                                   LoginHandler),
        tornado.web.url(r'/logout',                                  LogoutHandler),
        
        tornado.web.url(r'/',                                        app_projdb.IndexHandler),
        tornado.web.url(r'/projects/?',                              app_projdb.IndexHandler),
        tornado.web.url(r'/projects/(?P<project_id>\d+)/$',          app_projdb.DetailHandler),
        tornado.web.url(r'/projects/new/$',                          app_projdb.NewHandler),
        tornado.web.url(r'/projects/add/$',                          app_projdb.AddHandler),
        tornado.web.url(r'/projects/delete/(?P<project_id>\d+)/$',   app_projdb.DeleteHandler),
        tornado.web.url(r'/projects/download/(?P<project_id>\d+)/$', app_projdb.DownloadHandler),
        
        tornado.web.url(r'/workspace/?',                             app_workspace.WorkspaceHandler, name='workspace'),    
        tornado.web.url(r'/workspace/components/?',                  app_workspace.ComponentsHandler),
        tornado.web.url(r'/workspace/component/(.*)',                app_workspace.ComponentHandler),
        tornado.web.url(r'/workspace/connections/(.*)',              app_workspace.ConnectionsHandler),
        tornado.web.url(r'/workspace/addons/?',                      app_workspace.AddOnsHandler),
        tornado.web.url(r'/workspace/close/?',                       app_workspace.CloseHandler),
        tornado.web.url(r'/workspace/command',                       app_workspace.CommandHandler),
        tornado.web.url(r'/workspace/structure/(.*)',                app_workspace.StructureHandler),
        tornado.web.url(r'/workspace/exec/?',                        app_workspace.ExecHandler),
        tornado.web.url(r'/workspace/exit/?',                        app_workspace.ExitHandler),
        tornado.web.url(r'/workspace/file/(.*)',                     app_workspace.FileHandler),
        tornado.web.url(r'/workspace/files/?',                       app_workspace.FilesHandler),
        tornado.web.url(r'/workspace/geometry',                      app_workspace.GeometryHandler),
        tornado.web.url(r'/workspace/model/?',                       app_workspace.ModelHandler),
        tornado.web.url(r'/workspace/output/?',                      app_workspace.OutputHandler),
        tornado.web.url(r'/workspace/plot/?',                        app_workspace.PlotHandler),
        tornado.web.url(r'/workspace/project/?',                     app_workspace.ProjectHandler),
        tornado.web.url(r'/workspace/types/?',                       app_workspace.TypesHandler),
        tornado.web.url(r'/workspace/upload/?',                      app_workspace.UploadHandler),
        tornado.web.url(r'/workspace/workflow/(.*)',                 app_workspace.WorkflowHandler),
        tornado.web.url(r'/workspace/test/?',                        app_workspace.TestHandler),
    ]
    
    # settings: debug, static handler, etc
    app_settings = { 
        'debug': True,
        'static_path': os.path.join(os.path.dirname(__file__), 'static'),
        'login_url': '/login',
        'cookie_secret': '61oETzKXQAGaYdkL5gEmGeJJFuYh7EQnp2XdTP1o/Vo=',
    }

    # create and start the app
    application = tornado.web.Application(handlers, **app_settings)    
    print 'Starting app:',application 
    http_server = tornado.httpserver.HTTPServer(application)
    http_server.listen(9000)
    tornado.ioloop.IOLoop.instance().start()

if __name__ == "__main__":
    main()