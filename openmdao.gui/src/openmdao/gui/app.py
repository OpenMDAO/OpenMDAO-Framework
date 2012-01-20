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
# override the get_current_user() method in your request handlers to determine
# the current user based on the value of a cookie.
#
class BaseHandler(tornado.web.RequestHandler):
    def prepare(self):
        # get sessionid from cookie, TODO: use a real session manager
        cooky = Cookie.SmartCookie(self.request.headers['Cookie'])
        self.sessionid = cooky['sessionid'].value

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


handlers = [
    (r'/login',                                   LoginHandler),
	
    (r'/',                                        app_projdb.IndexHandler),
    (r'/projects/?',                              app_projdb.IndexHandler),
    (r'/projects/(?P<project_id>\d+)/$',          app_projdb.DetailHandler),
    (r'/projects/new/$',                          app_projdb.NewHandler),
    (r'/projects/add/$',                          app_projdb.AddHandler),
    (r'/projects/delete/(?P<project_id>\d+)/$',   app_projdb.DeleteHandler),
    (r'/projects/download/(?P<project_id>\d+)/$', app_projdb.DownloadHandler),
	
    tornado.web.url(r'/workspace/?', app_workspace.WorkspaceHandler, name='workspace'),
    
    (r'/workspace/components/?',                  app_workspace.ComponentsHandler),
    (r'/workspace/component',                     app_workspace.ComponentHandler),
    (r'/workspace/connections',                   app_workspace.ConnectionsHandler),
    (r'/workspace/addons/?',                      app_workspace.AddOnsHandler),
    (r'/workspace/close/?',                       app_workspace.CloseHandler),
    (r'/workspace/command',                       app_workspace.CommandHandler),
    (r'/workspace/structure/?',                   app_workspace.StructureHandler),
    (r'/workspace/exec/?',                        app_workspace.ExecHandler),
    (r'/workspace/exit/?',                        app_workspace.ExitHandler),
    (r'/workspace/file/',                         app_workspace.FileHandler),
    (r'/workspace/files/?',                       app_workspace.FilesHandler),
    (r'/workspace/geometry',                      app_workspace.GeometryHandler),
    (r'/workspace/logout/?',                      app_workspace.LogoutHandler),
    (r'/workspace/model/?',                       app_workspace.ModelHandler),
    (r'/workspace/output/?',                      app_workspace.OutputHandler),
    (r'/workspace/plot/?',                        app_workspace.PlotHandler),
    (r'/workspace/project/?',                     app_workspace.ProjectHandler),
    (r'/workspace/types/?',                       app_workspace.TypesHandler),
    (r'/workspace/upload/?',                      app_workspace.UploadHandler),
    (r'/workspace/workflow/?',                    app_workspace.WorkflowHandler),
    (r'/workspace/test/?',                        app_workspace.TestHandler),
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
    
    print application 

    http_server = tornado.httpserver.HTTPServer(application)
    http_server.listen(9000)
    tornado.ioloop.IOLoop.instance().start()