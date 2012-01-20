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


from app_projdb import *
from app_workspace import *

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


handlers = [
    (r'/login',                       LoginHandler),
	
    (r'/',                                        IndexHandler),
    (r'/projects/',                               IndexHandler),
    (r'/projects/(?P<project_id>\d+)/$',          DetailHandler),
    (r'/projects/new/$',                          NewHandler),
    (r'/projects/add/$',                          AddHandler),
    (r'/projects/delete/(?P<project_id>\d+)/$',   DeleteHandler),
    (r'/projects/download/(?P<project_id>\d+)/$', DownloadHandler),
	
    (r'/workspace',                   WorkspaceHandler),
    (r'/workspace/components',        ComponentsHandler),
    (r'/workspace/component/(.*)',    ComponentHandler),
    (r'/workspace/connections/(.*)',  ConnectionsHandler),
    (r'/workspace/addons',            AddOnsHandler),
    (r'/workspace/close',             CloseHandler),
    (r'/workspace/command',           CommandHandler),
    (r'/workspace/structure/(.*)',    StructureHandler),
    (r'/workspace/exec',              ExecHandler),
    (r'/workspace/exit',              ExitHandler),
    (r'/workspace/file/(.*)',         FileHandler),
    (r'/workspace/files',             FilesHandler),
    (r'/workspace/geometry',          GeometryHandler),
    (r'/workspace/logout',            LogoutHandler),
    (r'/workspace/model',             ModelHandler),
    (r'/workspace/output',            OutputHandler),
    (r'/workspace/plot/(.*)',         PlotHandler),
    (r'/workspace/project/',       ProjectHandler),
    (r'/workspace/types',             TypesHandler),
    (r'/workspace/upload',            UploadHandler),
    (r'/workspace/workflow/(.*)',     WorkflowHandler),
    (r'/workspace/test',              TestHandler),
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
    http_server.listen(8000)
    tornado.ioloop.IOLoop.instance().start()