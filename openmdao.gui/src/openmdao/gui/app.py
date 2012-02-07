import os, sys
import os.path

from optparse import OptionParser

# zmq
import zmq
from zmq.eventloop import ioloop
ioloop.install()

# tornado
from tornado import httpserver, ioloop, web

# openmdao
from openmdao.util.network import get_unused_ip_port

from openmdao.gui.util import ensure_dir, launch_browser
from openmdao.gui.consoleserverfactory import ConsoleServerFactory

from openmdao.gui.handlers import LoginHandler, LogoutHandler
import openmdao.gui.handlers_projdb    as proj
import openmdao.gui.handlers_workspace as wksp

class WebApp(web.Application):
    ''' openmdao web application server
        extends tornado web app with URL mappings, settings and server manager
    '''

    def __init__(self, server_mgr, cookie_secret=None):
        handlers = [
            web.url(r'/login',  LoginHandler),
            web.url(r'/logout', LogoutHandler),
            web.url(r'/',       web.RedirectHandler, {'url':'/projects', 'permanent':False}),           
        ]        
        handlers.extend(proj.handlers)
        handlers.extend(wksp.handlers)
        
        if cookie_secret is None:
            cookie_secret = os.urandom(1024)
            
        settings = { 
            'login_url':         '/login',
            'static_path':       os.path.join(os.path.dirname(__file__), 'static'),
            'template_path':     os.path.join(os.path.dirname(__file__), 'tmpl'),
            'cookie_secret':     cookie_secret,
            'debug':             True,
        }
        
        self.server_mgr = server_mgr
        
        super(WebApp, self).__init__(handlers, **settings)
        

class WrappedApp(object):
    ''' openmdao application
        wraps tornado web app, runs http server and opens browser
    '''

    def __init__(self,options):
        self.options = options
        
        if options.initialize or not os.path.exists('settings.py'):
            if options.reset:
                initialize_settings(reset=True)
            else:
                initialize_settings(reset=False)

        if (options.port < 1):
            options.port = get_unused_ip_port()
            
        self.server_mgr = ConsoleServerFactory()
        
        # dev options
        if options.development:
            # save cookie secret between restarts
            cookie_secret_file = os.path.expanduser("~/.openmdao/gui/cookie_secret")
            if os.path.exists(cookie_secret_file):
                cookie_secret = open(cookie_secret_file,'rb').read()
            else:
                cookie_secret = os.urandom(1024)
                open(cookie_secret_file,'wb').write(cookie_secret)        
            self.web_app = WebApp(self.server_mgr,cookie_secret)
        else:
            self.web_app = WebApp(self.server_mgr)
            
        self.http_server = httpserver.HTTPServer(self.web_app)
        self.http_server.listen(options.port)
        
        if not options.serveronly:
            launch_browser(options.port, options.browser)

        ioloop.IOLoop.instance().start()

    @staticmethod
    def get_options_parser():
        ''' create a parser for command line arguments
        '''
        parser = OptionParser()
        parser.add_option("-p", "--port", type="int", dest="port", default=0,
                          help="port to run server on (defaults to any available port)")
        parser.add_option("-b", "--browser", dest="browser", default="chrome",
                          help="preferred browser")
        parser.add_option("-s", "--server", action="store_true", dest="serveronly",
                          help="don't launch browser, just run server")
        parser.add_option("-i", "--init", action="store_true", dest="initialize",
                          help="(re)initialize settings")
        parser.add_option("-r", "--reset", action="store_true", dest="reset",
                          help="reset project database (valid only with -i and without -d)")
        parser.add_option("-d", "--dev", action="store_true", dest="development",
                          help="enable development options")
                          
        return parser

    def initialize_settings(reset):
        ''' first time setup (or re-setup)
        '''
        print "Initializing settings..."
        
        user_dir = os.path.expanduser("~/.openmdao/gui/")  # TODO: could put in a prefs file
        ensure_dir(user_dir)
        
        settings_file = "settings.py"
        database_file = user_dir+"mdaoproj.db"
        media_storage = user_dir+"media"
        
        if os.path.exists(settings_file):
            os.remove(settings_file)
        o = open(settings_file,"a") #open for append
        for line in open("settings.tmp"):
           line = line.replace("'NAME': 'mdaoproj.db'","'NAME': '"+database_file+"'")
           line = line.replace("MEDIA_ROOT = ''","MEDIA_ROOT = '"+media_storage+"'")
           o.write(line) 
        o.close()
        
        import settings
        print "MEDIA_ROOT=",settings.MEDIA_ROOT
        print "DATABASE=",settings.DATABASES['default']['NAME']
        
        print "Resetting project database..."
        if reset and os.path.exists(database_file):
            print "Deleting existing project database..."
            os.remove(database_file)
        from django.core.management import execute_manager
        execute_manager(settings,argv=[__file__,'syncdb'])


def main():
    ''' process command line arguments and do as commanded
    '''
    parser = WrappedApp.get_options_parser()
    (options, args) = parser.parse_args()
    app = WrappedApp(options)
    
if __name__ == '__main__':
    main()

