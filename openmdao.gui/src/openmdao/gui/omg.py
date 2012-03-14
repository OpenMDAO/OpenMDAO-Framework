""" OpenMDAO GUI

This graphical user interface for OpenMDAO is implemented as a web application.

Running this file will start a tornado web server on a local port and open a
browser on that port.  An up-to-date version of Chrome or Firefox with support
for WebSockets is required.
"""

import os, sys
import os.path

from optparse import OptionParser

# zmq
import zmq
from zmq.eventloop import ioloop

# tornado
from tornado import httpserver, web

# openmdao
from openmdao.util.network import get_unused_ip_port

from openmdao.gui.util import ensure_dir, launch_browser
from openmdao.gui.session import TornadoSessionManager 
from openmdao.gui.zmqservermanager import ZMQServerManager

from django.conf import settings

debug = True
def DEBUG(msg):
    if debug:
        print '<<<'+str(os.getpid())+'>>> AppServer --',msg

def get_user_dir():
    user_dir = os.path.expanduser("~/.openmdao/gui/")
    ensure_dir(user_dir)
    return user_dir
    
class App(web.Application):
    ''' openmdao web application
        extends tornado web app with URL mappings, settings and server manager
    '''

    def __init__(self, secret=None):
        from openmdao.gui.handlers import LoginHandler, LogoutHandler
        handlers = [
            web.url(r'/login',  LoginHandler),
            web.url(r'/logout', LogoutHandler),
            web.url(r'/',       web.RedirectHandler, {'url':'/projects', 'permanent':False}),           
        ]        
        
        import openmdao.gui.handlers_projectdb as proj
        handlers.extend(proj.handlers)
        
        import openmdao.gui.handlers_workspace as wksp
        handlers.extend(wksp.handlers)
        
        if secret is None:
            secret = os.urandom(1024)
            
        app_path     = os.path.dirname(os.path.abspath(__file__))
        app_settings = { 
            'login_url':         '/login',
            'static_path':       os.path.join(app_path, 'static'),
            'template_path':     os.path.join(app_path, 'templates'),
            'cookie_secret':     secret,
            'debug':             True,
        }
        
        user_dir = get_user_dir()
        
        self.project_dir = os.path.join(user_dir, 'projects')
        ensure_dir(self.project_dir)
        
        session_dir = os.path.join(user_dir, 'sessions')
        ensure_dir(session_dir)
            
        self.session_manager = TornadoSessionManager(secret,session_dir)
        self.server_manager  = ZMQServerManager('openmdao.gui.consoleserver.ConsoleServer')
        
        super(App, self).__init__(handlers, **app_settings)

class AppServer(object):
    ''' openmdao web application server
        wraps tornado web app, runs http server and opens browser
    '''
    def __init__(self,options):
        self.options = options
        
        user_dir = get_user_dir()

        # initialize django settings
        database = os.path.join(user_dir,'mdaoproj.db')
        settings.configure(DATABASE_ENGINE='sqlite3',
                           DATABASE_NAME=database,
                           MEDIA_ROOT=os.path.join(user_dir,'projects'),
                           INSTALLED_APPS=('projdb',
                                           'django.contrib.auth',
                                           'django.contrib.contenttypes',))

        if options.reset or not os.path.exists(database):
            print "Resetting project database..."
            if os.path.exists(database):
                print "Deleting existing project database..."
                os.remove(database)
            from django.core.management import execute_manager
            execute_manager(settings,argv=[__file__,'sqlall','projdb'])
            execute_manager(settings,argv=[__file__,'syncdb'])
        
        if (options.port < 1):
            options.port = get_unused_ip_port()
            
        # save secret between restarts
        secret_file = os.path.join(user_dir,'secret')
        if os.path.exists(secret_file):
            secret = open(secret_file,'rb').read()
        else:
            secret = os.urandom(1024)
            open(secret_file,'wb').write(secret)
        self.app = App(secret)
            
    def serve(self):
        ''' start server listening on port, launch browser if requested  & start the ioloop
        '''
        self.http_server = httpserver.HTTPServer(self.app)        
        self.http_server.listen(self.options.port)        

        if not self.options.serveronly:
            launch_browser(self.options.port, self.options.browser)
            
        try:
            ioloop.IOLoop.instance().start()
        except KeyboardInterrupt:
            DEBUG('interrupt received, shutting down.')

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
        parser.add_option("-r", "--reset", action="store_true", dest="reset",
                          help="reset project database")
        parser.add_option("-d", "--dev", action="store_true", dest="development",
                          help="enable development options")
                          
        return parser

def main():
    ''' process command line arguments and do as commanded
    '''
    
    # install zmq ioloop before creating any tornado objects
    ioloop.install()
    
    # create the server and kick it off
    parser = AppServer.get_options_parser()
    (options, args) = parser.parse_args()
    server = AppServer(options)
    
    DEBUG('starting server...')
    server.serve()
        
if __name__ == '__main__':
    # dont run main() if this is a forked windows process
    if sys.modules['__main__'].__file__ == __file__:
        main()
