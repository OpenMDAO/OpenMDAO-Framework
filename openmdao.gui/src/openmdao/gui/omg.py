""" OpenMDAO GUI

This graphical user interface for OpenMDAO is implemented as a web application.

Running this file will start a tornado web server on a local port and open a
browser on that port.  An up-to-date version of Chrome or Firefox with support
for WebSockets is required.
"""

import os
import signal
import socket
import sys
import threading
import time

from argparse import ArgumentParser

from zmq.eventloop import ioloop

# tornado
from tornado import httpserver, web

# openmdao
from openmdao.util.network import get_unused_ip_port
from openmdao.util.fileutil import get_ancestor_dir, is_dev_build
from openmdao.util.log import enable_console

from openmdao.gui.util import ensure_dir, launch_browser
from openmdao.gui.projectdb import Projects
from openmdao.gui.session import TornadoSessionManager
from openmdao.gui.zmqservermanager import ZMQServerManager

from openmdao.gui.handlers import LoginHandler, LogoutHandler, \
                                  ExitHandler, PluginDocsHandler
import openmdao.gui.handlers_projectdb as proj
import openmdao.gui.handlers_workspace as wksp

debug = True


def DEBUG(msg):
    if debug:
        print '<<<' + str(os.getpid()) + '>>> OMG --', msg
        sys.stdout.flush()


def get_user_dir():
    """ Return user's GUI directory. """
    user_dir = os.path.expanduser("~/.openmdao/gui/")
    ensure_dir(user_dir)
    return user_dir


class App(web.Application):
    ''' Openmdao web application.
        Extends tornado web app with URL mappings, settings and server manager.
    '''

    def __init__(self, secret=None, external=False):
        # locate the docs, so that the /docs url will point to the appropriate
        # docs, either for the current release or the current development build
        if is_dev_build():
            docpath = os.path.join(get_ancestor_dir(sys.executable, 3), 'docs',
                                   '_build', 'html')
        else:
            import openmdao.main
            docpath = os.path.join(os.path.dirname(openmdao.main.__file__), 'docs')

        handlers = [
            web.url(r'/',       web.RedirectHandler, {'url': '/projects', 'permanent': False}),
            web.url(r'/login',  LoginHandler),
            web.url(r'/logout', LogoutHandler),
            web.url(r'/exit',   ExitHandler),
            web.url(r'/docs/plugins/(.*)', PluginDocsHandler, {'route': '/docs/plugins/'}),
            web.url(r'/docs/(.*)', web.StaticFileHandler, {'path': docpath, 'default_filename': 'index.html'}),
        ]
        handlers.extend(proj.handlers)
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

        self.session_manager = TornadoSessionManager(secret, session_dir)
        self.server_manager  = ZMQServerManager('openmdao.gui.consoleserver.ConsoleServer', external)

        # External termination normally only used during GUI testing.
        if sys.platform == 'win32':
            # Fake SIGTERM by polling for a .sigterm file.
            self._exit_requested = False
            self._poller = threading.Thread(target=self._sigterm_poller,
                                            name='SIGTERM poller')
            self._poller.daemon = True
            self._poller.start()
        else:
            signal.signal(signal.SIGTERM, self._sigterm_handler)

        super(App, self).__init__(handlers, **app_settings)

    def _sigterm_poller(self):
        """ On Windows, poll for an external termination request file. """
        sigfile = os.path.join(os.getcwd(), 'SIGTERM.txt')
        while not self._exit_requested:
            time.sleep(1)
            if os.path.exists(sigfile):
                DEBUG('Detected SIGTERM, shutting down...')
                self._shutdown()
                break

    def _sigterm_handler(self, signum, frame):
        """ On Linux/OS X, handle SIGTERM signal. """
        DEBUG('Received SIGTERM, shutting down...')
        self._shutdown()

    def exit(self):
        """ Shutdown. """
        DEBUG('Exit requested, shutting down...')
        if sys.platform == 'win32':
            self._exit_requested = True
            self._poller.join(3)
        self._shutdown()

    def _shutdown(self):
        """ Stop all subprocesses and exit. """
        self.server_manager.cleanup()
        ioloop.IOLoop.instance().add_timeout(time.time() + 5, sys.exit)


class AppServer(object):
    ''' Openmdao web application server.
        Wraps tornado web app, runs http server, and opens browser.
    '''

    def __init__(self, options):
        self.options = options

        user_dir = get_user_dir()

        # initialize some settings
        database = os.path.join(user_dir, 'projects.db')

        if options.reset or not os.path.exists(database):
            print "Resetting project database..."
            if os.path.exists(database):
                print "Deleting existing project database..."
                os.remove(database)

            pdb = Projects(database)
            pdb.create()

        options.orig_port = options.port
        if (options.port < 1):
            options.port = get_unused_ip_port()

        # save secret between restarts
        secret_file = os.path.join(user_dir, 'secret')
        if os.path.exists(secret_file):
            secret = open(secret_file, 'rb').read()
        else:
            secret = os.urandom(1024)
            open(secret_file, 'wb').write(secret)
        self.app = App(secret, options.external)

    def serve(self):
        ''' Start server listening on port, launch browser if requested,
            and start the ioloop.
        '''
        self.http_server = httpserver.HTTPServer(self.app)
        for retry in range(3):
            try:
                if self.options.external:
                    self.http_server.listen(self.options.port)
                else:
                    self.http_server.listen(self.options.port, address='localhost')
            except socket.error:
                # Possibly 'Address already in use', try finding another port.
                if self.options.orig_port < 1 and retry < 2:
                    self.options.port = get_unused_ip_port()
                else:
                    raise
            else:
                break

        if not self.options.serveronly:
            launch_browser(self.options.port, self.options.browser)

        if self.options.external:
            print '***********************************************************'
            print '** WARNING: You have exposed the server to the external  **'
            print '**          network.  THIS IS NOT SAFE!!  Clients will   **'
            print '**          have access to a command prompt on the host  **'
            print '**          computer with the identity and privileges of **'
            print '**          the userid under which the server was run.   **'
            print '**                                                       **'
            print '**    This is very dangerous and you should NOT do it.   **'
            print '**      You exercise this option at your own risk!!!     **'
            print '**              (Ctrl-C to terminate server)             **'
            print '***********************************************************'

        DEBUG('Serving on port %d' % self.options.port)
        try:
            ioloop.IOLoop.instance().start()
        except KeyboardInterrupt:
            DEBUG('interrupt received, shutting down.')

    @staticmethod
    def get_argument_parser():
        ''' Create a parser for command-line arguments.
        '''
        parser = ArgumentParser(description='launch the graphical user interface')
        parser.add_argument('-p', '--port', type=int, dest='port', default=0,
                          help='port to run server on (defaults to any available port)')
        parser.add_argument('-b', '--browser', dest='browser', default='chrome',
                          help='preferred browser')
        parser.add_argument('-s', '--server', action='store_true', dest='serveronly',
                          help="don't launch browser, just run server")
        parser.add_argument('-r', '--reset', action='store_true', dest='reset',
                          help='reset project database')
        parser.add_argument('-x', '--external', action='store_true', dest='external',
                          help='allow access to server from external clients (WARNING: Not Safe or Secure!!)')
        return parser


def get_argument_parser():
    ''' Shortcut to AppServer argument parser.
    '''
    return AppServer.get_argument_parser()


def run(parser=None, options=None, args=None):
    ''' Launch the GUI with specified options.
    '''

    # install zmq ioloop before creating any tornado objects
    ioloop.install()

    # create the server and kick it off
    server = AppServer(options)
    server.serve()


def main():
    ''' Process command line arguments and run.
    '''
    enable_console()

    parser = AppServer.get_argument_parser()
    options, args = parser.parse_known_args()
    run(parser, options, args)


if __name__ == '__main__':
    main()
