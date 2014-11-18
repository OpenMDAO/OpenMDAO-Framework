from tornado.web import RequestHandler, StaticFileHandler
from openmdao.gui.session import TornadoSession
import threading
import os
import sys
import re
import openmdao.main
from openmdao.main.rbac import Credentials
from openmdao.main.plugin import find_docs_url
from openmdao.util.fileutil import get_ancestor_dir


class ReqHandler(RequestHandler):
    ''' Override the get_current_user() method in request handlers to
        determine the current user based on the value of a cookie.
    '''

    def initialize(self):
        self.session = TornadoSession(self.application.session_manager, self)

    def get_sessionid(self):
        return self.session.session_id

    def get_current_user(self):
        return self.get_secure_cookie('user')

    def get_server(self):
        return self.application.server_manager.server(self.get_sessionid())

    def delete_server(self):
        self.application.server_manager.delete_server(self.get_sessionid())

    def get_project_dir(self):
        return self.application.project_dir


class LoginHandler(ReqHandler):
    ''' Currently we support a single user, the current user.
        This hook is for potential future development using Credentials.
    '''

    def get(self):
        # single user scenario, auto-login based on username
        server_creds = Credentials()
        print "server_creds:\n", server_creds
        self.set_secure_cookie('user', server_creds.user)
        self.redirect('/')

    def post(self):
        server_creds = Credentials()
        print 'server creds:\n', server_creds

        # single user... only allow same user@host as the server to log in
        allowed_users = {server_creds.user: server_creds.public_key}
        encoded = self.get_argument('encoded')
        client_creds = Credentials.verify(encoded, allowed_users)
        print 'client creds:\n', client_creds

        if client_creds:
            self.set_secure_cookie('user', client_creds.user)
        self.redirect('/')


class LogoutHandler(ReqHandler):
    ''' Lets users log out of the application simply by deleting the
        nickname cookie.
    '''

    def get(self):
        self.clear_cookie('user')
        self.redirect('/')

    def post(self):
        self.clear_cookie('user')
        self.redirect('/')


class ExitHandler(ReqHandler):
    ''' Shut it down; try to close the browser window.
    '''

    def get(self):
        self.application.exit()
        self.render('closewindow.html')


class PluginDocsHandler(StaticFileHandler):
    ''' Retrieve docs for a plugin. '''
    _plugin_map = {}
    _plugin_lock = threading.Lock()
    regex = re.compile("site-packages/openmdao.main.+\.egg")

    def _cname_valid(self, name):
        # TODO: use regex to check form of cname (must be dotted module path)
        return True

    def initialize(self, route):
        rpath = self.request.path[len(route):].strip('/')
        parts = rpath.split('/', 1)
        self.cname = parts[0] + os.sep
        self.added = ''
        if len(parts) == 1:
            with self._plugin_lock:
                if self._cname_valid(parts[0]) and parts[0] not in self._plugin_map:
                    url = find_docs_url(parts[0], build_if_needed=False)
                    #Remove "file://" from URL
                    url = url[7:]
                    if self.cname.startswith('openmdao.'):
                        if(self.regex.search(url)):
                            # url points to docs in a release version, so use docs packaged with openmdao.main
                            root = os.path.join(os.path.dirname(openmdao.main.__file__), "docs")
                        else:  # url points to docs in a developer version, so use locally built docs
                            root = os.path.join(get_ancestor_dir(sys.executable, 3), 'docs',
                                            '_build', 'html')
                        self.added = os.path.dirname(url)[len(root) + 1:]
                    else:
                        root = os.path.dirname(url)

                    default = os.path.basename(url)
                    self._plugin_map[parts[0]] = (root, default, self.added)

        root, default, self.added = self._plugin_map[parts[0]]

        super(PluginDocsHandler, self).initialize(root, default)

    def get(self, path, include_body=True):

        # Note, don't want to lower-case our URL
        # However, we do want to replace any MS-DOS slashes.
        if os.path.sep != "/":
            path = path.replace("/", os.path.sep)

        if path + os.sep == self.cname:
            self.redirect(os.path.join('/docs',
                                       'plugins',
                                       self.cname, self.default_filename))

        elif path.startswith(self.cname):
            super(PluginDocsHandler, self).get(os.path.join(self.added, path[len(self.cname):]),
                                               include_body)
        else:
            super(PluginDocsHandler, self).get(path, include_body)
