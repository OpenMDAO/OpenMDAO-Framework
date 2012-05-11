from tornado.web import RequestHandler
from openmdao.gui.session import TornadoSession

from openmdao.main.rbac import Credentials


class ReqHandler(RequestHandler):
    ''' override the get_current_user() method in your request handlers to
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
    ''' lets users log into the application simply by specifying a nickname,
        which is then saved in a cookie.
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
    ''' lets users log out of the application simply by deleting the
        nickname cookie
    '''

    def get(self):
        self.clear_cookie('user')
        self.redirect('/')

    def post(self):
        self.clear_cookie('user')
        self.redirect('/')


class ExitHandler(ReqHandler):
    ''' shut it down, try to close the browser window
    '''

    def get(self):
        self.application.exit()
        self.render('closewindow.html')
