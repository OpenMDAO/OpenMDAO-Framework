import getpass

# tornado
from tornado.web import RequestHandler
from openmdao.gui.session import TornadoSession

SINGLE_USER = True

class BaseHandler(RequestHandler):
    ''' override the get_current_user() method in your request handlers to determine
        the current user based on the value of a cookie.
    '''
    def initialize(self):
        self.session = TornadoSession(self.application.session_manager,self)
        
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
        

class LoginHandler(BaseHandler):
    ''' lets users log into the application simply by specifying a nickname,
        which is then saved in a cookie.
    '''
    def get(self):
        if SINGLE_USER:
            # single user scenario, auto-login based on username
            username = getpass.getuser()
            print 'Auto-login:',username
            self.set_secure_cookie('user', username)
            self.redirect('/')
        else:
            self.write('<html><body bgcolor="Grey"><form action="/login" method="post">'
                   'Name: <input type="text" name="name">'
                   '<input type="submit" value="Sign in">'
                   '</form></body></html>')

    def post(self):
        print 'Login:',self.get_argument('name')
        self.set_secure_cookie('user', self.get_argument('name'))
        self.redirect('/')

class LogoutHandler(BaseHandler):
    ''' lets users log out of the application simply by deleting the nickname cookie
    '''
    def get(self):
        self.clear_cookie('user')
        self.redirect('/')

    def post(self):
        self.clear_cookie('user')
        self.redirect('/')
        
class ExitHandler(BaseHandler):
    ''' lets users log out of the application simply by deleting the nickname cookie
    '''
    def get(self):
        self.application.exit()
        self.render('closewindow.html')

