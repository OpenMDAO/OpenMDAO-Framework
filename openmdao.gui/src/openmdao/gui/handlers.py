
# tornado
from tornado import httpserver, ioloop, web

class BaseHandler(web.RequestHandler):
    ''' override the get_current_user() method in your request handlers to determine
        the current user based on the value of a cookie.
    '''        
    def get_sessionid(self):
        return self.get_cookie('sessionid')
    
    def get_current_user(self):
        return self.get_secure_cookie('user')
        
    def get_server(self):
        sessionid = self.get_cookie('sessionid')
        cserver = self.application.server_mgr.console_server(sessionid)
        return cserver
        
    def delete_server(self):
        sessionid = self.get_cookie('sessionid')
        self.application.server_mgr.delete_server(sessionid)

class LoginHandler(BaseHandler):
    ''' lets users log into the application simply by specifying a nickname,
        which is then saved in a cookie.
    '''
    def get(self):
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
        
