from pageobjects.openmdao_login import LoginPageObject, ProjectsListPageObject
from pageobjects import selenium_server_connection
#
import sys, unittest, re, time, os.path, logging
#

'''
  Run the use case described on
    https://docs.google.com/document/d/16m74ZhD1_TpgmGH_RPTUGthIMCGNsxRhJOpucbDX_4E/edit?hl=en_US
'''

import os, sys
from multiprocessing import Process

from optparse import OptionParser
from openmdao.util.network import get_unused_ip_port

def ensure_dir(d):
    ''' create directory if it does not exist
    '''
    if not os.path.isdir(d):
        os.makedirs(d)


from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait # available since 2.4.0
from selenium.webdriver import ActionChains

import unittest, time, re, inspect


def init(reset):
    # first time setup (or re-setup)
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

def dev(port):
    # run django development server
    import settings
    from django.core.management import execute_manager
    execute_manager(settings,argv=[__file__,'runserver',str(port)])

def pro(port):
    # run cherrypy 'production' server
    print "Running server on port",str(port)
    print "Quit the server with CTRL-BREAK"
    
    import django.core.handlers.wsgi
    from web.httpserver import WSGIServer, StaticMiddleware, LogMiddleware
    
    os.environ['DJANGO_SETTINGS_MODULE'] = 'settings'
    func = django.core.handlers.wsgi.WSGIHandler()
    func = StaticMiddleware(func)
    func = LogMiddleware(func)

    server = WSGIServer(('localhost', port), func)
    server.timeout = 50

    try:
        server.start()
    except KeyboardInterrupt:
        server.stop()




class PageObjectExample(unittest.TestCase):
    #
    def setUp(self):
        parser = OptionParser()
        parser.add_option("-p", "--port", type="int", dest="port", default=0,
                          help="port to run server on (defaults to any available port)")
        parser.add_option("-b", "--browser", dest="browser", default="firefox",
                          help="preferred browser")
        parser.add_option("-i", "--init", action="store_true", dest="initialize",
                          help="(re)initialize settings")
        parser.add_option("-r", "--reset", action="store_true", dest="reset",
                          help="reset project database (valid only with -i and without -d)")

        (options, args) = parser.parse_args()
    
        if options.initialize or not os.path.exists('settings.py'):
            if options.reset :
                init(reset=True)
            else:
                init(reset=False)
        
        if (options.port < 1):
            options.port = get_unused_ip_port()    

        self.server = Process(target=pro, args=(options.port,))

        self.server.start()

        # DesiredCapabilities capabilities = DesiredCapabilities.chrome();
        # capabilities.setCapability("chrome.binary", "/usr/lib/chromium-browser/chromium-browser");
        # WebDriver driver = new ChromeDriver(capabilities);

        if options.browser == "firefox":
            self.browser = webdriver.Firefox()
        elif options.browser == "chrome" :
            # for Chrome, need to get chromedriver exe from
            #    http://code.google.com/p/chromium/downloads/list
            self.browser = webdriver.Chrome(executable_path='/hx/u/hschilli/bin/chromedriver')

        #self.browser.get("http://localhost:%d" % options.port) # Load page

        self.port = options.port

        #
    def testLogin(self):
        lpo = LoginPageObject(self.browser, self.port)
        projects_page = lpo.login_successfully("herb", "herb" )
        #import pdb; pdb.set_trace()
        self.assertEqual( "Projects", projects_page.page_title )
        # lpo.username = "herb"
        # lpo.password = "herb"
        # lpo.submit()
        #
    def tearDown(self):
        self.browser.close()
        self.server.terminate()

        #
if __name__ == "__main__":
    unittest.main()
