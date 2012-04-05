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
import time

import unittest, time, re, os, inspect


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


class test_basicgui(unittest.TestCase):

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

        self.p = Process(target=pro, args=(options.port,))

        self.p.start()

        # DesiredCapabilities capabilities = DesiredCapabilities.chrome();
        # capabilities.setCapability("chrome.binary", "/usr/lib/chromium-browser/chromium-browser");
        # WebDriver driver = new ChromeDriver(capabilities);



        if options.browser == "firefox":
            self.browser = webdriver.Firefox()
        elif options.browser == "chrome" :
            # for Chrome, need to get chromedriver exe from
            #    http://code.google.com/p/chromium/downloads/list
            self.browser = webdriver.Chrome(executable_path='/hx/u/hschilli/bin/chromedriver')

        self.browser.get("http://localhost:%d" % options.port) # Load page

    def add_file( self, file_path ):
        ''' read in a file '''

        # Click the Files tab
        WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element_by_xpath('//*[@id="ftree_tab"]') ).click()

        # Click File ^ menu - need to do this to make the menu item visible
        WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element_by_xpath('/html/body/div/dl/dd[2]/div/nav2/ul/li/a') ).click()

        # Click Add File menu item
        time.sleep(3)
        WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element_by_xpath('/html/body/div/dl/dd[2]/div/nav2/ul/li/ul/li[3]/a') ).click()

        # Switch to the Window that pops up
        main_window_handle = self.browser.current_window_handle
        self.browser.switch_to_window( 'Add File' )

        file_input_element = WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element_by_name('myfile') )

        file_input_element.send_keys(file_path)

        # Submit
        WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element_by_xpath('/html/body/div/div[2]/form/input[2]') ).click()

        # go back to the main window
        self.browser.switch_to_window( main_window_handle )

    def open_plotter( self ):
        WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[3]/a') ).click() # Tools menu
        action_chains = ActionChains(self.browser)
        #plotter = self.browser.find_element_by_id("refresh-menu")
        plotter = self.browser.find_element_by_link_text("Plotter")
        action_chains.move_to_element( plotter ).click_and_hold( plotter ).release( plotter ).perform()


    def view_code_editor( self ):
        WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/a') ).click() # View menu
        action_chains = ActionChains(self.browser)
        #plotter = self.browser.find_element_by_id("refresh-menu")
        #plotter = self.browser.find_element_by_link_text("Code Editor")
        plotter = self.browser.find_elements_by_partial_link_text("Code Editor")
        #plotter = self.browser.find_element_by_css_selector( "body.ui-layout-container nav#menu.ui-layout-north ul li ul li a" )



        
        action_chains.move_to_element( plotter ).click_and_hold( plotter ).release( plotter ).perform()

    def view_workflow( self ):
        WebDriverWait(self.browser,10).until(
            lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/a') ).click() # View menu
        action_chains = ActionChains(self.browser)
        #plotter = self.browser.find_element_by_id("refresh-menu")
        plotter = self.browser.find_element_by_link_text("Workflow")
        action_chains.move_to_element( plotter ).click_and_hold( plotter ).release( plotter ).perform()


    def test_use_case_1(self):

        # good doc for the API is at http://readthedocs.org/docs/selenium-python/en/latest/api.html
        # also https://gist.github.com/1099713
        # The  XPath values in this code were obtained using Firebug
        
        try:
            #self.browser.find_element_by_xpath("//a[contains(@href,'/accounts/register/')]")

            self.browser.find_element_by_xpath("/html/body/div/div[2]/p/a")
        except NoSuchElementException:
            assert 0, "Cannot find link to register for an account"






        ####################
        # Log in
        ####################
        self.browser.find_element_by_name("username").send_keys("herb")
        self.browser.find_element_by_name("password").send_keys("herb")
        self.browser.find_element_by_xpath("/html/body/div/div[2]/form/input").submit()


        ####################
        # Create a new project
        ####################
        # we have to wait for the page to refresh, the last thing that seems to be updated is the title
        WebDriverWait(self.browser,10).until(lambda browser:self.browser.title.startswith("Projects"))
        self.browser.find_element_by_link_text( "New project").click()
        WebDriverWait(self.browser,10).until(lambda browser:self.browser.title.startswith("New Project"))
        self.browser.find_element_by_xpath("/html/body/div/div[2]/form/input").submit()

        ####################
        # Load this project into Workspace
        ####################
        self.browser.find_element_by_xpath("/html/body/div/div[2]/form[3]/input[2]").click()


        self.view_code_editor()

        time.sleep(2)

        self.open_plotter()

        time.sleep(2)

        self.view_workflow()



        #time.sleep(10)
        #self.browser.find_element_by_link_text("Plotter").click()

        # This works  for the Plotter menu!!!! 
        # Tools menu
        # WebDriverWait(self.browser,10).until(
        #      lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[3]/a') ).click()
        # action_chains = ActionChains(self.browser)
        # plotter = self.browser.find_element_by_link_text("Plotter")
        # action_chains.move_to_element( plotter ).click_and_hold( plotter ).release( plotter ).perform()

        # Wait for the workspace to load. Should avoid using sleep! 
        time.sleep(10)




        time.sleep(2)
        
        # add a file
        import openmdao.examples.simple.paraboloid
        file_path = openmdao.examples.simple.paraboloid.__file__
        if file_path.endswith( ".pyc" ):
            file_path = file_path[ :-1 ]
        self.add_file( file_path )







        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[3]/a') ).click() # Tools menu
        # action_chains = ActionChains(self.browser)
        # #code_editor = self.browser.find_element_by_id("code")
        # plotter = self.browser.find_element_by_link_text("Plotter")
        # action_chains.move_to_element( plotter ).click_and_hold( plotter ).release( plotter ).perform()


















        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[3]/a') ).click() # Tools menu
        # action_chains = ActionChains(self.browser)
        # #code_editor = self.browser.find_element_by_id("code")
        # plotter = self.browser.find_element_by_link_text("Refresh")
        # action_chains.move_to_element( plotter ).click_and_hold( plotter ).release( plotter ).perform()
        # #action_chains.move_to_element( code_editor ).click_and_hold( code_editor ).release( code_editor ).perform()


        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/a') ).click() # View menu
        # action_chains = ActionChains(self.browser)
        # code_editor = self.browser.find_element_by_id("code")
        # #plotter = self.browser.find_element_by_link_text("Refresh")
        # #action_chains.move_to_element( plotter ).click_and_hold( plotter ).release( plotter ).perform()
        # action_chains.move_to_element( code_editor ).click_and_hold( code_editor ).release( code_editor ).perform()


        # time.sleep(3)
        # WebDriverWait(self.browser,10).until(
        #      lambda browser:self.browser.find_element_by_id('refresh-menu') ).click()




        
        # ####################
        # # Refresh using menu item I put on the file menu
        # ####################
        # # Click File ^ menu - need to do this to make the menu item visible
        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/div/dl/dd[2]/div/nav2/ul/li/a') ).click()

        # # # Click refresh menu item
        # time.sleep(3)
        # WebDriverWait(self.browser,10).until(
        #      lambda browser:self.browser.find_element_by_id('refqqq') ).click()

        # print " used file menu to refresh", 80*"="


        # # tools menu
        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[3]/a') ).click()
        # # Plotter
        # time.sleep(3)
        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[3]/ul/li/a') ).click()





        ####################
        #  Refresh
        ####################
        # if self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/ul/li[11]/a') :
        #     print "refresh element found before view clicked"
        
        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/a') ).click()

        # print "Clicked on View"
        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/a') ).click()
        # #time.sleep(2)
        # refresh_menu_item = WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_id('refresh-menu') )
        # print refresh_menu_item.is_enabled()
        # print refresh_menu_item.is_displayed()
        # print refresh_menu_item.location
        # print refresh_menu_item.text
        # print refresh_menu_item.tag_name
        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_id('refresh-menu') ).click()
        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_id('code') ).click()


        # print "Using id. sleeping..."
        # time.sleep(20)

        
        #/html/body/nav/ul/li[2]/ul/li[2]/a



        # if self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/ul/li[2]/a') :
        #     print "refresh element found after view clicked"
        #     m = self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/ul/li[2]/a') 
        #     print dir( m )
        #     print "text is ", m.text

        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/ul/li[2]/a') ).click()
        

        # # try another way to refresh
        # action_chains = ActionChains(self.browser)
        # view_menu = WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/a') )
        # action_chains.move_to_element( view_menu ).perform()

        # action_chains = ActionChains(self.browser)
        # refresh_menu_item = WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/ul/li[11]/a') )
        # action_chains.move_to_element( refresh_menu_item ).click( refresh_menu_item ).perform()

        # print "done with refresh", 80*"="

        # # try another way to refresh aAGAIN
        # action_chains = ActionChains(self.browser)
        # view_menu = WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/a') )
        # action_chains.move_to_element( view_menu ).click( view_menu ).perform()

        # action_chains = ActionChains(self.browser)
        # refresh_menu_item = WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/ul/li[11]/a') )
        # action_chains.move_to_element( refresh_menu_item ).click( refresh_menu_item ).perform()

        # print "done with refresh again", 80*"="

        #time.sleep(20 )


        # # do a refresh
        #WebDriverWait(self.browser,10).until(
        #    lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/a') ).click()
        #time.sleep(2)
        #refresh_menu_item = WebDriverWait(self.browser,10).until(
        #    lambda browser:self.browser.find_element_by_xpath('/html/body/nav/ul/li[2]/ul/li[11]/a') )

        #print "refresh_menu_item = ", refresh_menu_item
        #print "dir(refresh_menu_item) = ", dir( refresh_menu_item )

        #refresh_menu_item.click()

        # time.sleep(3)
        # # Now the paraboloid.py file is visible, right click on it to import it
        # paraboloid_file = WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/div/dl/dd[2]/div/div[2]/ul/li/a') )


        # action_chains = ActionChains(self.browser)
        # action_chains.context_click(paraboloid_file).perform()

        # # Import * from file
        # WebDriverWait(self.browser,10).until(
        #     lambda browser:self.browser.find_element_by_xpath('/html/body/div[7]/ul/li[7]/a') ).click()


        time.sleep(30)


    def tearDown(self):
        self.browser.close()
        self.p.terminate()


if __name__ == "__main__":
    unittest.main()
