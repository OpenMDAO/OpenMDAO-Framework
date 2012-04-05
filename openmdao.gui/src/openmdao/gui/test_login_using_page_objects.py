'''
  Run the use case described on
    https://docs.google.com/document/d/16m74ZhD1_TpgmGH_RPTUGthIMCGNsxRhJOpucbDX_4E/edit?hl=en_US
'''
import sys
import unittest
import os
from multiprocessing import Process
from optparse import OptionParser

from pageobjects import locators
from pageobjects.openmdao_login import LoginPageObject, ProjectsListPageObject

from openmdao.util.network import get_unused_ip_port

from selenium import webdriver

from openmdao.gui.omg import init, dev, pro

def ensure_dir(d):
    ''' create directory if it does not exist
    '''
    if not os.path.isdir(d):
        os.makedirs(d)

# TODO: add implicit waits

class PageObjectExample(unittest.TestCase):
    #
    def setUp(self):

        
        parser = OptionParser()
        parser.add_option("-b", "--browser", dest="browser", default="firefox",
                          help="preferred browser")
        (options, args) = parser.parse_args()
    
        self.port = get_unused_ip_port()    
        self.server = Process(target=pro, args=(self.port,))
        self.server.start()

        # DesiredCapabilities capabilities = DesiredCapabilities.chrome();
        # capabilities.setCapability("chrome.binary", "/usr/lib/chromium-browser/chromium-browser");
        # WebDriver driver = new ChromeDriver(capabilities);

        # TODO: Can we re-use the webdriver instances across tests? What about the server? Use singletons?

        # TODO: Need to run tests both with firefox and chrome
        
        if options.browser == "firefox":
            self.browser = webdriver.Firefox()
        elif options.browser == "chrome" :
            # for Chrome, need to get chromedriver exe from
            #    http://code.google.com/p/chromium/downloads/list
            self.browser = webdriver.Chrome(executable_path='/hx/u/hschilli/bin/chromedriver')

    def test_successful_login(self):
        login_page = LoginPageObject(self.browser, self.port)
        self.assertEqual( "Login", login_page.page_title )
        projects_page = login_page.login_successfully("herb", "herb" )
        self.assertEqual( "Projects", projects_page.page_title )
        self.assertEqual( "http://localhost:%d/" % self.port, projects_page.page_url )
        assert projects_page.is_element_present( *locators["openmdao_projects.logout"] )

    def test_unsuccessful_login(self):
        login_page = LoginPageObject(self.browser, self.port)
        self.assertEqual( "Login", login_page.page_title )
        new_login_page = login_page.login_unsuccessfully("herb", "notherb" )
        self.assertEqual( "Login", login_page.page_title )
    
    # def test_adding_new_project_does_it_show_on_list(self):
    #     pass

    def tearDown(self):
        self.browser.close()
        self.server.terminate()

if __name__ == "__main__":
    unittest.main()
