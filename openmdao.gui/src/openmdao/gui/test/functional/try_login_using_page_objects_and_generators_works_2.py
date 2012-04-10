'''
  Run the use case described on
    https://docs.google.com/document/d/16m74ZhD1_TpgmGH_RPTUGthIMCGNsxRhJOpucbDX_4E/edit?hl=en_US
'''
import sys
import unittest
import os
import time
from multiprocessing import Process
from optparse import OptionParser

from pageobjects import locators
from pageobjects.openmdao_login import LoginPageObject, ProjectsListPageObject

from openmdao.util.network import get_unused_ip_port

from selenium import webdriver

from openmdao.gui.omg import init, dev, pro

from nose.tools import eq_ as eq
from nose.tools import raises, timed, with_setup

sys.path.append( "." )

port = None
server = None

def setup_server() :
    global port
    port = get_unused_ip_port()    
    global server
    server = Process(target=pro, args=(port,))
    server.start()

def teardown_server():
    server.terminate()
    
@with_setup(setup_server, teardown_server)
def test_generator(): 
    for browser_name in [ 'firefox', 'chrome' ]:
        for _test in [ _test_successful_login, _test_unsuccessful_login ]:
            yield _test, browser_name

def successful_login(browser):
        login_page = LoginPageObject(browser, port)
        eq( "Login", login_page.page_title )
        projects_page = login_page.login_successfully("herb", "herb" )
        eq( "Projects", projects_page.page_title )
        eq( "http://localhost:%d/" % port, projects_page.page_url )
        assert projects_page.is_element_present( *projects_page.locators["logout"] )

def _test_successful_login(browser_name):
    if browser_name == "firefox":
        browser = webdriver.Firefox()
    elif browser_name == "chrome" :
        browser = webdriver.Chrome(executable_path='/hx/u/hschilli/bin/chromedriver')
    successful_login(browser)
    browser.close()

def unsuccessful_login(browser):
    login_page = LoginPageObject(browser, port)
    eq( "Login", login_page.page_title )
    new_login_page = login_page.login_unsuccessfully("herb", "notherb" )
    eq( "Login", new_login_page.page_title )

def _test_unsuccessful_login(browser_name):
    if browser_name == "firefox":
        browser = webdriver.Firefox()
    elif browser_name == "chrome" :
        browser = webdriver.Chrome(executable_path='/hx/u/hschilli/bin/chromedriver')
    unsuccessful_login(browser)
    browser.close()


if __name__ == "__main__":
    unittest.main()
