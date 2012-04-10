import sys
import os
import inspect
from distutils.spawn import find_executable
from multiprocessing import Process
from openmdao.util.network import get_unused_ip_port
from selenium import webdriver
from openmdao.gui.omg import pro

port = None
server = None

browsers_to_test = [
    #"firefox",
    "chrome"
    ]

browsers = []
if "firefox" in browsers_to_test :
    firefox = webdriver.Firefox()
    firefox.implicitly_wait(15)
    browsers.append( firefox )
if "chrome" in browsers_to_test :
    chromedriver_path = find_executable( 'chromedriver' )
    #chrome = webdriver.Chrome(executable_path=chromedriver_path)
    chrome = webdriver.Chrome(executable_path='/hx/u/hschilli/bin/chromedriver')
    chrome.implicitly_wait(15)
    browsers.append( chrome )

def setup_server() :
    '''The function gets called once before any of the
    tests are called'''
    
    global port
    global server

    port = get_unused_ip_port()    
    gui_path = os.path.dirname(inspect.getfile(pro))
    os.chdir(gui_path)  # so server can find its static files
    sys.path.append( gui_path )
    server = Process(target=pro, args=(port,))
    server.start()
        
def teardown_server():
    '''The function gets called once after all of the
    tests are called'''
    
    for browser in browsers:
        browser.close()
    server.terminate()
