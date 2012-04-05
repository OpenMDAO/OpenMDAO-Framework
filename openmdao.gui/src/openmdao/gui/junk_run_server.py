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

port = get_unused_ip_port()    
server = Process(target=pro, args=(port,))
server.start()




time.sleep(3)


browser = webdriver.Firefox()
login_page = LoginPageObject(browser, port)
projects_page = login_page.login_successfully("herb", "herb" )

