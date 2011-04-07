
from selenium import selenium
import unittest, time, re, os

from multiprocessing        import Process
from openmdao.gui.mdao_util import PickUnusedPort
from openmdao.gui.mdao      import launch_server

class test_basicgui(unittest.TestCase):
    def setUp(self):
        os.chdir('..')  # so server can find it's static files
        self.port = PickUnusedPort()
        self.server = Process(target=launch_server,args=(self.port,))
        self.server.start()
        self.url = "http://localhost:"+str(self.port)
        
        self.verificationErrors = []
        self.selenium = selenium("localhost", 4444, "*chrome", self.url)
        self.selenium.start()
    
    def test_basicstuff(self):
        sel = self.selenium
        sel.open("/login")
        sel.type("username", "testuser")
        sel.type("password", "testpass")
        sel.click("submit")
        sel.wait_for_page_to_load("30000")
        try: self.failUnless(sel.is_text_present("Session^"))
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.failUnless(sel.is_text_present("File^"))
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.failUnless(sel.is_text_present("View^"))
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.failUnless(sel.is_text_present("Install Addon"))
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.failUnless(sel.is_text_present("About"))
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.failUnless(sel.is_text_present("New"))
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.failUnless(sel.is_text_present("Objects"))
        except AssertionError, e: self.verificationErrors.append(str(e))
        sel.click("otree_tab")
        sel.click("ftree_tab")
        sel.click("dataflow_tab")
        sel.click("editor_tab")
        try: self.failUnless(sel.is_text_present("1"))
        except AssertionError, e: self.verificationErrors.append(str(e))
        sel.click("palette_tab")
        for i in range(60):
            try:
                if sel.is_text_present("openmdao"): break
            except: pass
            time.sleep(1)
        else: self.fail("time out")
        sel.click("//div[@id='library']/div[1]/h3")
        sel.click("//div[@id='library']/ul[1]/div[3]/h3")
        try: self.failUnless(sel.is_text_present("Assembly"))
        except AssertionError, e: self.verificationErrors.append(str(e))
        sel.click("properties_tab")
        try: self.failUnless(sel.is_text_present("Property"))
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.failUnless(sel.is_text_present("Value"))
        except AssertionError, e: self.verificationErrors.append(str(e))
        sel.click("//ul[@id='history-menu']/li")
        sel.click("link=Command Line")
        sel.type("command", "A=1")
        sel.click("command-button")
        sel.click("leftcol")
        sel.click("otree_tab")
        sel.select_window("null")
        sel.click("link=A")
        sel.click("palette_tab")
        sel.click("//div[@id='library']/div[1]/h3")
        sel.click("//div[@id='library']/ul[1]/div[3]/h3")
        sel.click("dataflow_tab")
        
        # don't forget to exit (shutting down the server)
        sel.click("link=Exit")
    
    def tearDown(self):
        self.selenium.stop()
        self.assertEqual([], self.verificationErrors)
        try:
            print "terminating server..."
            self.server.terminate()
        except Exception,e:
            print "server terminate failed: ", e
            
if __name__ == "__main__":
    unittest.main()
