
import unittest, time, re, os, inspect
from selenium import selenium

from multiprocessing        import Process
from openmdao.util.network  import get_unused_ip_port
from openmdao.gui.mdao      import run_server

class test_basicgui(unittest.TestCase):
    
    def setUp(self):
        self.startdir = os.getcwd()
        gui_path = os.path.dirname(inspect.getfile(run_server))
        os.chdir(gui_path)  # so server can find it's static files

        self.port = get_unused_ip_port()
        self.server = Process(target=run_server,args=(self.port,))
        self.server.start()
        self.url = "http://localhost:"+str(self.port)
        
        self.verificationErrors = []
        # the selenium standalone server should be running on port 4444
        self.selenium = selenium("localhost", 4444, "*chrome", self.url)
        self.selenium.start()
    
    def test_basicstuff(self):
        sel = self.selenium
        sel.set_speed("1000")   # s l o w l y
        sel.open("/login")
        sel.type("username", "testuser")
        sel.type("password", "testpass")
        sel.click("submit")
        sel.wait_for_page_to_load("30000")
        try: self.assertTrue(sel.is_text_present("Session^"),'Session menu is missing')
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.assertTrue(sel.is_text_present("File^"),'File menu is missing')
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.assertTrue(sel.is_text_present("View^"),'View menu is missing')
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.assertTrue(sel.is_text_present("Install Addon"),'Addon menu is missing')
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.assertTrue(sel.is_text_present("About"),'About menu is missing')
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.assertTrue(sel.is_text_present("New"),'New menu option is missing')
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.assertTrue(sel.is_text_present("Objects"),'Options tab is missing')
        except AssertionError, e: self.verificationErrors.append(str(e))
        sel.click("otree_tab")
        sel.click("ftree_tab")
        sel.click("dataflow_tab")
        sel.click("editor_tab")
        try: self.assertTrue(sel.is_text_present("1"),'Line numbers missing in editor')
        except AssertionError, e: self.verificationErrors.append(str(e))
        sel.click("palette_tab")
        for i in range(60):
            try:
                if sel.is_text_present("openmdao"): break
            except: pass
            time.sleep(1)
        else: self.fail("time out")
        sel.click("//div[@id='library']/div[1]/h3")             # openmdao
        sel.click("//div[@id='library']/ul[1]/div[2]/h3")       # main
        sel.click("//div[@id='library']/ul[1]/ul[2]/div/h3")    # assembly
        try: self.assertTrue(sel.is_text_present("Assembly"),'Assembly not found in palette')
        except AssertionError, e: self.verificationErrors.append(str(e))
        sel.click("properties_tab")
        try: self.assertTrue(sel.is_text_present("Property"),'Property not found in Properties panel')
        except AssertionError, e: self.verificationErrors.append(str(e))
        try: self.assertTrue(sel.is_text_present("Value"),'Value not found in Properties panel')
        except AssertionError, e: self.verificationErrors.append(str(e))
        sel.click("//ul[@id='history-menu']/li")
        sel.click("link=Command Line")
        sel.type("command", "A=1")
        sel.click("command-button")
        sel.click("leftcol")
        sel.click("otree_tab")
        sel.select_window("null")
        sel.click("link=A")
        
        # don't forget to exit (shutting down the server)
        sel.click("link=Exit")
    
    def tearDown(self):
        os.chdir(self.startdir)
        self.selenium.stop()
        self.assertEqual([], self.verificationErrors)
        # wait 5 seconds, then verify server has shut down (kill it if not)
        # time.sleep(5)
        # if self.server.is_alive:
            # print "server still running after 5 sec, terminating..."
            # self.server.terminate()
            
if __name__ == "__main__":
    unittest.main()
