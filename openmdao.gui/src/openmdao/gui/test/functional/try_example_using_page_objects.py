from pageobjects.login import LoginPageObject
from pageobjects import selenium_server_connection
#
import sys, unittest, re, time, os.path, logging
#
class PageObjectExample(unittest.TestCase):
    #
    def setUp(self):
        self.log = logging.getLogger("pragmatic.pageobjectexample")
        self.verificationErrors = []
        self.selenium = selenium_server_connection.connect("localhost", 4444, "*chrome",
                                           "http://some.test.site")
        self.selenium.start()
        #
    def testLogin(self):
        lpo = LoginPageObject(self.selenium)
        lpo.username = "adam@element34.ca"
        lpo.password = "password"
        lpo.submit()
        #
    def tearDown(self):
        self.selenium.stop()
        self.assertEqual([], self.verificationErrors)
        #
if __name__ == "__main__":
    unittest.main()
