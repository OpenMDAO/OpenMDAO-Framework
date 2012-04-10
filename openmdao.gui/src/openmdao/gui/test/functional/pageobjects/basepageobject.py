import sys
import unittest
 	
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import ElementNotVisibleException


class BasePageObject(unittest.TestCase):

    def __init__(self, browser, port):

        self.browser = browser
        self.port = port

        self._page_title = None
        self._page_url = None

        print >>sys.stderr, "**** Creating new page object %s" % self.__class__.__name__

        #import pdb; pdb.set_trace()
        print >>sys.stderr, "    port", port
        

    @property
    def page_title(self):
        WebDriverWait(self.browser, 10).until(lambda s: self.browser.title)
        return self.browser.title

    @property
    def page_url(self):
        return self.browser.current_url

    def go_to(self):
        print >>sys.stderr, "go_to", self._page_url
        self.browser.get(self._page_url)
        print >>sys.stderr, "    done"
        
    def is_the_current_page(self):
        if self._page_title:
            #import pdb; pdb.set_trace()
            return self.page_title.startswith( self._page_title )
        else:
            return True # TODO: Not really sure what to do yet here

    def assert_on_correct_page(self):
        if self._page_title:
            if not self.page_title.startswith( self._page_title ) :
                raise self.failureException(
                    "Expected page title to start with '%s' but found '%s'" \
                    % ( self._page_title, self.page_title))
        
    def is_element_present(self, *locator):
        #self.browser.implicitly_wait(0)
        try:
            self.browser.find_element(*locator)
            return True
        except NoSuchElementException:
            return False
        #finally:
            # set back to where you once belonged
            #self.browser.implicitly_wait(self.testsetup.default_implicit_wait)

    def is_element_visible(self, *locator):
        try:
            return self.browser.find_element(*locator).is_displayed()
        except NoSuchElementException, ElementNotVisibleException:
            return False


