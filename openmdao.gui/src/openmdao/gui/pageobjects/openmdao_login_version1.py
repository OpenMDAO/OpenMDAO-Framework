from pageobjects import locators, selenium_server_connection
from pageobjects.basepageobject import BasePageObject
from pageobjects.basepageelement import BasePageElement
 	
from selenium.webdriver.support.ui import WebDriverWait

class UsernameElement(BasePageElement):
    #
    def __init__(self, browser):

        import pdb; pdb.set_trace()

        self.browser= browser
        self.locator = locators["openmdao_login.username"]
        #
    def __set__(self, obj, val):
        #self.browser.find_element_by_id(self.locator).send_keys(val)

        import pdb; pdb.set_trace()

        self.browser.find_element(*self.locator).send_keys(val)
        #
class PasswordElement(BasePageElement):
    #
    def __init__(self, browser):
        self.browser= browser
        self.locator = locators["openmdao_login.password"]
        #
    def __set__(self, obj, val):
        #self.browser.find_element_by_id(self.locator).send_keys(val)
        self.browser.find_element(*self.locator).send_keys(val)
        #
class ProjectsListPageObject(BasePageObject):
    #
    #username = UsernameElement()
    #password = PasswordElement()
    #
    def __init__(self, browser, port):

        self.browser = browser
        self.port = port
        #self.browser.get("http://localhost:%d/accounts/login" % self.port)
        #self.assertEqual("", self.browser.title)
        #
    @property
    def page_title(self):
        WebDriverWait(self.browser, 10).until(lambda s: self.browser.title)
        return self.browser.title

    def get_title( self ) :
        return self.browser.title
    

class LoginPageObject(BasePageObject):
    #
    #username = UsernameElement()
    #password = PasswordElement()
    #
    def __init__(self, browser, port):

        self.browser = browser
        self.port = port
        #self.username = UsernameElement(self.browser)
        #self.password = PasswordElement(self.browser)
        self.browser.get("http://localhost:%d/accounts/login/?next=/" % self.port)
        self.assertEqual("", self.browser.title)

    # username prop #
    @property
    def username(self):
        return self._username

    @username.setter
    def username(self, username):
        print "in username setter"
        self.browser.find_element(*locators["openmdao_login.username"]).send_keys(username)
        self._username = username

    # password prop #
    @property
    def password(self):
        return self._password

    @password.setter
    def password(self, password):
        self.browser.find_element(*locators["openmdao_login.password"]).send_keys(password)
        self._password = password

    def login_successfully( self, username, password ) :
        self.username = username
        self.password = password
        self.submit()
        return ProjectsListPageObject( self.browser, self.port )
    
    @property
    def page_title(self):
        WebDriverWait(self.browser, 10).until(lambda s: self.browser.title)
        return self.browser.title


    def submit(self):
        #wait_for = "selenium.browserbot.getCurrentWindow().document.getElementById('LogoutButton')"
        locator = locators["openmdao_login.submit"]
        #self.browser.find_element_by_xpath(locators["openmdao_login.submit"]).click()
        self.browser.find_element(*locator).click()
        #self.se.wait_for_condition(wait_for, "30000")


