from pageobjects import locators, selenium_server_connection
from pageobjects.basepageobject import BasePageObject
from pageobjects.basepageelement import BasePageElement
 	
class UsernameElement(BasePageElement):
    #
    def __init__(self):
        self.locator = locators["login.username"]
        #
    def __set__(self, obj, val):
        se = selenium_server_connection.connection
        se.type(self.locator, val)
        #
class PasswordElement(BasePageElement):
    #
    def __init__(self):
        self.locator = locators["login.password"]
        #
    def __set__(self, obj, val):
        se = selenium_server_connection.connection
        se.type(self.locator, val)
        #
class LoginPageObject(BasePageObject):
    #
    username = UsernameElement()
    password = PasswordElement()
    #
    def __init__(self, se):
        self.se = se
        self.se.open("/login")
        self.assertEqual("My Application - Login", self.se.get_title())
        #
    def submit(self):
        wait_for = "selenium.browserbot.getCurrentWindow().document.getElementById('LogoutButton')"
        self.se.click(locators["login.submit"])
        self.se.wait_for_condition(wait_for, "30000")
