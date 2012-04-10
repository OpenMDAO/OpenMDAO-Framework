#from pageobjects.seleniumwrapper import WebDriverWrapper
#

#web_driver_connection = WebDriverWrapper()

from selenium.webdriver.common.by import By

locators = {}
locators["login.username"] = "username"
locators["login.password"] = "password"
locators["login.submit"] = "login"

locators["openmdao_login.username"] = "id_username"
locators["openmdao_login.password"] = "id_password"
locators["openmdao_login.submit"] = "/html/body/div/div[2]/form/input"

locators["openmdao_login.username"] = ( By.ID, "id_username" )
locators["openmdao_login.password"] = ( By.ID, "id_password" )
locators["openmdao_login.submit"] = ( By.XPATH, "/html/body/div/div[2]/form/input" )

locators["openmdao_projects.logout"] = ( By.XPATH, "/html/body/a" )
