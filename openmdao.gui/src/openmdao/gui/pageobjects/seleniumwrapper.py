from selenium import selenium
#
class SeleniumWrapper(object):
    
    # singleton
    _instance = None
    #
    def __new__(cls, *args, **kwargs):
        if not cls._instance:
            cls._instance = super(SeleniumWrapper, cls).__new__(cls, *args, **kwargs)
            return cls._instance
    #
    def connect(self, host, port, browser, server):
        self.connection = selenium(host, port, browser, server)
        return self.connection

from selenium import webdriver

class WebDriverWrapper(object):
    
    # singleton
    _instance = None
    #
    def __new__(cls, *args, **kwargs):
        if not cls._instance:
            cls._instance = super(WebDriverWrapper, cls).__new__(cls, *args, **kwargs)
            return cls._instance
    #
    def connect(self, host, port, browser, server):
        self.connection = selenium(host, port, browser, server)
        return self.connection

