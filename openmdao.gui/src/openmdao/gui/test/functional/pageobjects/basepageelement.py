#from pageobjects import selenium_server_connection
#
class BasePageElement(object):
  #
  def __get__(self, obj, cls=None):
    selenium_server_connection.get_text(self.locator)
    #
  def __delete__(self, obj):
    pass
