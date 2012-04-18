from selenium.webdriver.support.ui import WebDriverWait


class BasePageObject(object):
    """
    PageObject abstracts the details of page element layout and low-level
    operations so that tests can be higher level and more robust.

    browser:
        Browser-appropriate WebDriver.

    port: int
        Port on ``localhost`` to use.

    title: string
        Expected page title.

    url: string
        URL for page.

    Elements on this page should be subclasses of :class:`BaseElement` which
    implements the Python descriptor protocol.

    An example page declaration::

        class ProjectPage(BasePageObject):

            project_name = InputElement((By.ID, 'id_projectname'))
            description = InputElement((By.ID, 'id_description'))
            version = InputElement((By.ID, 'id_version'))
            shared = CheckboxElement((By.ID, 'id_shared'))

    """

    def __init__(self, browser, port, url=None):
        self.browser = browser
        self.port = port
        self._page_url = url

    def __call__(self, element_name):
        """
        Return the page element corresponding to `element_name`.
        Just doing ``self.element_name`` will get/set the value of the element.
        Using ``self('element_name')`` returns the element object, so
        something other than its value can be accessed.
        """
        descriptor = getattr(self.__class__, element_name)
        return descriptor.get(self)

    @property
    def page_title(self):
        """ Current browser title. """
        return WebDriverWait(self.browser, 10).until(
                                                  lambda browser: browser.title)
    @property
    def page_url(self):
        """ Current browser URL. """
        return self.browser.current_url

    def go_to(self):
        """ Go to this page's URL. """
        self.browser.get(self._page_url)
        self.browser.execute_script("var openmdao_test_mode = true;")

