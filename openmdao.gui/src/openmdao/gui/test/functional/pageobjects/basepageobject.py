import time

from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException

# Default WebDriverWait timeout.
TMO = 15


def rgba(color):
    """ Return 4-element list of red, green, blue, alpha values. """
    if color.startswith('rgba'):
        values = [int(v) for v in color[5:-1].split(',')]
    else:
        values = [int(v) for v in color[4:-1].split(',')]
        values.append(1)
    return values


class BasePageObject(object):
    """
    PageObject abstracts the details of page element layout and low-level
    operations so that tests can be higher level and more robust.

    browser: :class:`WebDriver`
        Browser-appropriate WebDriver.

    port: int
        Port on ``localhost`` to use.

    root: :class:`WebElement`
        Optional 'root' element of page. If not ``None``, then page
        elements are found by serching from this point rather than the
        entire browser page.

    Elements on this page should be subclasses of :class:`BaseElement` which
    implements the Python descriptor protocol.

    An example page declaration::

        class ProjectPage(BasePageObject):

            project_name = InputElement((By.ID, 'id_projectname'))
            description = InputElement((By.ID, 'id_description'))
            version = InputElement((By.ID, 'id_version'))

    """

    url = None
    title_prefix = None

    def __init__(self, browser, port, root=None):
        self.browser = browser
        self.port = port
        self.root = root
        # Note that we're testing. This alters some 'pretty' behavior.
        self.browser.execute_script('openmdao_test_mode = true;')

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
        return WebDriverWait(self.browser, TMO).until(
                   lambda browser: browser.title)

    @property
    def page_url(self):
        """ Current browser URL. """
        return self.browser.current_url

    def go_to(self):
        """ Go to this page's URL. """
        self.browser.get('http://localhost:%d%s' % (self.port, self.url))
        return self.verify(self.browser, self.port)

    @classmethod
    def verify(cls, browser, port, prefix=None):
        """
        Verify that we're on the page by checking `title_prefix`.
        Returns self.
        """
        time.sleep(0.5)  # Pacing for page load.
        prefix = prefix or cls.title_prefix
        if prefix:
            try:
                WebDriverWait(browser, TMO).until(
                    lambda browser: browser.title.startswith(prefix))
            except TimeoutException:
                raise TimeoutException('Waiting for title %r' % prefix)
        return cls(browser, port)
