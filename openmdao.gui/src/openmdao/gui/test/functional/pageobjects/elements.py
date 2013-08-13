"""
Element descriptors and underlying object types which are intended to be used
with BasePageObject.
"""

import logging
import time

from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import NoSuchElementException, \
                                       ElementNotVisibleException, \
                                       StaleElementReferenceException
from basepageobject import TMO, rgba
from grid import Grid


class _BaseElement(object):
    """
    Base for element objects, retains browser driver (from `page`) and locator.

    page: :class:`BasePageObject`
        Page object containing ``browser`` attribute.

    locator: tuple
        WebDriver locator used to find this element in `page`.
    """

    def __init__(self, page, locator):
        self._browser = page.browser
        self._locator = locator
        self._root = page.root

    @property
    def element(self):
        """ The element on the page. """
        if self._root is None:
            return WebDriverWait(self._browser, TMO).until(
                       lambda browser: browser.find_element(*self._locator))
        else:
            return WebDriverWait(self._browser, TMO).until(
                       lambda browser: self._root.find_element(*self._locator))

    @property
    def color(self):
        """ Return RGBA values for ``color`` property. """
        return rgba(self.value_of_css_property('color'))

    @property
    def background_color(self):
        """ Return RGBA values for ``background-color`` property. """
        return rgba(self.value_of_css_property('background-color'))

    @property
    def is_present(self):
        """ True if the element can be found. """
        self._browser.implicitly_wait(1)
        try:
            if self._root is None:
                self._browser.find_element(*self._locator)
            else:
                self._root.find_element(*self._locator)
        except (NoSuchElementException,
                StaleElementReferenceException):
            return False
        finally:
            self._browser.implicitly_wait(TMO)
        return True

    @property
    def is_visible(self):
        """ True if the element is visible. """
        try:
            return self.element.is_displayed()
        except (NoSuchElementException,
                ElementNotVisibleException,
                StaleElementReferenceException):
            return False

    @property
    def location(self):
        """ Return dict containing x and y coords. """
        return self.element.location

    def find_element_by_xpath(self, xpath):
        """ Convenience routine. """
        return self.element.find_element_by_xpath(xpath)

    def find_elements_by_xpath(self, xpath):
        """ Convenience routine. """
        return self.element.find_elements_by_xpath(xpath)

    def find_elements_by_css_selector(self, css):
        """ Convenience routine. """
        return self.element.find_elements_by_css_selector(css)

    def find_elements_by_tag_name(self, tag):
        """ Convenience routine. """
        return self.element.find_elements_by_tag_name(tag)

    def get_attribute(self, name):
        """ Return value for the attribute `name`. """
        return self.element.get_attribute(name)

    def value_of_css_property(self, name):
        """ Return value for the the CSS property `name`. """
        return self.element.value_of_css_property(name)


class _ButtonElement(_BaseElement):
    """ Basically something to ``click``. """

    def __init__(self, page, locator):
        super(_ButtonElement, self).__init__(page, locator)

    @property
    def value(self):
        """ The element's ``value`` attribute. """
        return self.element.get_attribute('value')

    @property
    def text(self):
        """ The element's text. """
        return self.element.text

    def click(self):
        """ 'Click' on the button. """
        element = self.element
        WebDriverWait(self._browser, TMO).until(
            lambda browser: element.is_displayed())
        time.sleep(0.1)  # Just some pacing.
        element.click()


class _CheckboxElement(_BaseElement):
    """ The `value` of this is the selection state. """

    def __init__(self, page, locator):
        super(_CheckboxElement, self).__init__(page, locator)

    @property
    def value(self):
        """ The element's selection state. """
        return self.element.is_selected()

    @value.setter
    def value(self, new_value):
        element = self.element
        if bool(new_value) != element.is_selected():
            time.sleep(1)  # Just some pacing.
            element.click()  # Toggle it.


class _GridElement(_BaseElement):
    """ A SlickGrid. """

    def __init__(self, page, locator):
        super(_GridElement, self).__init__(page, locator)

    @property
    def value(self):
        """ The element's ``value`` attribute is a :class:`Grid`. """
        return Grid(self._browser, self.element)


class _InputElement(_BaseElement):
    """ A text input field. """

    def __init__(self, page, locator):
        super(_InputElement, self).__init__(page, locator)

    @property
    def value(self):
        """ The element's text input value. """
        return self.element.get_attribute('value')

    @value.setter
    def value(self, new_value):
        element = self.element
        for retry in range(3):
            try:
                WebDriverWait(self._browser, TMO).until(
                    lambda browser: element.is_displayed())
                WebDriverWait(self._browser, TMO).until(
                    lambda browser: element.is_enabled())
                if element.get_attribute('value'):
                    element.clear()
                time.sleep(0.1)  # Just some pacing.
                element.send_keys(new_value)
                return
            except StaleElementReferenceException:
                if retry < 2:
                    logging.warning('InputElement.send_keys:'
                                    ' StaleElementReferenceException')
                    element = self.element
                else:
                    raise

    def set_values(self, *values):
        """ FIXME: doesn't work, see Selenium issue #2239
            http://code.google.com/p/selenium/issues/detail?id=2239
        """
        element = self.element
        WebDriverWait(self._browser, TMO).until(
            lambda browser: element.is_displayed())
        WebDriverWait(self._browser, TMO).until(
            lambda browser: element.is_enabled())
        if element.get_attribute('value'):
            element.clear()
        time.sleep(0.1)  # Just some pacing.
        element.send_keys(*values)


class _SelectElement(_BaseElement):
    """ The `value` of this is the selection state. """

    def __init__(self, page, locator):
        super(_SelectElement, self).__init__(page, locator)

    @property
    def value(self):
        """ The element's selected value. """
        return self.element.get_attribute('value')

    @value.setter
    def value(self, new_value):
        element = self.element
        WebDriverWait(self._browser, TMO).until(
            lambda browser: element.is_displayed())
        for option in element.find_elements_by_tag_name('option'):
            if option.text == new_value:
                option.click()


class _TextElement(_BaseElement):
    """ Just some text on the page. """

    def __init__(self, page, locator):
        super(_TextElement, self).__init__(page, locator)

    @property
    def value(self):
        """ The element's text. """
        return self.element.text


class _GenericElement(_BaseElement):

    def __init__(self, page, locator):
        super(_GenericElement, self).__init__(page, locator)

    @property
    def value(self):
        return self.element


class BaseElement(object):
    """
    Implements the Python descriptor protocol in combination with
    :class:`BasePageObject`.

    cls: :class:`_BaseElement`
        The type of element to create.

    locator: tuple
        WebDriver locator for the element.
    """

    def __init__(self, cls, locator):
        self._cls = cls
        self._locator = locator
        self._elements = {}

    def __get__(self, page, cls):
        """ Return the element's value. """
        if page is None:
            return self
        element = self.get(page)
        return element.value

    def __set__(self, page, value):
        """ Set the element's value to `value`. """
        element = self.get(page)
        element.value = value

    def get(self, page):
        """ Return element instance for `page`. """
        element = self._elements.get(page)
        if element is None:
            element = self._cls(page, self._locator)
            self._elements[page] = element
        return element


class ButtonElement(BaseElement):
    """ Basically something to ``click``. """
    def __init__(self, locator):
        super(ButtonElement, self).__init__(_ButtonElement, locator)


class CheckboxElement(BaseElement):
    """ The `value` of this is the selection state. """
    def __init__(self, locator):
        super(CheckboxElement, self).__init__(_CheckboxElement, locator)


class GridElement(BaseElement):
    """ The `value` of this is a :class:`Grid`. """
    def __init__(self, locator):
        super(GridElement, self).__init__(_GridElement, locator)


class InputElement(BaseElement):
    """ A text input field. """
    def __init__(self, locator):
        super(InputElement, self).__init__(_InputElement, locator)


class SelectElement(BaseElement):
    """ A drop-down selector. """
    def __init__(self, locator):
        super(SelectElement, self).__init__(_SelectElement, locator)


class TextElement(BaseElement):
    """ Just some text on the page. """
    def __init__(self, locator):
        super(TextElement, self).__init__(_TextElement, locator)


class GenericElement(BaseElement):
    """A Generic Element for objects not of the above types"""
    def __init__(self, locator):
        super(GenericElement, self).__init__(_GenericElement, locator)
