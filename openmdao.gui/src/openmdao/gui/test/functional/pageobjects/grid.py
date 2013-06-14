import logging
import time

from functools import partial

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import rgba


def _row_sorter(row_element):
    '''Sort rows by the Style attribute, which for slickgrids should
    be something like: "top: 30px"'''

    style = row_element.get_attribute('style')
    style = style.replace('top: ', '')
    style = style.replace('px;', '')
    return int(style)


class Grid(object):
    """
    Represents a SlickGrid at `root`.
    Note that after modifying a cell the grid needs to be re-fetched.
    """

    def __init__(self, browser, root):
        self._browser = browser
        self._root = root
        self._rows = None
        self._headers = None

    @property
    def headers(self):
        '''Grid contains a list of GridHeader objects.'''
        if self._headers is None:
            headers = self._root.find_elements(By.CLASS_NAME, 'slick-header-column')
            self._headers = [GridHeader(self._browser, row)
                                for i, row in enumerate(headers)]
        
        return self._headers

    @property
    def rows(self):
        '''Grid contains a list of GridRow objects. Note, they are not
        always sorted correctly in the DOM, so we must sort them based
        on the value of "top" in the style attribute.'''

        if self._rows is None:
            rows = self._root.find_elements(By.CLASS_NAME, 'slick-row')
            rows_sort = sorted(rows, key=_row_sorter)
            self._rows = [GridRow(self._browser, row, self._root, i, self.headers)
                          for i, row in enumerate(rows_sort)]
        return self._rows

    @property
    def value(self):
        return [row.value for row in self.rows]

    @property
    def displayed(self):
       return self._root.is_displayed()

    def __len__(self):
        return len(self.rows)

    def __getitem__(self, index):
        for retry in range(5):
            try:
                item = self.rows[index]
            except StaleElementReferenceException:
                if retry < 4:
                    logging.warning('Grid.__getitem__: StaleElementReferenceException')
                    self._rows = None  # refetch row
                else:
                    raise
        return item

class GridHeader(object):
    """ Represents a slickHeaderColumn at `root` """
    #slick-header
    # slick-header-columns
    #  slick-header-column
    #   slick-column-name
    def __init__(self, browser, root):
        self._browser = browser
        self._root = root

    @property
    def value(self):
        return self._root.find_element(By.CLASS_NAME, "slick-column-name").text

    def click(self):
        self._root.click()

    def context_click(self):
        #Todo: action chain
        chain = ActionChains(self._browser)
        chain.context_click(self._root).perform()

class GridColumnPicker(object):
    """ Represents a slickColumnPicker at `root` """
    def __init__(self, browser, root):
        self._browser = browser
        self._root = root

    @property
    def displayed(self):
        return self._root.is_displayed()

    @property
    def options(self):
        return [GridColumnPickerOption(self._browser, option) for option in self._root.find_elements(By.XPATH, "li")]

    def get_option(self, option_name):
        for option in self.options:
            if(option.text == option_name):
                return option
        
class GridColumnPickerOption(object):
    """ """
    def __init__(self, browser, root):
        self._browser = browser
        self._root = root

    @property
    def text(self):
        return self._root.find_element(By.XPATH, "label").text

    def click(self):
        self._root.find_element(By.XPATH, "label/input").click()

class GridRow(object):
    """ Represents a SlickRow at `root`. """

    def __init__(self, browser, root, grid_root, row, headers):
        self._browser = browser
        self._root = root
        self._grid_root = grid_root
        self._row = row
        self._cells = None

    @property
    def cells(self):
        if self._cells is None:
            for retry in range(5):
                try:
                    cells = self._root.find_elements(By.CLASS_NAME, 'slick-cell')
                except StaleElementReferenceException:
                    if retry < 4:
                        logging.warning('GridRow.cells: StaleElementReferenceException')
                        self._root = self._grid_root.find_elements(By.CLASS_NAME, 'slick-row')[self._row]
                    else:
                        raise
                else:
                    break
            self._cells = [GridCell(self._browser, cell) for cell in cells]

        return self._cells

    @property
    def value(self):
        for retry in range(5):
            try:
                val = [cell.value for cell in self.cells]
            except StaleElementReferenceException:
                if retry < 4:
                    logging.warning('GridRow.value: StaleElementReferenceException')
                    self._cells = None  # refetch row
                else:
                    raise
            else:
                break
        return val

    def __len__(self):
        return len(self.cells)

    def __getitem__(self, index):
        return self.cells[index].value

    def __setitem__(self, index, value):
            self.cells[index].value = value


class GridCell(object):
    """ Represents a SlickCell at `root`. """

    def __init__(self, browser, root):
        self._browser = browser
        self._root = root

    @property
    def value(self):
        return self._root.text

    def click(self):
        chain = ActionChains(self._browser)
        chain.click(self._root).perform()

    @value.setter
    def value(self, value):
        """ Sets the value of the cell. """
        chain = ActionChains(self._browser)
        chain.double_click(self._root).perform()
        element = self._root.find_elements(By.XPATH, 'input')[0]
        WebDriverWait(self._browser, 5).until(
            lambda browser: element.is_displayed())
        WebDriverWait(self._browser, 5).until(
            lambda browser: element.is_enabled())
        if element.get_attribute('value'):
            element.clear()
        time.sleep(0.1)  # Just some pacing.
        element.send_keys(value + Keys.RETURN)

    def select(self, index):
        """ Sets a ``select`` element to `index` . """
        chain = ActionChains(self._browser)
        chain.double_click(self._root).perform()
        element = self._root.find_elements(By.XPATH, 'select')[0]
        WebDriverWait(self._browser, 5).until(
            lambda browser: element.is_displayed())
        WebDriverWait(self._browser, 5).until(
            lambda browser: element.is_enabled())
        option = element.find_elements(By.XPATH, 'option')[index]
        option.click()

    @property
    def editable(self):
        return "cell-editable" in self._root.get_attribute("class")

    @property
    def color(self):
        """ Return RGBA values for ``color`` property. """
        return rgba(self.value_of_css_property('color'))

    @property
    def background_color(self):
        """ Return RGBA values for ``background-color`` property. """
        return rgba(self.value_of_css_property('background-color'))

    def value_of_css_property(self, name):
        """ Return value for the the CSS property `name`. """
        return self._root.value_of_css_property(name)
