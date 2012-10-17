import logging
import time

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import rgba


class Grid(object):
    """
    Represents a SlickGrid at `root`.
    Note that after modifying a cell the grid needs to be re-fetched.
    """

    def __init__(self, browser, root):
        self._browser = browser
        self._root = root
        self._rows = None

    @property
    def rows(self):
        if self._rows is None:
            rows = self._root.find_elements(By.CLASS_NAME, 'slick-row')
            self._rows = [GridRow(self._browser, row, self._root, i)
                          for i, row in enumerate(rows)]
        return self._rows

    @property
    def value(self):
        return [row.value for row in self.rows]

    def __len__(self):
        return len(self.rows)

    def __getitem__(self, index):
        return self.rows[index]


class GridRow(object):
    """ Represents a SlickRow at `root`. """

    def __init__(self, browser, root, grid_root, row):
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
        element.send_keys(value+Keys.RETURN)

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

