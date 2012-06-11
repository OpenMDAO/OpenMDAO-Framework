import time

from selenium.webdriver import ActionChains
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait


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
            self._rows = [GridRow(self._browser, row) for row in rows]
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

    def __init__(self, browser, root):
        self._browser = browser
        self._root = root
        self._cells = None

    @property
    def cells(self):
        if self._cells is None:
            cells = self._root.find_elements(By.CLASS_NAME, 'slick-cell')
            self._cells = [GridCell(self._browser, cell) for cell in cells]
        return self._cells

    @property
    def value(self):
        return [cell.value for cell in self.cells]

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

