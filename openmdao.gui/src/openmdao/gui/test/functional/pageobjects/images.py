import logging

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO
from elements import GenericElement

class ImagesPage(BasePageObject):
    """ Geometry Viewer window. """

    title_prefix = 'OpenMDAO Image Viewer'

    # Left side.
    images_tree = GenericElement((By.ID, 'tree_frame_images_tree'))
    faces_tree = GenericElement((By.ID, 'tree_frame_faces_tree'))

    # Right side.
    canvas = GenericElement((By.ID, 'canvas_frame_canvas'))
    status = GenericElement((By.ID, 'canvas_frame_status'))

    def __init__(self, browser, port):
        super(ImagesPage, self).__init__(browser, port)

    def get_image_names(self):
        """ Return names in the images tree. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'images'))

        images = self.browser.find_elements_by_css_selector('img')
        image_names = []
        for i in range(len(images)):
            for retry in range(10):
                try:
                    image_names.append(images[i].get_attribute('src').strip())
                except StaleElementReferenceException:
                    logging.warning('get_image_names: StaleElementReferenceException')
                else:
                    break
        return image_names
