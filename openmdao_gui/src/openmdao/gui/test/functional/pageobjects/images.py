import logging

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO


class ImagesPage(BasePageObject):
    """ Image Viewer window. """

    title_prefix = 'OpenMDAO Image Viewer'

    def __init__(self, browser, port):
        super(ImagesPage, self).__init__(browser, port)

    def get_image(self):
        """ Return src of the main image. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'images'))

        image = self.browser.find_elements_by_css_selector('div.galleria-stage img')[0]
        return image.get_attribute('src').strip()

    def get_thumbnails(self):
        """ Return src of thumbnail images. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'images'))

        images = self.browser.find_elements_by_css_selector('div.galleria-thumbnails img')
        image_srcs = []
        for i in range(len(images)):
            for retry in range(10):
                try:
                    image_srcs.append(images[i].get_attribute('src').strip())
                except StaleElementReferenceException:
                    logging.warning('get_thumbnails: StaleElementReferenceException')
                else:
                    break
        return image_srcs
