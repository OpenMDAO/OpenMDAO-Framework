import logging
import time

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO
from elements import GenericElement


class GeometryPage(BasePageObject):
    """ Geometry Viewer window. """

    title_prefix = 'OpenMDAO:'

    # Left side.
    edges_tree = GenericElement((By.ID, 'tree_frame_edges_tree'))
    faces_tree = GenericElement((By.ID, 'tree_frame_faces_tree'))

    # Right side.
    canvas = GenericElement((By.ID, 'canvas_frame_canvas'))
    status = GenericElement((By.ID, 'canvas_frame_status'))

    def __init__(self, browser, port):
        super(GeometryPage, self).__init__(browser, port)

        self.locators = {}
        self.locators["edges"] = (By.XPATH, "//div[@id='tree_frame_edges_tree']//a")
        self.locators["faces"] = (By.XPATH, "//div[@id='tree_frame_faces_tree']//a")

    def get_edges(self):
        """ Return names in the edges tree. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'tree_frame_edges_tree'))
        # FIXME: absolute delay for tree population.
        time.sleep(2)
        edges = self.browser.find_elements(*self.locators["edges"])
        edge_names = []
        for i in range(len(edges)):
            for retry in range(10):  # This has had issues...
                try:
                    print edges[i]
                    edge_names.append(edges[i].text.strip())
                except StaleElementReferenceException:
                    logging.warning('get_edges: StaleElementReferenceException')
                else:
                    break
        return edge_names

    def get_faces(self):
        """ Return names in the edges tree. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'tree_frame_faces_tree'))
        # FIXME: absolute delay for tree population.
        time.sleep(2)
        faces = self.browser.find_elements(*self.locators["faces"])
        face_names = []
        for i in range(len(faces)):
            for retry in range(10):  # This has had issues...
                try:
                    face_names.append(faces[i].text.strip())
                except StaleElementReferenceException:
                    logging.warning('get_edges: StaleElementReferenceException')
                else:
                    break
        return face_names
