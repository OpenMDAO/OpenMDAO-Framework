import logging

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait

from selenium.common.exceptions import StaleElementReferenceException

from basepageobject import BasePageObject, TMO
from elements import GenericElement, TextElement, CheckboxElement


class EdgeNode(BasePageObject):
    """ An edge node in the Edges tree. """

    name = TextElement((By.XPATH, './a'))
    viz  = CheckboxElement((By.XPATH, './div/span/input[@value=1]'))
    grd  = CheckboxElement((By.XPATH, './div/span/input[@value=16]'))
    ori  = CheckboxElement((By.XPATH, './div/span/input[@value=8]'))

    def __init__(self, browser, port, root):
        super(EdgeNode, self).__init__(browser, port, root)


class FaceNode(BasePageObject):
    """ A face node in the Faces tree. """

    name = TextElement((By.XPATH, './a'))
    viz  = CheckboxElement((By.XPATH, './div/span/input[@value=1]'))
    grd  = CheckboxElement((By.XPATH, './div/span/input[@value=32]'))
    trn  = CheckboxElement((By.XPATH, './div/span/input[@value=2]'))

    def __init__(self, browser, port, root):
        super(FaceNode, self).__init__(browser, port, root)


class GeometryPage(BasePageObject):
    """ Geometry Viewer window. """

    title_prefix = 'OpenMDAO Geometry Viewer:'

    # Left side.
    edges_tree = GenericElement((By.ID, 'tree_frame_edges_tree'))
    faces_tree = GenericElement((By.ID, 'tree_frame_faces_tree'))

    # Right side.
    canvas = GenericElement((By.ID, 'canvas_frame_canvas'))
    status = GenericElement((By.ID, 'canvas_frame_status'))

    def __init__(self, browser, port):
        super(GeometryPage, self).__init__(browser, port)

    def has_canvas(self):
        canvas = self.browser.find_elements_by_css_selector('canvas')
        if len(canvas) > 0:
            return True
        else:
            return False

    def expand_edges(self):
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'tree_frame_edges_tree'))

        expander = self.edges_tree.find_element_by_css_selector('ins')
        expander.click()

    def expand_faces(self):
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'tree_frame_faces_tree'))

        expander = self.faces_tree.find_element_by_css_selector('ins')
        expander.click()

    def get_edge_names(self):
        """ Return names in the edges tree. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'tree_frame_edges_tree'))

        edges = self.edges_tree.find_elements_by_css_selector('ul > li > ul > li')
        edge_names = []
        for i in range(len(edges)):
            for retry in range(10):
                try:
                    edge_names.append(edges[i].get_attribute('nom').strip())
                except StaleElementReferenceException:
                    logging.warning('get_edge_names: StaleElementReferenceException')
                else:
                    break
        return edge_names

    def get_face_names(self):
        """ Return names in the faces tree. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'tree_frame_faces_tree'))

        faces = self.faces_tree.find_elements_by_css_selector('ul > li > ul > li')
        face_names = []
        for i in range(len(faces)):
            for retry in range(10):
                try:
                    face_names.append(faces[i].get_attribute('nom').strip())
                except StaleElementReferenceException:
                    logging.warning('get_face_names: StaleElementReferenceException')
                else:
                    break
        return face_names

    def get_edge(self, name):
        """ Return the edge element with the given name in the edges tree. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'tree_frame_edges_tree'))

        edges = self.edges_tree.find_elements_by_css_selector('li')
        for i in range(len(edges)):
            for retry in range(10):
                try:
                    edge_name = edges[i].get_attribute('nom').strip()
                    if edge_name == name:
                        edge = EdgeNode(self.browser, self.port, edges[i])
                        return edge
                except StaleElementReferenceException:
                    logging.warning('get_edge: StaleElementReferenceException')
                else:
                    break

    def get_face(self, name):
        """ Return the face element with the given name in the faces tree. """
        WebDriverWait(self.browser, TMO).until(
            lambda browser: browser.find_element(By.ID, 'tree_frame_faces_tree'))

        faces = self.faces_tree.find_elements_by_css_selector('li')
        for i in range(len(faces)):
            for retry in range(10):
                try:
                    face_name = faces[i].get_attribute('nom').strip()
                    if face_name == name:
                        face = FaceNode(self.browser, self.port, faces[i])
                        return face
                except StaleElementReferenceException:
                    logging.warning('get_face: StaleElementReferenceException')
                else:
                    break
