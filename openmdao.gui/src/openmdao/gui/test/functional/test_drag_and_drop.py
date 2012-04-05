from selenium import webdriver
from selenium.webdriver.common.action_chains import ActionChains

class TestDragAndDrop:
     def setup(self):
          self.firefox = webdriver.Firefox()

     def teardown(self):
          self.firefox.quit()

     def test_that_we_can_drag_and_drop(self):
          self.firefox.get('http://www.theautomatedtester.co.uk/demo2.html')
          draggable = self.firefox.find_element_by_class_name("draggable")
          droppable = self.firefox.find_element_by_id("droppable")
          dragdrop = ActionChains(self.firefox)\
                         .drag_and_drop(draggable, droppable)
                         
          #Now we know what we want to happen, let's perform the actions
          dragdrop.perform()
