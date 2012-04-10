#!/usr/bin/env python

from pyvirtualdisplay import Display
from selenium import webdriver

#display = Display(visible=0, size=(800, 600))
#display.start()

# now Firefox will run in a virtual display. 
# you will not see the browser.
browser = webdriver.Firefox()
browser.get('http://www.google.com')
print browser.title
browser.quit()

#display.stop()
