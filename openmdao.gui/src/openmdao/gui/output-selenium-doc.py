from selenium import webdriver
for call in dir(webdriver):
    str = "webdriver." + call + ".__doc__"
    print call + ":"
    print eval(str)
