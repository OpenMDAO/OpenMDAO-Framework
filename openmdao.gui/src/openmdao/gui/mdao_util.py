## utility functions used by mdao.py

import os, os.path
import webbrowser
from xml.dom.minidom import Document

# a decorator to make a class a singleton
def singleton(cls):
    instances = {}
    def getinstance():
        if cls not in instances:
            instances[cls] = cls()
        return instances[cls]
    return getinstance

# make sure a directory exists
def ensure_dir(d):
    if not os.path.isdir(d):
        os.makedirs(d)

# print the contents of a list
def print_list (list):
    for item in list:
        print item

# print the contents of a dictionary
def print_dict (dict):
    for item in dict.items():
        key, value = item
        print str(key)+' = '+str(value)

# modified version of:
# http://code.activestate.com/recipes/305313-xml-directory-tree/        
def makenode(doc,path):
    "Return a document node contains a directory tree for the path."
    node = doc.createElement('dir')
    node.setAttribute('name', path)
    for f in os.listdir(path):
        fullname = os.path.join(path, f)
        if os.path.isdir(fullname):
            elem = makenode(doc,fullname)
        else:
            elem = doc.createElement('file')
            elem.setAttribute('name', f)
        node.appendChild(elem)
    return node

# create a nested dictionary for a file structure with file names as keys   
def filedict(path):
    "Return a directory tree for the path."
    dict = {}
    for f in os.listdir(path):
        fullname = os.path.join(path, f)
        if os.path.isdir(fullname):
            dict[f] = filedict(fullname)
        else:
            dict[f] = os.path.getsize(fullname)
    return dict

# create a nested dictionary for a file structure with pathnames as keys   
def filepathdict(path):
    "Return a directory tree for the path."
    dict = {}
    for f in os.listdir(path):
        fullname = os.path.join(path, f)
        if os.path.isdir(fullname):
            dict[fullname] = filepathdict(fullname)
        else:
            dict[fullname] = os.path.getsize(fullname)
    return dict

# create a nested list for a package structure
def packagedict(types):
    dict={}
    for t in types:
        parent = dict
        nodes = t[0].split('.');
        name = nodes[len(nodes)-1]
        for node in nodes:
            if node==name:
                parent[node] = { 'path':t[0], 'version':t[1] }
            else:
                if not node in parent:
                    parent[node] = {}
            parent = parent[node]
    return dict

from xml.etree.ElementTree import Element, SubElement, tostring
from xml.dom.minidom import Document
def packageXML(types):
        xml = '<?xml version=\"1.0\"?>\n'
        xml = xml + '<response>\n'
        typeTree = Element("Types")
        # get the installed types
        for t in types:
            path = t[0].split('.');
            last = path[len(path)-1]
            parent = typeTree
            for node in path:
                if not node==last:
                    # it's a package name, see if we have it already
                    existingElem = None
                    packages = parent.findall('Package')
                    for p in packages:
                        if p.get("name") == node:
                            existingElem = p
                    # set the parent to this package
                    if existingElem is None:
                        pkgElem = SubElement(parent,"Package")
                        pkgElem.set("name",node)
                        parent = pkgElem
                    else:
                        parent = existingElem
                else:
                    # it's the class name, add it under current package
                    typeElem = SubElement(parent,"Type")
                    typeElem.set("name",node)
                    typeElem.set("path",t[0])
        # get the "working" types
        pkgElem = SubElement(typeTree,"Package")
        pkgElem.set("name","working")
        xml = xml + tostring(typeTree)
        xml = xml + '</response>\n'
        return xml    
    
# find an unused port    
# ref: http://code.activestate.com/recipes/531822-pick-unused-port/
# note: use the port before it's taken by some other process!
import socket
def PickUnusedPort():
  s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  s.bind(('localhost', 0))
  addr, port = s.getsockname()
  s.close()
  return port

#
# launch web browser on specified port
#
def launch_browser(port,preferred_browser=None):
    ''' try to use preferred browser if specified, fall back to default 
    '''
    url = 'http://localhost:'+str(port)    
    print 'Opening URL in browser: '+url+' (pid='+str(os.getpid())+')'
    
    # webbrowser doesn't know about chrome, so try to find it (this is for win7)
    if preferred_browser and preferred_browser.lower() == 'chrome':
        print 'Trying to find Google Chrome...'
        USERPROFILE = os.getenv("USERPROFILE").replace('\\','/')
        CHROMEPATH = USERPROFILE+'/AppData/Local/Google/Chrome/Application/chrome.exe'
        if os.path.isfile(CHROMEPATH):
            preferred_browser = CHROMEPATH+' %s'
    
    # try to get preferred browser, fall back to default
    if preferred_browser:
        try:
            browser = webbrowser.get(preferred_browser);
        except:
            print "Couldn't launch preferred browser ("+preferred_browser+"), using default..."
            browser = webbrowser.get()
    else:
        browser = webbrowser.get()
    
    # open new browser window (may open in a tab depending on user preferences, etc.)
    if browser:
            browser.open(url,1,True)
    else:
        print "Couldn't launch browser: "+str(browser)
   