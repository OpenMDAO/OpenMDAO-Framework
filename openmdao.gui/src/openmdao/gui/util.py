## utility functions used by openmdao gui

import os, os.path
import webbrowser
from xml.dom.minidom import Document

def singleton(cls):
    ''' a decorator to make a class a singleton
    '''
    instances = {}
    def getinstance():
        if cls not in instances:
            instances[cls] = cls()
        return instances[cls]
    return getinstance

def ensure_dir(d):
    ''' create directory if it doesn't exist
    '''
    if not os.path.isdir(d):
        os.makedirs(d)

def print_list (list):
    ''' print the contents of a list
    '''
    for item in list:
        print item

def print_dict (dict):
    ''' print the contents of a dictionary
    '''
    for item in dict.items():
        key, value = item
        print str(key)+' = '+str(value)

def makenode(doc,path):
    ''' modified version of:
        http://code.activestate.com/recipes/305313-xml-directory-tree/        
    '''
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

def filedict(path,key='pathname',root=''):
    ''' create a nested dictionary for a file structure
        the key may be one of:
        
            'filename'    the name of the file
            'pathname'    the full pathname of the file (default)
    '''
    dict = {}
    for filename in os.listdir(path):
        pathname = os.path.join(path, filename)
        k = locals()[key]
        l = len(root)
        if key=='pathname' and l > 0:
            k = k[l:]
        if os.path.isdir(pathname):
            dict[k] = filedict(pathname,key,root)
        else:
            dict[k] = os.path.getsize(pathname)
    return dict

def packagedict(types):
    ''' create a nested list for a package structure
    '''
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

def launch_browser(port,preferred_browser=None):
    ''' launch web browser on specified port
        try to use preferred browser if specified, fall back to default 
        (chrome will launch in "app mode" on Windows 7)
    '''
    url = 'http://localhost:'+str(port)    
    print 'Opening URL in browser: '+url+' (pid='+str(os.getpid())+')'
    
    # webbrowser doesn't know about chrome, so try to find it (this is for win7)
    if preferred_browser and preferred_browser.lower() == 'chrome':
        USERPROFILE = os.getenv("USERPROFILE")
        if USERPROFILE:
            CHROMEPATH = USERPROFILE+'\AppData\Local\Google\Chrome\Application\chrome.exe'
       	    if os.path.isfile(CHROMEPATH):
                preferred_browser = CHROMEPATH.replace('\\','\\\\')+' --app=%s'
    
    # try to get preferred browser, fall back to default
    if preferred_browser:
        try:
            browser = webbrowser.get(preferred_browser);
        except:
            print "Couldn't get preferred browser ("+preferred_browser+"), using default..."
            browser = webbrowser.get()
    else:
        browser = webbrowser.get()
    
    # open new browser window (may open in a tab depending on user preferences, etc.)
    if browser:
        browser.open(url,1,True)
        print "Opened in",browser.name
    else:
        print "Couldn't launch browser: "+str(browser)

# ref: http://g-off.net/software/a-python-repeatable-threadingtimer-class
#
# Copyright (c) 2009 Geoffrey Foster
# 
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights to use,
# copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following
# conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.
 
from threading import Event, Thread
 
class RepeatTimer(Thread):
    def __init__(self, interval, function, iterations=0, args=[], kwargs={}):
        Thread.__init__(self)
        self.interval = interval
        self.function = function
        self.iterations = iterations
        self.args = args
        self.kwargs = kwargs
        self.finished = Event()
 
    def run(self):
        count = 0
        while not self.finished.is_set() and (self.iterations <= 0 or count < self.iterations):
            self.finished.wait(self.interval)
            if not self.finished.is_set():
                self.function(*self.args, **self.kwargs)
                count += 1
 
    def cancel(self):
        self.finished.set()
