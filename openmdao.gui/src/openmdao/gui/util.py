"""  utility functions used by openmdao gui
"""

import sys
import os
import os.path
import webbrowser
import json
from xml.etree.ElementTree import Element, SubElement, tostring


def ensure_dir(d):
    ''' create directory if it doesn't exist
    '''
    if not os.path.isdir(d):
        os.makedirs(d)


def print_list(list):
    ''' print the contents of a list
    '''
    for item in list:
        print item


def print_dict(dict):
    ''' print the contents of a dictionary
    '''
    for item in dict.items():
        key, value = item
        print str(key)+' = '+str(value)


def print_json(data):
    ''' pretty print json data
    '''
    print json.dumps(json.loads(str(data)), indent=2)


def makenode(doc, path):
    ''' Return a document node contains a directory tree for the path.
        modified version of:
        http://code.activestate.com/recipes/305313-xml-directory-tree/
    '''
    node = doc.createElement('dir')
    node.setAttribute('name', path)
    for f in os.listdir(path):
        fullname = os.path.join(path, f)
        if os.path.isdir(fullname):
            elem = makenode(doc, fullname)
        else:
            elem = doc.createElement('file')
            elem.setAttribute('name', f)
        node.appendChild(elem)
    return node


def filedict(path, key='pathname', root=''):
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
            dict[k] = filedict(pathname, key, root)
        else:
            dict[k] = os.path.getsize(pathname)
    return dict


def packagedict(types):
    ''' create a nested dict for a package structure
    '''
    dict={}
    for typ,meta in types:
        parent = dict
        nodes = typ.split('.')
        name = nodes[len(nodes)-1]
        for node in nodes:
            if node==name:
                parent[node] = meta.copy()
                parent[node].update({'path': typ})
            else:
                if not node in parent:
                    parent[node] = {}
            parent = parent[node]
    return dict


def packageXML(types):
    ''' create an XML representation of a package structure
    '''
    xml = '<?xml version=\"1.0\"?>\n'
    xml = xml + '<response>\n'
    typeTree = Element("Types")
    # get the installed types
    for t in types:
        path = t[0].split('.')
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
                    pkgElem = SubElement(parent, "Package")
                    pkgElem.set("name", node)
                    parent = pkgElem
                else:
                    parent = existingElem
            else:
                # it's the class name, add it under current package
                typeElem = SubElement(parent, "Type")
                typeElem.set("name", node)
                typeElem.set("path", t[0])
    # get the "working" types
    pkgElem = SubElement(typeTree, "Package")
    pkgElem.set("name", "working")
    xml = xml + tostring(typeTree)
    xml = xml + '</response>\n'
    return xml


def launch_browser(port, preferred_browser=None):
    ''' launch web browser on specified port
        try to use preferred browser if specified, fall back to default
        (chrome will launch in "app mode")
    '''
    url = 'http://localhost:'+str(port)
    print 'Opening URL in browser: '+url+' (pid='+str(os.getpid())+')'

    # webbrowser doesn't know about chrome, so try to find it
    if preferred_browser and preferred_browser.lower() == 'chrome':
        if sys.platform == 'win32':
            # Windows7
            USERPROFILE = os.getenv("USERPROFILE")
            if USERPROFILE:
                CHROMEPATH = USERPROFILE+'\AppData\Local\Google\Chrome\Application\chrome.exe'
                if os.path.isfile(CHROMEPATH):
                    preferred_browser = CHROMEPATH.replace('\\', '\\\\')+' --app=%s'
        elif sys.platform == 'darwin':
            # Mac OSX
            CHROMEPATH = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
            if os.path.isfile(CHROMEPATH):
                CHROMEPATH = CHROMEPATH.replace('Google Chrome', 'Google\ Chrome')
                preferred_browser = 'open -a '+CHROMEPATH+' %s'
        elif sys.platform == 'linux2':
            # Linux
            CHROMEPATH = '/usr/bin/chromium-browser'
            if os.path.isfile(CHROMEPATH):
                preferred_browser = CHROMEPATH+' --app=%s &'

    # try to get preferred browser, fall back to default
    if preferred_browser:
        try:
            browser = webbrowser.get(preferred_browser)
        except:
            print "Couldn't get preferred browser ("+preferred_browser+"), using default..."
            browser = webbrowser.get()
    else:
        browser = webbrowser.get()

    # open new browser window (may open in a tab depending on user preferences, etc.)
    if browser:
        browser.open(url, 1, True)
        print "Opened in", browser.name
    else:
        print "Couldn't launch browser: "+str(browser)
