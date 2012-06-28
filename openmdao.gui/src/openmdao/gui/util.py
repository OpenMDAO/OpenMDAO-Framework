"""  utility functions used by openmdao gui
"""

import sys
import os
import os.path
import webbrowser
import json

from distutils.spawn import find_executable


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
        print str(key) + ' = ' + str(value)


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
    dct = {}
    for filename in os.listdir(path):
        pathname = os.path.join(path, filename)
        k = locals()[key]
        l = len(root)
        if key == 'pathname' and l > 0:
            k = k[l:]
        if os.path.isdir(pathname):
            dct[k] = filedict(pathname, key, root)
        else:
            dct[k] = os.path.getsize(pathname)
    return dct


def unique_shortnames(names):
    """Return a dict containing full name vs. short name where short name
    is still unique within the given list.  Each entry in the initial list
    of dotted names is assumed to be unique.
    """
    looking = set(names)
    dct = dict([(n, n.split('.')) for n in names])
    level = 1
    while looking:
        shorts = dict([(n, '.'.join(dct[n][len(dct[n]) - level:len(dct[n])])) for n in looking])
        shortcounts = dict([(s, 0) for n, s in shorts.items()])
        for n, shrt in shorts.items():
            shortcounts[shrt] += 1
        for n, shrt in shorts.items():
            if shortcounts[shrt] == 1:
                looking.remove(n)
                dct[n] = shorts[n]
        level += 1
    return dct


def packagedict(types):
    ''' create a nested dict for a package structure
    '''
    dct = {}
    namedict = unique_shortnames([t[0] for t in types])

    for typ, meta in types:
        m = meta.copy()
        m['modpath'] = typ
        dct[namedict[typ]] = m

    return dct


def get_executable_path(executable_names):
    '''Look for an executable given a list of the possible names
    '''
    path = None
    for name in executable_names:
        path = find_executable(name)
        if path:
            break
    return path


def launch_browser(port, preferred_browser=None):
    ''' launch web browser on specified port
        try to use preferred browser if specified, fall back to default
        (chrome will launch in "app mode")
    '''
    url = 'http://localhost:' + str(port)
    print 'Opening URL in browser: ' + url + ' (pid=' + str(os.getpid()) + ')'

    # webbrowser doesn't know about chrome, so try to find it
    if preferred_browser and preferred_browser.lower() == 'chrome':
        if sys.platform == 'win32':
            # Windows7
            USERPROFILE = os.getenv("USERPROFILE")
            if USERPROFILE:
                CHROMEPATH = USERPROFILE + '\AppData\Local\Google\Chrome\Application\chrome.exe'
                if os.path.isfile(CHROMEPATH):
                    preferred_browser = CHROMEPATH.replace('\\', '\\\\') + ' --app=%s'
        elif sys.platform == 'darwin':
            # Mac OSX
            CHROMEPATH = '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome'
            if os.path.isfile(CHROMEPATH):
                CHROMEPATH = CHROMEPATH.replace('Google Chrome', 'Google\ Chrome')
                preferred_browser = 'open -a ' + CHROMEPATH + ' %s'
        elif sys.platform == 'linux2':
            # Linux
            CHROMEPATH = get_executable_path(["chromium-browser", "google-chrome", "chrome"])
            if CHROMEPATH and os.path.isfile(CHROMEPATH):
                preferred_browser = CHROMEPATH + ' --app=%s &'

    # try to get preferred browser, fall back to default
    if preferred_browser:
        try:
            browser = webbrowser.get(preferred_browser)
        except:
            print "Couldn't get preferred browser (" + preferred_browser + "), using default..."
            browser = webbrowser.get()
    else:
        browser = webbrowser.get()

    # open new browser window (may open in a tab depending on user preferences, etc.)
    if browser:
        browser.open(url, 1, True)
        print "Opened in", browser.name
    else:
        print "Couldn't launch browser: " + str(browser)
