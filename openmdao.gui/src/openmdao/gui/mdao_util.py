## utility functions used by mdao.py

import os, os.path
from xml.dom.minidom import Document

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
        print key
        print value

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
