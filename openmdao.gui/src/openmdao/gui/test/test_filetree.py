import os, os.path
from xml.dom.minidom import Document
import jsonpickle
import json

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
    
#
# main.  test 2 methods of representing a file tree (XML & JSON).
#
if __name__ == "__main__":

    testpath = os.path.abspath('..')

    doc = Document()    
    doc.appendChild(makenode(doc,testpath))
    print doc.toprettyxml()
    
    print ''    
    print ''
    
    dict = filedict(testpath)
    jsondict = jsonpickle.encode(dict)    
    print 'json = ',json.dumps(jsondict,sort_keys=False,indent=4)

    

