// wv-tree.js implements a "tree widget"
// written by John Dannenhoffer

function TreeAddNode(iparent, name, gprim, prop1, prop2, prop3) {
    // alert("in TreeAddNode(" + iparent + "," + name + "," + gprim + "," + prop1 + "," + prop2 + "," + prop3 + ")");

    // validate the input
    if (iparent < 0 || iparent >= this.name.length) {
        alert("iparent=" + iparent + " is out of range");
        return;
    }

    // find the next node index
    var inode = this.name.length;

    // store this node's values
    this.name[  inode] = name;
    this.gprim[ inode] = gprim;
    this.parent[inode] = iparent;
    this.child[ inode] = -1;
    this.next[  inode] = -1;
    this.opened[inode] =  0;

    // either use given property or inherit from parent
    if (prop1 == "on" || prop1 == "off") {
        this.prop1[inode] = prop1;
    } else {
        this.prop1[inode] = this.prop1[iparent];
    }

    if (prop2 == "on" || prop2 == "off") {
        this.prop2[inode] = prop2;
    } else {
        this.prop2[inode] = this.prop2[iparent];
    }

    if (prop3 == "on" || prop3 == "off") {
        this.prop3[inode] = prop3;
    } else {
        this.prop3[inode] = this.prop3[iparent];
    }

    // if the parent does not have a child, link this
    //    new node to the parent
    if (this.child[iparent] < 0) {
        this.child[iparent] = inode;

    // otherwise link this node to the last parent's child
    } else {
        var jnode = this.child[iparent];
        while (this.next[jnode] >= 0) {
            jnode = this.next[jnode];
        }

        this.next[jnode] = inode;
    }
}

function TreeExpand(inode) {
    // alert("in TreeExpand(" + inode + ")");

    // validate inputs
    if (inode < 0 || inode >= this.opened.length) {
        alert("inode=" + inode + " is out of range");
        return;
    }

    // expand inode
    this.opened[inode] = 1;

    // update the display
    this.update();
}

function TreeContract(inode) {
    // alert("in TreeContract(" + inode + ")");

    // validate inputs
    if (inode < 0 || inode >= this.opened.length) {
        alert("inode=" + inode + " is out of range");
        return;
    }

    // contract inode
    this.opened[inode] = 0;

    // contract all descendents of inode
    for (var jnode = 1; jnode < this.parent.length; jnode++) {
        var iparent = this.parent[jnode];
        while (iparent > 0) {
            if (iparent == inode) {
                this.opened[jnode] = 0;
                break;
            }

            iparent = this.parent[iparent];
        }
    }

    // update the display
    this.update();
}

function TreeProp(inode, iprop, onoff) {
    // alert("in TreeProp(" + inode + "," + iprop + "," + onoff + ")");

    // validate inputs
    if (inode < 0 || inode >= this.opened.length) {
        alert("inode=" + inode + " is out of range");
        return;
    } else if (onoff != "on" && onoff != "off") {
        alert("onoff=" + onoff + " is not 'on' or 'off'");
        return;
    }

    // set the property for inode
    if (iprop == 1) {
        this.prop1[inode] = onoff;
    } else if (iprop == 2) {
        this.prop2[inode] = onoff;
    } else if (iprop == 3) {
        this.prop3[inode] = onoff;
    } else {
        alert("iprop=" + iprop + " is not 1, 2, or 3");
        return;
    }

    // set property of all descendents of inode
    for (var jnode = 1; jnode < this.parent.length; jnode++) {
        var iparent = this.parent[jnode];
        while (iparent > 0) {
            if (iparent == inode) {
                if        (iprop == 1) {
                    this.prop1[jnode] = onoff;
                } else if (iprop == 2) {
                    this.prop2[jnode] = onoff;
                } else if (iprop == 3) {
                    this.prop3[jnode] = onoff;
                }
                break;
            }

            iparent = this.parent[iparent];
        }
    }

    this.update();
}

function TreeBuild() {
    // alert("in TreeBuild()");

    var doc = this.document;

    // build a table
    var newTable = doc.createElement("table");
    newTable.setAttribute("id", this.treeId);
    doc.getElementById("leftframe").appendChild(newTable);

    // traverse the nodes using depth-first search
    var inode = 1;
    while (inode > 0) {

        // table row "node"+inode
        var newTR = doc.createElement("TR");
        newTR.setAttribute("id", "node" + inode);
        newTable.appendChild(newTR);

        // table data "node"+inode+"a"
        var newTDa = doc.createElement("TD");
        newTDa.setAttribute("id", "node" + inode + "a");
        newTDa.className = "fakelink";
        newTR.appendChild(newTDa);

        var newTexta = doc.createTextNode("");
        newTDa.appendChild(newTexta);

        // table data "node"+inode+"b"
        var newTDb = doc.createElement("TD");
        newTR.appendChild(newTDb);

        var newTextb = doc.createTextNode(this.name[inode]);
        newTDb.appendChild(newTextb);

        // table data "node"+inode+"c"
        var newTDc = doc.createElement("TD");
        newTDc.setAttribute("id", "node" + inode + "c");
        newTDc.className = "facelinkon";
        newTR.appendChild(newTDc);

        var newTextc = doc.createTextNode(this.prop1name);
        newTDc.appendChild(newTextc);

        // table data "node:+inode+"d"
        var newTDd = doc.createElement("TD");
        newTDd.setAttribute("id", "node" + inode + "d");
        newTDd.className = "facelinkon";
        newTR.appendChild(newTDd);

        var newTextd = doc.createTextNode(this.prop2name);
        newTDd.appendChild(newTextd);

        // table data "node:+inode+"e"
        var newTDd = doc.createElement("TD");
        newTDd.setAttribute("id", "node" + inode + "e");
        newTDd.className = "facelinkon";
        newTR.appendChild(newTDd);

        var newTextd = doc.createTextNode(this.prop3name);
        newTDd.appendChild(newTextd);

        // go to next row
        if        (this.child[inode] >= 0) {
            inode = this.child[inode];
        } else if (this.next[inode] >= 0) {
            inode = this.next[inode];
        } else {
            while (inode > 0) {
                inode = this.parent[inode];
                if (this.parent[inode] == 0) {
                    var newTR = doc.createElement("TR");
                    newTR.setAttribute("height", "10px");
                    newTable.appendChild(newTR);
                }
                if (this.next[inode] >= 0) {
                    inode = this.next[inode];
                    break;
                }
            }
        }
    }

    this.update();
}

function TreeUpdate() {
    // alert("in TreeUpdate()");

    var doc = this.document;

    // traverse the nodes using depth-first search
    for (var inode = 1; inode < this.opened.length; inode++) {
        var element = doc.getElementById("node" + inode);

        // unhide the row
        element.style.display = null;

        // hide the row if one of its parents has .opened=0
        var jnode = this.parent[inode];
        while (jnode != 0) {
            if (this.opened[jnode] == 0) {
                element.style.display = "none";
                break;
            }

            jnode = this.parent[jnode];
        }

        // if the current node has children, set up appropriate event handler
        if (this.child[inode] > 0) {
            if (this.opened[inode] == 0) {
                var foo  = doc.getElementById("node" + inode + "a");
                var This = this;

                foo.firstChild.nodeValue = "+";
                foo.onclick = function () {
                    var thisNode = this.id.substring(4,999);
                    thisNode     = thisNode.substring(0,thisNode.length-1);
                    This.expand(thisNode);
                };

            } else {
                var foo  = doc.getElementById("node" + inode + "a");
                var This = this;

                foo.firstChild.nodeValue = "-";
                foo.onclick = function () {
                    var thisNode = this.id.substring(4,999);
                    thisNode     = thisNode.substring(0,thisNode.length-1);
                    This.contract(thisNode);
                };
            }
        }

        // set the class of the properties
        if (this.prop3[inode] == "off") {
            var foo = doc.getElementById("node" + inode + "e");

            foo.setAttribute("class", "fakelinkoff");
            foo.onclick = function () {
                var thisNode = this.id.substring(4,999);
                thisNode     = thisNode.substring(0,thisNode.length-1);
                This.prop(thisNode, 3, "on");
            };

            if (this.gprim[inode] != "*") {
                g.sceneGraph[this.gprim[inode]].attrs &= ~g.plotAttrs.TRANSPARENT;
                g.sceneUpd = 1;
            }
        } else {
            var foo = doc.getElementById("node" + inode + "e");

            foo.setAttribute("class", "fakelinkon");
            foo.onclick = function () {
                var thisNode = this.id.substring(4,999);
                thisNode     = thisNode.substring(0,thisNode.length-1);
                This.prop(thisNode, 3, "off");
            };

            if (this.gprim[inode] != "*") {
                g.sceneGraph[this.gprim[inode]].attrs |= g.plotAttrs.TRANSPARENT;
                g.sceneUpd = 1;
            }
        }

        if (this.prop2[inode] == "off") {
            var foo = doc.getElementById("node" + inode + "d");

            foo.setAttribute("class", "fakelinkoff");
            foo.onclick = function () {
                var thisNode = this.id.substring(4,999);
                thisNode     = thisNode.substring(0,thisNode.length-1);
                This.prop(thisNode, 2, "on");
            };

            if (this.gprim[inode] != "*") {
                g.sceneGraph[this.gprim[inode]].attrs &= ~g.plotAttrs.LINES;
                g.sceneUpd = 1;
            }
        } else {
            var foo = doc.getElementById("node" + inode + "d");

            foo.setAttribute("class", "fakelinkon");
            foo.onclick = function () {
                var thisNode = this.id.substring(4,999);
                thisNode     = thisNode.substring(0,thisNode.length-1);
                This.prop(thisNode, 2, "off");
            };

            if (this.gprim[inode] != "*") {
                g.sceneGraph[this.gprim[inode]].attrs |= g.plotAttrs.LINES;
                g.sceneUpd = 1;
            }
        }

        if (this.prop1[inode] == "off") {
            var foo = doc.getElementById("node" + inode + "c");

            foo.setAttribute("class", "fakelinkoff");
            foo.onclick = function () {
                var thisNode = this.id.substring(4,999);
                thisNode     = thisNode.substring(0,thisNode.length-1);
                This.prop(thisNode, 1, "on");
            };

            if (this.gprim[inode] != "*") {
                g.sceneGraph[this.gprim[inode]].attrs &= ~g.plotAttrs.ON;
                g.sceneUpd = 1;
            }
        } else {
            var foo  = doc.getElementById("node" + inode + "c");
            var This = this;

            foo.setAttribute("class", "fakelinkon");
            foo.onclick = function () {
                var thisNode = this.id.substring(4,999);
                thisNode     = thisNode.substring(0,thisNode.length-1);
                This.prop(thisNode, 1, "off");
            };

            if (this.gprim[inode] != "*") {
                g.sceneGraph[this.gprim[inode]].attrs |= g.plotAttrs.ON;
                g.sceneUpd = 1;
            }
        }
    }
}

function Tree(doc, treeId, prop1name, prop1value, prop2name, prop2value, prop3name, prop3value) {
    // alert("in Tree(" + treeId + "," + doc + "," + prop1name + "," + prop1value + "," + prop2name + "," + prop2value + "," + prop3name + "," + prop3value + ")");

    // validate inputs
    if        (prop1value != "on" && prop1value != "off") {
        alert("prop1value=" + prop1value + " is not 'on' or 'off'");
        return;
    } else if (prop2value != "on" && prop2value != "off") {
        alert("prop2value=" + prop2value + " is not 'on' or 'off'");
        return;
    } else if (prop3value != "on" && prop3value != "off") {
        alert("prop3value=" + prop3value + " is not 'on' or 'off'");
        return;
    }

    // remember the document
    this.document = doc;
    this.treeId   = treeId;

    // remember the property names
    this.prop1name = prop1name;
    this.prop2name = prop2name;
    this.prop3name = prop3name;

    // arrays to hold the nodes
    this.name   = new Array();
    this.gprim  = new Array();
    this.parent = new Array();
    this.child  = new Array();
    this.next   = new Array();
    this.opened = new Array();

    this.prop1  = new Array();
    this.prop2  = new Array();
    this.prop3  = new Array();

    // initialize node=0 (the root)
    this.name[  0] = "**root**"
    this.gprim[ 0] = "*";
    this.parent[0] = -1;
    this.child[ 0] = -1;
    this.next[  0] = -1;
    this.prop1[ 0] = prop1value;
    this.prop2[ 0] = prop2value;
    this.prop3[ 0] = prop3value;
    this.opened[0] = +1;

    // add methods
    this.addNode  = TreeAddNode;
    this.expand   = TreeExpand;
    this.contract = TreeContract;
    this.prop     = TreeProp;
    this.build    = TreeBuild;
    this.update   = TreeUpdate;
}
