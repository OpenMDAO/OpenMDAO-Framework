/*

 * simple key-stroke & mouse driven UI for WV
 *
 * Notes: g.sceneUpd should be set to 1 if the scene should rerendered
 *        g.sgUpdate will be set to 1 if the sceneGraph has changed
 *                   sould be set back to 0 after UI responds
 */


//
// Event Handlers

var NO_MODIFIER = 0;
var ALT_KEY = 1;
var SHIFT_KEY = 2;
var CTRL_KEY = 4;
var WHEEL_DELTA = 120;

// work around for global variables
var brch=0;
var pmtr=0;
function getCursorXY(e) 
{
  if (!e) e = event;
  g.cursorX  = e.clientX;
  g.cursorY  = e.clientY;
  g.cursorX -= g.offLeft+1;
  g.cursorY  = g.height-g.cursorY+g.offTop+1;

  g.modifier = 0;
  if (e.shiftKey) g.modifier |= SHIFT_KEY;
  if (e.altKey)   g.modifier |= ALT_KEY;
  if (e.ctrlKey)  g.modifier |= CTRL_KEY;
}
 

function getMouseDown(e) 
{
  if (!e) e = event;
  g.startX   = e.clientX;
  g.startY   = e.clientY;
  g.startX  -= g.offLeft+1;
  g.startY   = g.height-g.startY+g.offTop+1;

  g.dragging = true;
  g.button   = e.button;
  g.modifier = 0;
  if (e.shiftKey) g.modifier |= SHIFT_KEY;
  if (e.altKey)   g.modifier |= ALT_KEY;
  if (e.ctrlKey)  g.modifier |= CTRL_KEY;
}


function getMouseUp(e) 
{
  g.dragging = false;
}


function getKeyPress(e)
{
  if (!e) e = event;
  g.keyPress = e.charCode;
}

function getMouseWheel(e)
{
  if (!e) e=event;
  g.wheelDelta = e.wheelDelta/WHEEL_DELTA;
}
//
// Required WV functions


function wvUpdateCanvas(gl)
{
  // Show the status line
  g.statusline.snapshot();
}


function wvInitUI()
{

  // set up extra storage for matrix-matrix multiplies
  g.uiMatrix = new J3DIMatrix4();
  
                                // ui cursor variables
  g.cursorX  = -1;              // current cursor position
  g.cursorY  = -1;
  g.keyPress = -1;              // last key pressed
  g.startX   = -1;              // start of dragging position
  g.startY   = -1;
  g.button   = -1;              // button pressed
  g.modifier =  0;              // modifier (shift,alt,cntl) bitflag
  g.offTop   =  0;              // offset to upper-left corner of the canvas
  g.offLeft  =  0;
  g.dragging = false;
  g.wheelDelta = 0;             // delta for mouse wheel
  
  var canvas = document.getElementById("WebViewer");
    canvas.addEventListener('mousemove',  getCursorXY,  false);
    canvas.addEventListener('mousedown',  getMouseDown, false);
    canvas.addEventListener('mouseup',    getMouseUp,   false);
    canvas.addEventListener('mousewheel', getMouseWheel, false);
  document.addEventListener('keypress',   getKeyPress,  false);

  g.statusline = new StatusLine("statusline");
  g.debug = true;
}

function wvUpdateUI()
{
    // if the tree has not been created but the scene graph (possibly) exists...
    // if the scene graph and Parameters have been updated, (re-)build the Tree
    if (g.sgUpdate == 1){ 

        if (g.sceneGraph === undefined) {
            alert("g.sceneGraph is undefined --- but we need it");
        }

        // if there was a previous Tree, keep track of whether or not
        //    the Parameters, Branches, and Display was open
        var pmtrsOpen = 0;
        var brchsOpen = 0;

        if (myTree.opened.length > 3) {
            pmtrsOpen = myTree.opened[1];
            brchsOpen = myTree.opened[2];
        }
        // clear previous Nodes from the Tree
        myTree.clear();

        // put the group headers into the Tree
        //myTree.addNode(0, "Parameters", "", addPmtr, "pmtr1_cmenu");
        //myTree.addNode(0, "Branches",   "", addBrch, "brch1_cmenu");
        myTree.addNode(0, "Display",    "", null,    ""           );

        /*// put the Parameters into the Tree
        for (var ipmtr = 0; ipmtr < pmtr.length; ipmtr++) {
            var name  = "__"+pmtr[ipmtr].name;
            var type  =      pmtr[ipmtr].type;
            var nrow  =      pmtr[ipmtr].nrow;
            var ncol  =      pmtr[ipmtr].ncol;
            var value =      pmtr[ipmtr].value[0];

            if (nrow > 1 || ncol > 1) {
                value = "["+nrow+"x"+ncol+"]";
            }

            if (type == 500) {       // OCSM_EXTERNAL
                myTree.addNode(1, name, "", editPmtr, "pmtr2_cmenu", ""+value, "", "");
            } else {
                myTree.addNode(1, name, "", null,     "",            ""+value, "", "");
            }
        }*/

        //g.pmtrStat = -2;

        // open the Parameters (if they were open before the Tree was rebuilt)
        //if (pmtrsOpen == 1) {
        //    myTree.opened[1] = 1;
        //}

        // put the Branches into the Tree
        /*for (var ibrch = 0; ibrch < brch.length; ibrch++) {
            var name  = "__"+brch[ibrch].name;
            var type  =      brch[ibrch].type;
            var actv;
            if        (brch[ibrch].actv == 301) {
                actv = "suppressed";
            } else if (brch[ibrch].actv == 302) {
                actv = "inactive";
            } else if (brch[ibrch].actv == 303) {
                actv = "deferred";
            } else {
                actv = "";
            }

            myTree.addNode(2, name, "", editBrch, "brch2_cmenu", type, "", "", actv, "", "");

            // add the Branch's attributes to the Tree
            var inode = myTree.name.length - 1;
            for (var iattr = 0; iattr < brch[ibrch].attrs.length; iattr++) {
                var aname  = brch[ibrch].attrs[iattr][0];
                var avalue = brch[ibrch].attrs[iattr][1];
                myTree.addNode(inode, "____"+aname, "", editAttr, "attr2_cmenu", avalue, "", "");
            }
        }*/

        //g.brchStat = -2;

        // open the Branches (if they were open before the Tree was rebuilt)
        //if (brchsOpen == 1) {
         //   myTree.opened[2] = 1;
        //}

        // put the Display attributes into the Tree
        for (var gprim in g.sceneGraph) {
	  
            // parse the name
            var matches = gprim.split(" ");

	    var ibody = 0;	
            //var ibody = Number(matches[1]);
            if        (matches[0] == "Face") {
                var iface = matches[1];
            } else if (matches[0] == "Edge") {
                var iedge = matches[1];
            } else {
                alert("unknown type: "+matches[0]);
                continue;
            }

            // determine if Body does not exists
            var kbody = -1;
            for (var jnode = 1; jnode < myTree.name.length; jnode++) {
                if (myTree.name[jnode] == "__Body "+ibody) {
                    kbody = jnode;
                }
            }

            // if Body does not exist, create it and its Face and Edge
            //    subnodes now
            var kface, kedge;
            if (kbody < 0) {
                myTree.addNode(1, "__Body "+ibody, "", null, "",
                               "Viz", "on", toggleViz, "Grd", "off", toggleGrd);
                kbody = myTree.name.length - 1;

                myTree.addNode(kbody, "____Faces", "", null, "",
                               "Viz", "on", toggleViz, "Grd", "off", toggleGrd, "Trn", "off", toggleTrn);
                kface = myTree.name.length - 1;

                myTree.addNode(kbody, "____Edges", "", null, "",
                               "Viz", "on", toggleViz, "Grd", "off", toggleGrd, "Ori", "off", toggleOri);
                kedge = myTree.name.length - 1;

            // otherwise, get pointers to the face-group and edge-group Nodes
            } else {
                kface = myTree.child[kbody];
                kedge = kface + 1;
            }

            // make the Tree Node
            if        (matches[0] == "Face") {
                myTree.addNode(kface, "______face "+iface, gprim, null, "disp2_cmenu",
                               "Viz", "on", toggleViz, "Grd", "off", toggleGrd, "Trn", "off", toggleTrn);
            } else if (matches[0] == "Edge") {
                myTree.addNode(kedge, "______edge "+iedge, gprim, null, "disp2_cmenu",
                               "Viz", "on", toggleViz, "Grd", "off", toggleGrd, "Ori", "off", toggleOri);
            }
        }
	
	g.debug = false;
        // open the Branches (by default)
        myTree.opened[3] = 1;

        // mark that we have (re-)built the Tree
        g.sgUpdate = 0;

        // convert the abstract Tree Nodes into an HTML table
        myTree.build();
    }
  //
  // deal with key presses
  if (g.keyPress != -1) 
  {
  
    if (g.keyPress == 42)       // '*' -- center the view
      g.centerV = 1;

    //if (g.keyPress == 60)       // '<' -- finer tessellation
     // g.socketUt.send("coarser");
      
    //if (g.keyPress == 62)       // '>' -- finer tessellation
     // g.socketUt.send("finer");

    if (g.keyPress ==  76)      // 'L' -- locating state
      if (g.locate == 1)
      {
        g.locate = 0;
      } else {
        g.locate = 1;
      }
  
    if (g.keyPress ==  80)      // 'P' -- picking state
      if (g.pick == 1) 
      {
        g.pick     = 0;
      } else {
        g.pick     = 1;
        g.sceneUpd = 1;
      }

    if (g.keyPress ==  99)      // 'c' -- color state
      if (g.active !== undefined) 
      {
        g.sceneGraph[g.active].attrs ^= g.plotAttrs.SHADING;
        g.sceneUpd = 1;
      }
      
    if (g.keyPress == 104)      // 'h' -- home (reset view transformation)
    {
      g.mvMatrix.makeIdentity();
      g.scale    = 1.0;
      g.sceneUpd = 1;
    }
        
    if (g.keyPress == 108)      // 'l' -- line state
      if (g.active !== undefined)
      {
        g.sceneGraph[g.active].attrs ^= g.plotAttrs.LINES;
        g.sceneUpd = 1;
      }

    if (g.keyPress == 110)      // 'n' -- next active
    {
      for (var gprim in g.sceneGraph)
      {
        if (g.active === undefined)
        {
          g.active = gprim;
          break;
        }
        if (g.active == gprim) g.active = undefined;
      }
    }

    if (g.keyPress == 111)      // 'o' -- orientation state
      if (g.active !== undefined)
      {
        g.sceneGraph[g.active].attrs ^= g.plotAttrs.ORIENTATION;
        g.sceneUpd = 1;
      }
        
    if (g.keyPress == 112)      // 'p' -- point state
      if (g.active !== undefined)
      {
        g.sceneGraph[g.active].attrs ^= g.plotAttrs.POINTS;
        g.sceneUpd = 1;
      }

    if (g.keyPress == 114)      // 'r' -- render state
      if (g.active !== undefined)
      {
        g.sceneGraph[g.active].attrs ^= g.plotAttrs.ON;
        g.sceneUpd = 1;
      }

    if (g.keyPress == 115)      // 's' -- set active to picked
      if (g.picked !== undefined) g.active = g.picked.gprim;

    if (g.keyPress == 116)      // 't' -- transparent state
      if (g.active !== undefined)
      {
        g.sceneGraph[g.active].attrs ^= g.plotAttrs.TRANSPARENT;
        g.sceneUpd = 1;
      }
  }
  g.keyPress = -1;

  //
  // UI is in screen coordinates (not object)
  g.uiMatrix.load(g.mvMatrix);
  g.mvMatrix.makeIdentity();

  if (g.wheelDelta !== 0)
  {
    var scale = Math.exp(g.wheelDelta/128.0);
    g.mvMatrix.scale(scale, scale, scale);
    g.scale   *= scale;
    g.sceneUpd = 1;
    g.wheelDelta = 0;
  }
  //
  // now mouse movement
  if (g.dragging) 
  {
    // alt and shift key is down
    if (g.modifier === (ALT_KEY|SHIFT_KEY) )
    {
      var angleX =  (g.startY-g.cursorY)/4.0;
      var angleY = -(g.startX-g.cursorX)/4.0;
      if ((angleX !== 0.0) || (angleY !== 0.0))
      {
        g.mvMatrix.rotate(angleX, 1,0,0);
        g.mvMatrix.rotate(angleY, 0,1,0);
        g.sceneUpd = 1;
      }
    }
    
    // alt is down
    if (g.modifier === ALT_KEY)
    {
      var xf = g.startX - g.width/2;
      var yf = g.startY - g.height/2;
      if ((xf !== 0.0) || (yf !== 0.0)) 
      {
        var theta1 = Math.atan2(yf, xf);
        xf = g.cursorX - g.width/2;
        yf = g.cursorY - g.height/2;
        if ((xf !== 0.0) || (yf !== 0.0)) 
        {
          var dtheta = Math.atan2(yf, xf)-theta1;
          if (Math.abs(dtheta) < 1.5708)
          {
            var angleZ = 128*(dtheta)/3.1415926;
            g.mvMatrix.rotate(angleZ, 0,0,1);
            g.sceneUpd = 1;
          }
        }
      }
    }

    // no modifier
    if (g.modifier === NO_MODIFIER)
    {
      var transX = (g.cursorX-g.startX)/256.0;
      var transY = (g.cursorY-g.startY)/256.0;
      if ((transX !== 0.0) || (transY !== 0.0))
      {
        g.mvMatrix.translate(transX, transY, 0.0);
        g.sceneUpd = 1;
      }
    }

    g.startX = g.cursorX;
    g.startY = g.cursorY;
  }

}


function wvUpdateView()
{
  g.mvMatrix.multiply(g.uiMatrix);
}


function wvServerMessage(text)
{
  logger(" Server Message: " + text);
}

function reshape(gl)
{
    var canvas = document.getElementById('WebViewer');

    canvas.height = window.innerHeight * 0.95;
    canvas.width = window.innerWidth * 0.95;

    if (g.offTop != canvas.offsetTop || g.offLeft != canvas.offsetLeft) {
        g.offTop  = canvas.offsetTop;
        g.offLeft = canvas.offsetLeft;
    }

    if (g.width == canvas.width && g.height == canvas.height) return;

    g.width  = canvas.width;
    g.height = canvas.height;

    // Set the viewport and projection matrix for the scene
    gl.viewport(0, 0, g.width, g.height);
    g.perspectiveMatrix = new J3DIMatrix4();
    g.sceneUpd = 1;

    wvInitDraw();
}

//
// put out a cursor
function jack(gl, x,y,z, delta)
{
  if (g.sceneGraph["jack"] !== undefined)
  {
    deleteGPrim(gl, "jack");
    delete g.sceneGraph["jack"];
  }

  var vertices = new Float32Array(18);
  for (var i = 0; i < 6; i++)
  {
    vertices[3*i  ] = x;
    vertices[3*i+1] = y;
    vertices[3*i+2] = z;
  }
  vertices[ 0] -= delta;
  vertices[ 3] += delta;
  vertices[ 7] -= delta;
  vertices[10] += delta;
  vertices[14] -= delta;
  vertices[17] += delta;
  
  var vbo = createVBO(gl, vertices,  undefined,
                          undefined, undefined);
  g.sceneGraph["jack"] = createGPrim(1, 1, g.plotAttrs.ON);
  g.sceneGraph["jack"].lines[0]  = vbo;
  g.sceneGraph["jack"].lineWidth = 3;
  g.sceneGraph["jack"].lColor    = [0.0, 0.0, 1.0];
  
}

function toggleViz(e) {
    // alert("in toggleViz("+e+")");

    // get the Tree Node
    var inode = e["target"].id.substring(4);
    inode     = inode.substring(0,inode.length-4);
    inode     = Number(inode);

    // toggle the Viz property
    if        (myTree.valu1[inode] == "off") {
        myTree.prop(inode, 1, "on");
    } else if (myTree.valu1[inode] == "on") {
        myTree.prop(inode, 1, "off");
    } else {
        alert("illegal Viz property:"+myTree.valu1[inode]);
        return;
    }
}

//
// callback to toggle Grd property
//
function toggleGrd(e) {
    // alert("in toggleGrd("+e+")");

    // get the Tree Node
    var inode = e["target"].id.substring(4);
    inode     = inode.substring(0,inode.length-4);
    inode     = Number(inode);

    // toggle the Grd property
    if        (myTree.valu2[inode] == "off") {
        myTree.prop(inode, 2, "on");
    } else if (myTree.valu2[inode] == "on") {
        myTree.prop(inode, 2, "off");
    } else {
        alert("illegal Grd property:"+myTree.valu2[inode]);
        return;
    }
}

//
// callback to toggle Trn property
//
function toggleTrn(e) {
    // alert("in toggleTrn("+e+")");

    // get the Tree Node
    var inode = e["target"].id.substring(4);
    inode     = inode.substring(0,inode.length-4);
    inode     = Number(inode);

    // toggle the Trn property
    if        (myTree.valu3[inode] == "off") {
        myTree.prop(inode, 3, "on");
    } else if (myTree.valu3[inode] == "on") {
        myTree.prop(inode, 3, "off");
    } else {
        alert("illegal Trn property:"+myTree.valu3[inode]);
        return;
    }
}

//
// callback to toggle Ori property
//
function toggleOri(e) {
    // alert("in toggleOri("+e+")");

    // get the Tree Node
    var inode = e["target"].id.substring(4);
    inode     = inode.substring(0,inode.length-4);
    inode     = Number(inode);

    // toggle the Ori property
    if        (myTree.valu3[inode] == "off") {
        myTree.prop(inode, 3, "on");
    } else if (myTree.valu3[inode] == "on") {
        myTree.prop(inode, 3, "off");
    } else {
        alert("illegal Ori property:"+myTree.valu3[inode]);
        return;
    }
}

function Tree(doc, treeId) {
    // alert("in Tree("+doc+","+treeId+")");

    // remember the document
    this.document = doc;
    this.treeId   = treeId;

    // arrays to hold the Nodes
    this.name   = new Array();
    this.gprim  = new Array();
    this.click  = new Array();
    this.cmenu  = new Array();
    this.parent = new Array();
    this.child  = new Array();
    this.next   = new Array();
    this.nprop  = new Array();
    this.opened = new Array();

    this.prop1  = new Array();
    this.valu1  = new Array();
    this.cbck1  = new Array();
    this.prop2  = new Array();
    this.valu2  = new Array();
    this.cbck2  = new Array();
    this.prop3  = new Array();
    this.valu3  = new Array();
    this.cbck3  = new Array();

    // initialize Node=0 (the root)
    this.name[  0] = "**root**";
    this.gprim[ 0] = "";
    this.click[ 0] = null;
    this.cmenu[ 0] = "";
    this.parent[0] = -1;
    this.child[ 0] = -1;
    this.next[  0] = -1;
    this.nprop[ 0] =  0;
    this.prop1[ 0] = "";
    this.valu1[ 0] = "";
    this.cbck1[ 0] = null;
    this.prop2[ 0] = "";
    this.valu2[ 0] = "";
    this.cbck2[ 0] = null;
    this.prop3[ 0] = "";
    this.valu3[ 0] = "";
    this.cbck3[ 0] = null;
    this.opened[0] = +1;

    // add methods
    this.addNode  = TreeAddNode;
    this.expand   = TreeExpand;
    this.contract = TreeContract;
    this.prop     = TreeProp;
    this.clear    = TreeClear;
    this.build    = TreeBuild;
    this.update   = TreeUpdate;
}

//
// add a Node to the Tree
//
function TreeAddNode(iparent, name, gprim, click, cmenu, prop1, valu1, cbck1, prop2, valu2, cbck2, prop3, valu3, cbck3) {
    // alert("in TreeAddNode("+iparent+","+name+","+gprim+","+click+","+cmenu+","+prop1+","+valu1+","+cbck1+","+prop2+","+valu2+","+cbck2+","+prop3+","+valu3+","+cbck3+")");

    // validate the input
    if (iparent < 0 || iparent >= this.name.length) {
        alert("iparent="+iparent+" is out of range");
        return;
    }

    // find the next Node index
    var inode = this.name.length;

    // store this Node's values
    this.name[  inode] = name;
    this.gprim[ inode] = gprim;
    this.click[ inode] = click;
    this.cmenu[ inode] = cmenu;
    this.parent[inode] = iparent;
    this.child[ inode] = -1;
    this.next[  inode] = -1;
    this.nprop[ inode] =  0;
    this.opened[inode] =  0;

    // store the properties
    if (prop1 !== undefined) {
        this.nprop[inode] = 1;
        this.prop1[inode] = prop1;
        this.valu1[inode] = valu1;
        this.cbck1[inode] = cbck1;
    }

    if (prop2 !== undefined) {
        this.nprop[inode] = 2;
        this.prop2[inode] = prop2;
        this.valu2[inode] = valu2;
        this.cbck2[inode] = cbck2;
    }

    if (prop3 !== undefined) {
        this.nprop[inode] = 3;
        this.prop3[inode] = prop3;
        this.valu3[inode] = valu3;
        this.cbck3[inode] = cbck3;
    }

    // if the parent does not have a child, link this
    //    new Node to the parent
    if (this.child[iparent] < 0) {
        this.child[iparent] = inode;

    // otherwise link this Node to the last parent's child
    } else {
        var jnode = this.child[iparent];
        while (this.next[jnode] >= 0) {
            jnode = this.next[jnode];
        }

        this.next[jnode] = inode;
    }
}

//
// build the Tree (ie, create the html table from the Nodes)
//
function TreeBuild() {
    // alert("in TreeBuild()");

    var doc = this.document;

    // if the table already exists, delete it and all its children (3 levels)
    var thisTable = doc.getElementById(this.treeId);
    if (thisTable) {
        var child1 = thisTable.lastChild;
        while (child1) {
            var child2 = child1.lastChild;
            while (child2) {
                var child3 = child2.lastChild;
                while (child3) {
                    child2.removeChild(child3);
                    child3 = child2.lastChild;
                }
                child1.removeChild(child2);
                child2 = child1.lastChild;
            }
            thisTable.removeChild(child1);
            child1 = thisTable.lastChild;
        }
        thisTable.parentNode.removeChild(thisTable);
    }

    // build the new table
    var newTable = doc.createElement("table");
    newTable.setAttribute("id", this.treeId);
    doc.getElementById("leftframe").appendChild(newTable);

    // traverse the Nodes using depth-first search
    var inode = 1;
    while (inode > 0) {

        // table row "node"+inode
        var newTR = doc.createElement("TR");
        newTR.setAttribute("id", "node"+inode);
        newTable.appendChild(newTR);

        // table data "node"+inode+"col1"
        var newTDcol1 = doc.createElement("TD");
        newTDcol1.setAttribute("id", "node"+inode+"col1");
        newTDcol1.className = "fakelinkon";
        newTR.appendChild(newTDcol1);

        var newTexta = doc.createTextNode("");
        newTDcol1.appendChild(newTexta);

        // table data "node"+inode+"col2"
        var newTDcol2 = doc.createElement("TD");
        newTDcol2.setAttribute("id", "node"+inode+"col2");
        if (this.cmenu[inode] != "") {
            newTDcol2.className = "fakelinkcmenu";
            newTDcol2.setAttribute("contextmenu", this.cmenu[inode]);
        }
        newTR.appendChild(newTDcol2);

        var newTextb = doc.createTextNode(this.name[inode]);
        newTDcol2.appendChild(newTextb);

        var name = this.name[inode];
        while (name.charAt(0) == "_") {
            name = name.substring(1);
        }

        var ibrch = 0;
        for (var jbrch = 0; jbrch < brch.length; jbrch++) {
            if (brch[jbrch].name == name) {
                if (brch[jbrch].ileft == -2) {
                    newTDcol2.className = "errorTD";
                }
                break;
            }
        }

        // table data "node"+inode+"col3"
        if (this.nprop[inode] > 0) {
            var newTDcol3 = doc.createElement("TD");
            newTDcol3.setAttribute("id", "node"+inode+"col3");
            if (this.cbck1[inode] != "") {
                newTDcol3.className = "fakelinkon";
            }
            newTR.appendChild(newTDcol3);

            if (this.nprop[inode] == 1) {
                newTDcol3.setAttribute("colspan", "3");
            }

            var newTextc = doc.createTextNode(this.prop1[inode]);
            newTDcol3.appendChild(newTextc);
        }

        // table data "node:+inode+"col4"
        if (this.nprop[inode] > 1) {
            var newTDcol4 = doc.createElement("TD");
            newTDcol4.setAttribute("id", "node"+inode+"col4");
            if (this.cbck2[inode] != "") {
                newTDcol4.className = "fakelinkon";
            }
            newTR.appendChild(newTDcol4);

            if (this.nprop[inode] == 2) {
                newTDcol4.setAttribute("colspan", "2");
            }

            var newTextd = doc.createTextNode(this.prop2[inode]);
            newTDcol4.appendChild(newTextd);
        }

        // table data "node:+inode+"col5"
        if (this.nprop[inode] > 2) {
            var newTDcol5 = doc.createElement("TD");
            newTDcol5.setAttribute("id", "node"+inode+"col5");
            if (this.cbck3[inode] != "") {
                newTDcol5.className = "fakelinkon";
            }
            newTR.appendChild(newTDcol5);

            var newTextd = doc.createTextNode(this.prop3[inode]);
            newTDcol5.appendChild(newTextd);
        }

        // go to next row
        if        (this.child[inode] >= 0) {
            inode = this.child[inode];
        } else if (this.next[inode] >= 0) {
            inode = this.next[inode];
        } else {
            while (inode > 0) {
                inode = this.parent[inode];
                if (this.parent[inode] == 0) {
                    newTR = doc.createElement("TR");
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

//
// clear the Tree
//
function TreeClear() {
    // alert("in TreeClear()");

    // remove all but the first Node
    this.name.splice(  1);
    this.gprim.splice( 1);
    this.click.splice( 1);
    this.cmenu.splice( 1);
    this.parent.splice(1);
    this.child.splice( 1);
    this.next.splice(  1);
    this.nprop.splice( 1);
    this.opened.splice(1);

    this.prop1.splice(1);
    this.valu1.splice(1);
    this.cbck1.splice(1);
    this.prop2.splice(1);
    this.valu2.splice(1);
    this.cbck2.splice(1);
    this.prop3.splice(1);
    this.valu3.splice(1);
    this.cbck3.splice(1);

    // reset the root Node
    this.parent[0] = -1;
    this.child[ 0] = -1;
    this.next[  0] = -1;
}

//
// expand a Node in the Tree
//
function TreeContract(inode) {
    // alert("in TreeContract("+inode+")");

    // validate inputs
    if (inode < 0 || inode >= this.opened.length) {
        alert("inode="+inode+" is out of range");
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

//
// expand a Node in the Tree
//
function TreeExpand(inode) {
    // alert("in TreeExpand("+inode+")");

    // validate inputs
    if (inode < 0 || inode >= this.opened.length) {
        alert("inode="+inode+" is out of range");
        return;
    }

    // expand inode
    this.opened[inode] = 1;

    // update the display
    this.update();
}

//
// change a property of a Node
//
function TreeProp(inode, iprop, onoff) {
    // alert("in TreeProp("+inode+","+iprop+","+onoff+")");

    // validate inputs
    if (inode < 0 || inode >= this.opened.length) {
        alert("inode="+inode+" is out of range");
        return;
    } else if (onoff != "on" && onoff != "off") {
        alert("onoff="+onoff+" is not 'on' or 'off'");
        return;
    }

    // set the property for inode
    if (iprop == 1) {
        this.valu1[inode] = onoff;
    } else if (iprop == 2) {
        this.valu2[inode] = onoff;
    } else if (iprop == 3) {
        this.valu3[inode] = onoff;
    } else {
        alert("iprop="+iprop+" is not 1, 2, or 3");
        return;
    }

    // set property of all descendents of inode
    for (var jnode = 1; jnode < this.parent.length; jnode++) {
        var iparent = this.parent[jnode];
        while (iparent > 0) {
            if (iparent == inode) {
                if        (iprop == 1) {
                    this.valu1[jnode] = onoff;
                } else if (iprop == 2) {
                    this.valu2[jnode] = onoff;
                } else if (iprop == 3) {
                    this.valu3[jnode] = onoff;
                }
                break;
            }

            iparent = this.parent[iparent];
        }
    }

    this.update();
}

//
// update the Tree (after build/expension/contraction/property-set)
//
function TreeUpdate() {
    // alert("in TreeUpdate()");

    var doc = this.document;

    // traverse the Nodes using depth-first search
    for (var inode = 1; inode < this.opened.length; inode++) {
        var element = doc.getElementById("node"+inode);

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

        // if the current Node has children, set up appropriate event handler to expand/collapse
        if (this.child[inode] > 0) {
            if (this.opened[inode] == 0) {
                var myElem = doc.getElementById("node"+inode+"col1");
                var This   = this;

                myElem.firstChild.nodeValue = "+";
                myElem.onclick = function () {
                    var thisNode = this.id.substring(4);
                    thisNode     = thisNode.substring(0,thisNode.length-4);
                    This.expand(thisNode);
                };

            } else {
                var myElem = doc.getElementById("node"+inode+"col1");
                var This   = this;

                myElem.firstChild.nodeValue = "-";
                myElem.onclick = function () {
                    var thisNode = this.id.substring(4);
                    thisNode     = thisNode.substring(0,thisNode.length-4);
                    This.contract(thisNode);
                };
            }
        }

        if (this.click[inode] !== null) {
            var myElem = doc.getElementById("node"+inode+"col2");
            myElem.onclick = this.click[inode];
        }

        // set the class of the properties
        if (this.nprop[inode] >= 1) {
            var myElem = doc.getElementById("node"+inode+"col3");
            myElem.onclick = this.cbck1[inode];

            if (this.prop1[inode] == "Viz") {
                if (this.valu1[inode] == "off") {
                    myElem.setAttribute("class", "fakelinkoff");
                    if (this.gprim[inode] != "") {
                        g.sceneGraph[this.gprim[inode]].attrs &= ~g.plotAttrs.ON;
                        g.sceneUpd = 1;
                    }
                } else {
                    myElem.setAttribute("class", "fakelinkon");
                    if (this.gprim[inode] != "") {
                        g.sceneGraph[this.gprim[inode]].attrs |=  g.plotAttrs.ON;
                        g.sceneUpd = 1;
                    }
                }
            }
        }

        if (this.nprop[inode] >= 2) {
            var myElem = doc.getElementById("node"+inode+"col4");
            myElem.onclick = this.cbck2[inode];

            if (this.prop2[inode] == "Grd") {
                if (this.valu2[ inode] == "off") {
                    myElem.setAttribute("class", "fakelinkoff");

                    if (this.gprim[inode] != "") {
                        g.sceneGraph[this.gprim[inode]].attrs &= ~g.plotAttrs.LINES;
                        g.sceneGraph[this.gprim[inode]].attrs &= ~g.plotAttrs.POINTS;
                        g.sceneUpd = 1;
                    }
                } else {
                    myElem.setAttribute("class", "fakelinkon");

                    if (this.gprim[inode] != "") {
                        g.sceneGraph[this.gprim[inode]].attrs |=  g.plotAttrs.LINES;
                        g.sceneGraph[this.gprim[inode]].attrs |=  g.plotAttrs.POINTS;
                        g.sceneUpd = 1;
                    }
                }
            }
        }

        if (this.nprop[inode] >= 3) {
            var myElem = doc.getElementById("node"+inode+"col5");
            myElem.onclick = this.cbck3[inode];

            if (this.prop3[inode] == "Trn") {
                if (this.valu3[ inode] == "off") {
                    myElem.setAttribute("class", "fakelinkoff");

                    if (this.gprim[inode] != "") {
                        g.sceneGraph[this.gprim[inode]].attrs &= ~g.plotAttrs.TRANSPARENT;
                        g.sceneUpd = 1;
                    }
                } else {
                    myElem.setAttribute("class", "fakelinkon");

                    if (this.gprim[inode] != "") {
                        g.sceneGraph[this.gprim[inode]].attrs |=  g.plotAttrs.TRANSPARENT;
                        g.sceneUpd = 1;
                    }
                }
            } else if (this.prop3[inode] == "Ori") {
                if (this.valu3[ inode] == "off") {
                    myElem.setAttribute("class", "fakelinkoff");

                    if (this.gprim[inode] != "") {
                        g.sceneGraph[this.gprim[inode]].attrs &= ~g.plotAttrs.ORIENTATION;
                        g.sceneUpd = 1;
                    }
                } else {
                    myElem.setAttribute("class", "fakelinkon");

                    if (this.gprim[inode] != "") {
                        g.sceneGraph[this.gprim[inode]].attrs |=  g.plotAttrs.ORIENTATION;
                        g.sceneUpd = 1;
                    }
                }
            }
        }
    }
}
           
//
// Status Line object
//
// This object keeps track of framerate plus other wv data and displays it as 
// the innerHTML text of the HTML element with the passed id. Once created you 
// call snapshot at the end of every rendering cycle. Every 500ms the framerate 
// is updated in the HTML element.
//

StatusLine = function(id)
{
    this.id = id;
    this.numFramerates = 10;
    this.framerateUpdateInterval = 500;

    this.renderTime = -1;
    this.framerates = [ ];
    var self = this;
    var fr = function() { self.updateFramerate(); };
    setInterval(fr, this.framerateUpdateInterval);
};


StatusLine.prototype.updateFramerate = function()
{
    var sline = document.getElementById(this.id);
    if (sline === null) {
      return;
    }
    var tot = 0;
    for (var i = 0; i < this.framerates.length; ++i)
        tot += this.framerates[i];

    var framerate = tot / this.framerates.length;
    framerate = Math.round(framerate);
    var string = "Framerate:"+framerate+"fps";
    if (g.picked !== undefined) 
      string = string+"&nbsp; &nbsp; &nbsp; Picked: "+g.picked.gprim+
                      "  strip = "+g.picked.strip+"  type = "+g.picked.type;
    if (g.active !== undefined) 
      string = string+"&nbsp; &nbsp; &nbsp; Active: "+g.active;
    if (g.located !== undefined)
      string = string+"&nbsp; &nbsp; &nbsp; ("+g.located[0]+", &nbsp; "+
                      g.located[1]+", &nbsp; "+g.located[2]+")";
              
    sline.innerHTML = string;
 };


StatusLine.prototype.snapshot = function()
{
    if (this.renderTime < 0)
        this.renderTime = new Date().getTime();
    else {
        var newTime = new Date().getTime();
        var t = newTime - this.renderTime;
        if (t === 0)
            return;
        var framerate = 1000/t;
        this.framerates.push(framerate);
        while (this.framerates.length > this.numFramerates)
            this.framerates.shift();
        this.renderTime = newTime;
    }
};
