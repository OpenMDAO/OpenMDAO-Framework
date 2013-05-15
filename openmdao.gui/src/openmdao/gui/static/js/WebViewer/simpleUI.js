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
}

function wvUpdateUI()
{
    //console.log(g);
    // if the tree has not been created but the scene graph (possibly) exists...
    if (g.sgUpdate == 1 && (g.sceneGraph !== undefined)) {

        // ...count number of primitives in the scene graph
        var count = 0;
        for (var gprim in g.sceneGraph) {

            // parse the name
            /*var matches = gprim.split(" ");

            var ibody = Number(matches[1]);
            if        (matches[2] == "Face") {
                var iface = matches[3];
            } else if (matches[2] == "Loop") {
                var iloop = matches[3];
            } else if (matches[2] == "Edge") {
                var iedge = matches[3];
            } else {
                alert("unknown type: " + matches[2]);
                continue;
            }

            // determine if Body does not exists
            var knode = -1;
            /*for (var jnode = 1; jnode < myTree.name.length; jnode++) {
                if (myTree.name[jnode] == "Body " + ibody) {
                    knode = jnode;
                }
            }

            // if Body does not exist, create it and its Face, Loop, and Edge
            //    subnodes now
            var kface, kloop, kedge;
            if (knode < 0) {
                postMessage("Processing Body " + ibody);

                myTree.addNode(0, "Body " + ibody, "*");
                knode = myTree.name.length - 1;

                myTree.addNode(knode, "__Faces", "*");
                kface = myTree.name.length - 1;

                myTree.addNode(knode, "__Loops", "*");
                kloop = myTree.name.length - 1;

                myTree.addNode(knode, "__Edges", "*");
                kedge = myTree.name.length - 1;

            // otherwise, get pointers to the face-group and loop-group nodes
            } else {
                kface = myTree.child[knode];
                kloop = kface + 1;
                kedge = kloop + 1;
            }

            // make the tree node
            if        (matches[2] == "Face") {
                myTree.addNode(kface, "____face " + iface, gprim);
            } else if (matches[2] == "Loop") {
                myTree.addNode(kloop, "____loop " + iloop, gprim);
            } else if (matches[2] == "Edge") {
                myTree.addNode(kedge, "____edge " + iedge, gprim);
            }

            count++;*/
        }

        // if we had any primitives, we are assuming that we have all of
        //    them, so build the tree and remember that we have
        //    built the tree
        //if (count > 0) {
        //    myTree.build();
        //    g.sgUpdate = 0;
        //}
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
