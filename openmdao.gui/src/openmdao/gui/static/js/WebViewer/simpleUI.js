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

function getDisplayControl(display, isActive){
    var span = jQuery("<span></span>");
    span.html(display);

    var button = jQuery("<button></button>");
    button.addClass("btn btn-primary btn-small");
    button.addClass(display);
    button.attr("type", "button");
    button.data("controlType", display);

    if(isActive){
        button.addClass("active");
    }

    button.append(span);

    return button
}

function getDisplayControls(attrs){
    var html = jQuery("<div></div>");
    html.addClass("btn-group");
    html.attr("data-toggle", "buttons-checkbox");

    html.append(getDisplayControl("viz", isAttributeSet(attrs, g.plotAttrs.ON)));
    html.append(getDisplayControl("grd", isAttributeSet(attrs, g.plotAttrs.LINES | g.plotAttrs.POINTS)));
    html.append(getDisplayControl("trn", isAttributeSet(attrs, g.plotAttrs.TRANSPARENT)));
    html.append(getDisplayControl("ori", isAttributeSet(attrs, g.plotAttrs.ORIENTATION)));

    return html;    
}

function addBody(bodyIndex){

    jQuery("#leftframe").jstree("create_node", jQuery("#geom_display_body"), "inside", {
        "attr" : { 
            "id" : "geom_display_body_" + bodyIndex,
        },
        "data" : "" + bodyIndex,
        "state" : "closed",
    });

    jQuery("#leftframe").jstree("create_node", "#geom_display_body_" + bodyIndex, "inside", {
        "attr" : { "id" : "geom_display_body_" + bodyIndex + "_face"},
        "data" : "Faces",
        "state" : "closed",
    });

    jQuery("#leftframe").jstree("create_node", "#geom_display_body_" + bodyIndex, "inside", {
        "attr" : { "id" : "geom_display_body_" + bodyIndex + "_edge"},
        "data" : "Edges",
        "state" : "closed",
    });

    var bodyId = "#geom_display_body_" + bodyIndex;
    var bodyNode = jQuery(bodyId);
    var facesNode = bodyNode.children("ul").children().eq(1);
    var edgesNode = bodyNode.children("ul").children().eq(0);
   
    var attrs = g.plotAttrs.ON | g.plotAttrs.ORIENTATION; 
    var controls = getDisplayControls(attrs);

    controls.data("attrs", attrs);

    jQuery(bodyId + " ul").before(controls);
    facesNode.append(controls.clone(true));
    edgesNode.append(controls.clone(true));
}

function addVisual(bodyIndex, visualType, visualIndex, gprim){
    var parentId = "geom_display_body_" + bodyIndex + "_" + visualType;
    var nodeId = parentId + "_" + visualIndex;

    jQuery("#leftframe").jstree("create_node", "#" + parentId, visualIndex, {
        "attr" : { "id" : nodeId},
        "data" : "" + visualIndex + " ",
    });

    var node = jQuery("#" + nodeId);
    var controls = getDisplayControls(gprim.attrs);

    controls.data("gprim", gprim);
    node.append(controls);
}

function getData(button, key){
    return button.parent().data(key);
}

function setData(button, key, value){
    button.parent().data(key, value);
}

function getChildren(root, controlType){
    var tagName = root.prop("tagName");
    return root.parent().siblings("ul").children("li").children("div").children(tagName + "." + controlType)
    
}

function handleClick(attribute){
    var plotAttrs = {
        "viz" : g.plotAttrs.ON,
        "grd" : g.plotAttrs.LINES | g.plotAttrs.POINTS,
        "trn" : g.plotAttrs.TRANSPARENT,
        "ori" : g.plotAttrs.ORIENTATION,
    };
  
    return function(e){
        var button = jQuery(this).is("button") ? jQuery(this) : jQuery(this).parent();
        var attrMask = plotAttrs[attribute];
        var attrs = getData(button, "gprim") ? getData(button, "gprim").attrs : getData(button, "attrs");
        var attributeIsSet = isAttributeSet(attrMask, attrs);
       
        var setAttr = function(attributes){
            return setAttribute(attributes, attrMask, attrMask);
        };

        var resetAttr = function(attributes){
            return setAttribute(attributes, attrMask, 0);
        };

        var buttonActivator = attributeIsSet ? deactivateButton : activateButton;
        var attributeSetter = attributeIsSet ? resetAttr : setAttr;

        console.log(buttonActivator);

        toggleElement(button, attribute, attributeSetter, buttonActivator);

        g.sceneUpd = 1;
    };
}

function toggleElement(root, controlType, attributeSetter, iconSetter){
    if( hasChildren(root) ){
        setData(root, "attrs", attributeSetter(getData(root, "attrs")));
        getChildren(root, controlType).each(function(){
            var button  = jQuery(this);
            if(button.data("controlType") === root.data("controlType")){
                iconSetter(button);
                toggleElement(button, controlType, attributeSetter, iconSetter);
            }
        });
    } 
    
    else{
        var gprim = getData(root, "gprim");
        gprim.attrs = attributeSetter(gprim.attrs); 
        setData(root, "gprim", gprim);
    }
        
}

function isAttributeSet(setValue, currentValue){
    return (currentValue & setValue) >= 1;
} 

function setAttribute(attributes, mask, value){
    console.log("Attributes: " + attributes);
    console.log("Mask: " + mask);
    console.log("Value: " + value);

    return (attributes & (~mask)) | (value & mask);
}

function activateButton(button){
    button.addClass("active");
}

function deactivateButton(button){
    button.removeClass("active");
}

function hasChildren(button){
    return (button.parent().siblings("ul").size() > 0);
}

function addNode( id, parent ){
    parent = if parent !== undefined ? parent : "leftframe";
    jQuery("#leftframe").jstree("create_node", "#" + parent, "inside", {
        "attr" : { 
            "id" : id 
        },
        "data" : id,
    });
}

function wvUpdateUI()
{
// if the tree has not been created but the scene graph (possibly) exists...
// if the scene graph and Parameters have been updated, (re-)build the Tree
    if (g.sgUpdate == 1){ 

        if (g.sceneGraph === undefined) {
            alert("g.sceneGraph is undefined --- but we need it");
        }

        var ibody = 0;
        for(var gprim in g.sceneGraph){
        var primitiveType = g.scengeGraph[gprim].GPtype === 1 ? "Edges" : "Faces";
	if( jQuery("#" + gprim).length === 0 ){
            
	    if( jQuery("#" + primitiveType).length === 0 ){
                addNode(primitiveType);
                jQuery("#" + primitiveType).data("attrs", 1);
            }
	    addNode(gprim, primitiveType);
            jQuery("#" + gprim).data("grpim", g.sceneGraph[gprim]);
        }
        }
        /*if( jQuery("#geom_display_body_" + ibody).length === 0 ){
            //addBody(ibody);

            for (var gprim in g.sceneGraph){
                var matches = gprim.split(" ");
                addVisual(ibody, matches[0].toLowerCase(), parseInt(matches[1], 10), g.sceneGraph[gprim]);
            }

            jQuery(".viz").click(handleClick("viz"));
            jQuery(".grd").click(handleClick("grd"));
            jQuery(".trn").click(handleClick("trn"));
            jQuery(".ori").click(handleClick("ori"));
        }*/
    }

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
