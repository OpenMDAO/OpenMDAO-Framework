/*
This file was written by Alexander Dyke, 2011

Feel free to use or modify this code however you please.
*/


/*
@fileoverview
This file contains functions to facilitate camera movement.

a targetCamera should be instantiated within the main class 
and passed the viewInfo, lightPosition, root of the model, and the client.

In the main class, mouse events should be set up, and passed
directly to the targetCamera, which will then take care of
the rest. 
Example: 

function initStep2(clientElements)
{

  ... //normal loading and setup code here

  g_viewInfo = o3djs.rendergraph.createBasicView(
      g_mainPack,
      this.client.root,
      this.client.renderGraphRoot);

  ...

  var root = this.client.root;
	//create the camera
  g_camera = o3djs.targetCamera.create(g_viewInfo, g_lightPosParam, root, g_client);

  ...

  //add the listeners

  o3djs.event.addEventListener(g_o3dElement, 'mousedown', startDragging);
  o3djs.event.addEventListener(g_o3dElement, 'mousemove', drag);
  o3djs.event.addEventListener(g_o3dElement, 'mouseup', stopDragging);
  o3djs.event.addEventListener(g_o3dElement, 'wheel', scrollMe);
  window.document.onkeypress = keyPressedCallback;

...

}


//these functions just pass mouse events to the camera

function startDragging(e) //mousedown
	{g_camera.click(e);}

function drag(e) //mousemove
	{g_camera.Drag(e);}
 
function stopDragging(e) //mouseup
	{g_camera.stopDrag();}

function scrollMe(e) //mouse scrolling
	{g_camera.mouseWheel(e);}

function keyPressedCallback(event) //keyboard pressed
{
	event = event || window.event;
	g_camera.keyPressed(event);
}





To manually move the camera, you may interact directly with 
the class through the aballRot, zoom, and panCam methods.

The rotation is handeled by an arcball. The arcball will
rotate the model geometry about the origin, and if the model
is not centered on the origin, the targetCamera will translate
the model back to its origional position. The result is the 
appearance of the camera orbiting the model's center.

Panning is done by shifting the viewing eye and target
by some offset along a vector. The vector allong which the
panning takes place is the vector perpendicular to both the 
viewing vector (between the eye and target), and the world's 
up vector.


*/
o3djs.provide('o3djs.targetCamera');

o3djs.require('o3djs.math');
o3djs.require('o3djs.quaternions'); //for post-arcball calculations
o3djs.require('o3djs.arcball');
o3djs.require('o3djs.picking');  //used to update the boundingbox
o3djs.require('o3djs.event');	//used to pre-process keyboard events


//create a namespace

o3djs.targetCamera = o3djs.targetCamera || {};


//constructor
o3djs.targetCamera.create = function(g_viewInfo, g_lightPosParam, g_root, g_client) 
{
	return new o3djs.targetCamera.TargetCamera(g_viewInfo, g_lightPosParam, g_root, g_client);
};

o3djs.targetCamera.TargetCamera = function(g_viewInfo, g_lightPosParam, g_root, g_client)
{
	//a great bunch of globals we need for dealing with camera stuff
	this.eye = [0,0,0];		//point defining where the camera actually is
	this.target = [0,0,0]; 		//where the camera looks
	this.up = [0,1,0];		//which way is up

	this.pan = [0,0];		//the panning coeficents
		
	this.distanceFromTarget = 700;	//distance the camera is from its target

	this.rightClick = false;
	this.middleButton = false;
	this.dragging = false;	

	this.offset;
	this.lastOffset;
	
	this.thisRot = o3djs.math.matrix4.identity();
	this.lastRot = o3djs.math.matrix4.identity();
	
	this.nearPlane = 1;		//near clipping plane
	this.farPlane = 10000000000000000000000;//far clipping plane, close to infinity

	this.viewInfo = g_viewInfo;	//need this to update the actual view on the webpage
	this.lightPosParam = g_lightPosParam; //where the light shines from
	this.root = g_root;
	this.client = g_client;
	
	this.aball = o3djs.arcball.create(300,100);

	this.treeInfo = null;
};

o3djs.targetCamera.TargetCamera.prototype.update = function ()
{
	this.eye[0] = this.target[0];
	this.eye[1] = this.target[1];
	this.eye[2] = this.target[2] + this.distanceFromTarget;


//To pan, we shift the whole view (eye and target) left/right and up/down 
//to do this, we calculate some vectors perp to the camera vector and the up vector, 
//and shift the view allong these vectors


//some temporary eye and target values

var eye = [this.eye[0], this.eye[1], this.eye[2]];
var target = [this.target[0], this.target[1], this.target[2]];



//first, determine the viewing vector we already have
var viewVec = [0,0,0];
viewVec[0] = eye[0] - target[0];
viewVec[1] = eye[1] - target[1];
viewVec[2] = eye[2] - target[2];

//now, determine a perp vector to both this vector, and the up vector
	//this vector slides the view left/right on the screen

var perpLR = o3djs.math.cross(viewVec, this.up);
perpLR = o3djs.math.normalize(perpLR); //create a unit vector

//a vector for the up/down movement on the screen. This is perp to both perpLR and the viewing vector
var perpUD = o3djs.math.cross(viewVec, perpLR);
perpUD = o3djs.math.normalize(perpUD); //make unit vector


//scale the pannign coeficent by a ratio of the distance to the target
var panx = this.pan[0] / (750/this.distanceFromTarget);
var pany = this.pan[1] / (750/this.distanceFromTarget);

//scale the panning vectors by the panning coeficent
perpLR[0] *= panx;
perpLR[1] *= panx;
perpLR[2] *= panx;

perpUD[0] *= pany;
perpUD[1] *= pany;
perpUD[2] *= pany;


//combine the LR and UD vectors into one master adjustment vector
var adjust = o3djs.math.addVector(perpUD, perpLR);

//use this vector to slide our eye and target, effectivly shifting the 'viewing window' 
eye = o3djs.math.addVector(adjust, eye);
target = o3djs.math.addVector(adjust, target);


//update the actual camera, and light value
	this.viewInfo.drawContext.view = o3djs.math.matrix4.lookAt(eye, target, this.up);
	this.lightPosParam.value = this.eye; //keep shining on the object!

};


o3djs.targetCamera.TargetCamera.prototype.zoom = function(y)
{
	//zoom by an exponntial coeficent
    var zoomPower = Math.abs(y / 6);
    var zoom;
    if (y > 0) {
      zoom = Math.pow(11 / 12, zoomPower);
    } else {
      zoom = Math.pow((1 + 1 / 12), zoomPower);
    }
    this.distanceFromTarget *= zoom;

};

o3djs.targetCamera.TargetCamera.prototype.panCam = function(x,y)
{
	//simply change the panning vector
	this.pan[0] += x ;
	this.pan[1] -= y ;
};



o3djs.targetCamera.TargetCamera.prototype.aballRot = function(e)
{
	var rotationQuat = this.aball.drag([e.x, e.y]);
	var rot_mat = o3djs.quaternions.quaternionToRotation(rotationQuat);
	this.thisRot = o3djs.math.matrix4.mul(this.lastRot, rot_mat);

	var m =  this.root.localMatrix;
	
	//translate the model back to its origonal position	

	
	m[3][0] = this.target[0];
	m[3][1] = this.target[1];
	m[3][2] = this.target[2];

	o3djs.math.matrix4.setUpper3x3(m, this.thisRot);

	this.root.translate([this.target[0] * -1, this.target[1] * -1, this.target[2] * -1]);
	this.root.localMatrix = m; //rotate the model
};
/*
-----------------------------
Code below to handle mouse events
-----------------------------
*/

o3djs.targetCamera.TargetCamera.prototype.click = function(e)  //registers a click event
{
	if (e.which) //for some platforms, 'which' works
		{this.rightClick = (e.which == 3); this.middleButton = (e.which == 2);} 
	else if (e.button) //other platforms, 'button' workds
		{this.rightClick = (e.button == 2); this.middleButton = (e.button == 1);}
	else {this.rightClick = false; this.middleButton = false;}

	this.offset = {x: e.x, y: e.y};
	this.lastOffset = this.offset;
	this.dragging = true;
	
	this.lastRot = this.thisRot;
	this.aball.click([e.x, e.y]);
};

o3djs.targetCamera.TargetCamera.prototype.drag = function(e) //mousemove event
{
	if(this.dragging && e.x != undefined)
	{
		this.offset = {x: e.x, y: e.y};

		dY = (this.offset.y - this.lastOffset.y);
		dX = (this.offset.x - this.lastOffset.x);
		this.lastOffset = this.offset;

		if(this.rightClick)
			{this.panCam(dX, dY);}
		else if(this.middleButton)
			{this.zoom(dY);}
		else
			{this.aballRot(e);}
	
		this.update();
	}
};

o3djs.targetCamera.TargetCamera.prototype.stopDrag = function() //mouseup
{
	this.dragging = false;
	this.lastRot = this.thisRot;
};

o3djs.targetCamera.TargetCamera.prototype.mouseWheel = function(e) //mousewheel
{
	if(e.deltaY)
	{
		this.zoom(e.deltaY/10);
		this.update();
	}
};
/*
-------------------------------------
Code below to handle keyboard events
-------------------------------------
*/

o3djs.targetCamera.TargetCamera.prototype.keyPressed = function(event) 
{
           if (event.metaKey){return;}

           var keyChar =String.fromCharCode(o3djs.event.getEventKeyChar(event));
           keyChar = keyChar.toLowerCase();
//window.console.log("event code: " + event.keyCode + "\nCalculated Character: " + keyChar + "\no3d Event Code: " + o3djs.event.getEventKeyChar(event));
           this.keyPressedAction(this.convertChar(keyChar), event.shiftKey);
};
o3djs.targetCamera.TargetCamera.prototype.convertChar = function(c)
{
	if(c == '+'|| c == '-') {return c;}
	if(c == '=') {return '+';}
	if(c == '_') {return '-';}

	return c.toLowerCase(); 
};

o3djs.targetCamera.TargetCamera.prototype.keyPressedAction = function(keyPressed, shift) 
{
	var panScale = 2;
	var rotScale = 3;
	var zScale = 1;
	if(shift){rotScale = 6; panScale = 5; zScale = 2;} //speed up if the shift key was down
	
	var center = {x: this.client.width/2, y: this.client.height/2};

	var pressed = true; //varable tells us if we took an action, so we know if we have to update the camera


        switch(keyPressed) 
	{
	     case 's':	//simulate a mouse click, dragging downward
		this.aball.click([center.x, center.y + rotScale/2]);
		this.aballRot({x: center.x, y: center.y - rotScale});
		this.lastRot = this.thisRot;
	     break;
	     case 'w': //rot up
		this.aball.click([center.x, center.y - rotScale/2]);
		this.aballRot({x: center.x, y: center.y + rotScale});
		this.lastRot = this.thisRot;
	     break;
	     case 'a':	//rot left
		this.aball.click([center.x + rotScale, center.y]);
		this.aballRot({x: center.x - rotScale * 2, y: center.y});
		this.lastRot = this.thisRot;
	     break;
	     case 'd': //rot right
		this.aball.click([center.x - rotScale, center.y]);
		this.aballRot({x: center.x + rotScale * 2, y: center.y});
		this.lastRot = this.thisRot;
	     break;
	     case 'i'://pan up
		this.panCam(0,panScale);
	     break;
	     case 'l': //pan left
		this.panCam(panScale,0);
	     break;
	     case 'k': //pan down
		this.panCam(0,-panScale);
	     break;
	     case 'j': //pan right
		this.panCam(-panScale,0);
	     break;
	     case '=': //zoom in
		this.zoom(zScale);
	     break;
	     case '+': //zoom in (when shift)
		this.zoom(zScale);
	     break;
	     case '-': //zomo out
		this.zoom(-zScale);
	     break;
	     case '_': //zoom out (when shift)
		this.zoom(-zScale);
	     break;
	     case 'r': //reset view

		//translation matricies get set to the identity matrix
		this.lastRot = o3djs.math.matrix4.identity();
  		this.thisRot = o3djs.math.matrix4.identity();

		//translate back to orig position
		this.root.localMatrix = o3djs.math.matrix4.setUpper3x3(this.root.localMatrix, o3djs.math.matrix4.identity());

		//force the arcball to update its matricies
		var e = {which: true, x: (this.client.width/2), y: this.client.height / 2}; //dummy click
		this.click(e); this.drag(e); this.stopDrag(e); //click, drag, release, all without moving the mouse

		//and set the eye and target back to what they were
		this.set();
	     break;
	     default:
		pressed = false; //we did not take any key action, so don't update the view!
        }

	if(pressed){this.update();}
}






/*
-----------------------------------
Code below to setup camrea initially
-----------------------------------
*/

o3djs.targetCamera.TargetCamera.prototype.updateTreeInfo = function() //used to update tree info when we are getting the  boundingBox
{
	if(!this.treeInfo)
	{
 	 this.treeInfo = o3djs.picking.createTransformInfo(this.root, null);
	}
  this.treeInfo.update();
};

o3djs.targetCamera.TargetCamera.prototype.set = function()
{
	//update the boudingBox
	this.updateTreeInfo();
	var bbox = this.treeInfo.getBoundingBox();
 
//google had a slick little algorithm to place the camera so the model was centered
      this.target = o3djs.math.lerpVector(bbox.minExtent, bbox.maxExtent, 0.5);
      var diag = o3djs.math.length(o3djs.math.subVector(bbox.maxExtent,
                                                bbox.minExtent));
      this.eye = o3djs.math.addVector(this.target, [0, 0, 1.5 * diag]);
	
	this.distanceFromTarget = o3djs.math.distance(this.eye, this.target);
	this.pan = [0,0];

	this.update();
};

