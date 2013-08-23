/*
 *      wv: The Web Viewer
 *
 *              Render functions
 *
 *      Copyright 2011-2013, Massachusetts Institute of Technology
 *      Licensed under The GNU Lesser General Public License, version 2.1
 *      See http://www.opensource.org/licenses/lgpl-2.1.php
 *
 */


//
// A console function is added to the context: wv.logger(msg).
// By default, it maps to the window.console() function on WebKit and to an 
// empty function on other browsers.
wv["logger"] = function(msg)
{
    if (window.console && window.console.log) window.console.log("wv: "+msg);
}


//
// A debug-based console function is added to the context: wv.log(msg).
wv["log"] = function(msg)
{
    if (wv.debug != 0) wv.logger(msg);
}


//
// checks for OpenGL errors
wv["checkGLError"] = function(gl, source) {
    if (wv.debug <= 0) return;
    var error  = gl.getError();
    if (error != gl.NO_ERROR) {
        var str = "GL Error @ " + source + ": " + error;
        if (wv.debug == 1) wv.logger(str);
        if (wv.debug >  1) throw str;
    }
}


//
// figures out what is running
var BrowserDetect = {
	init: function () {
		this.browser = this.searchString(this.dataBrowser) || "An unknown browser";
		this.version = this.searchVersion(navigator.userAgent)
			|| this.searchVersion(navigator.appVersion)
			|| "an unknown version";
		this.OS = this.searchString(this.dataOS) || "an unknown OS";
	},
	searchString: function (data) {
		for (var i=0;i<data.length;i++)	{
			var dataString = data[i].string;
			var dataProp = data[i].prop;
			this.versionSearchString = data[i].versionSearch || data[i].identity;
			if (dataString) {
				if (dataString.indexOf(data[i].subString) != -1)
					return data[i].identity;
			}
			else if (dataProp)
				return data[i].identity;
		}
	},
	searchVersion: function (dataString) {
		var index = dataString.indexOf(this.versionSearchString);
		if (index == -1) return;
		return parseFloat(dataString.substring(index+this.versionSearchString.length+1));
	},
	dataBrowser: [
		{
			string: navigator.userAgent,
			subString: "Chrome",
			identity: "Chrome"
		},
		{ 	string: navigator.userAgent,
			subString: "OmniWeb",
			versionSearch: "OmniWeb/",
			identity: "OmniWeb"
		},
		{
			string: navigator.vendor,
			subString: "Apple",
			identity: "Safari",
			versionSearch: "Version"
		},
		{
			prop: window.opera,
			identity: "Opera",
			versionSearch: "Version"
		},
		{
			string: navigator.vendor,
			subString: "iCab",
			identity: "iCab"
		},
		{
			string: navigator.vendor,
			subString: "KDE",
			identity: "Konqueror"
		},
		{
			string: navigator.userAgent,
			subString: "Firefox",
			identity: "Firefox"
		},
		{
			string: navigator.vendor,
			subString: "Camino",
			identity: "Camino"
		},
		{		// for newer Netscapes (6+)
			string: navigator.userAgent,
			subString: "Netscape",
			identity: "Netscape"
		},
		{
			string: navigator.userAgent,
			subString: "MSIE",
			identity: "Explorer",
			versionSearch: "MSIE"
		},
		{
			string: navigator.userAgent,
			subString: "Gecko",
			identity: "Mozilla",
			versionSearch: "rv"
		},
		{ 		// for older Netscapes (4-)
			string: navigator.userAgent,
			subString: "Mozilla",
			identity: "Netscape",
			versionSearch: "Mozilla"
		}
	],
	dataOS : [
		{
			string: navigator.platform,
			subString: "Win",
			identity: "Windows"
		},
		{
			string: navigator.platform,
			subString: "Mac",
			identity: "Mac"
		},
		{
                        string: navigator.userAgent,
                        subString: "iPhone",
                        identity: "iPhone/iPod"
	        },
		{
			string: navigator.platform,
			subString: "Linux",
			identity: "Linux"
		}
	]

}


//
// Initialize the Canvas element with the passed name as a WebGL object and 
// return the WebGLRenderingContext.
// Turn off anti-aliasing so that picking works at the fringe
wv["initWebGL"] = function(canvasName)
{
    var canvas = document.getElementById(canvasName);
    return  gl = WebGLUtils.setupWebGL(canvas, { antialias: false });
}


//
// Load this shader and return the WebGLShader object.
//
wv["loadShader"] = function(ctx, shaderID, shaderType, shaderSrc)
{

  // Create the shader object
  var shader = ctx.createShader(shaderType);

  // Load the shader source
  ctx.shaderSource(shader, shaderSrc);

  // Compile the shader
  ctx.compileShader(shader);

  // Check the compile status
  var compiled = ctx.getShaderParameter(shader, ctx.COMPILE_STATUS);
  if (!compiled && !ctx.isContextLost()) {
    // Something went wrong during compilation; get the error
    var error = ctx.getShaderInfoLog(shader);
    wv.log("*** Error compiling shader "+shaderID+":"+error);
    ctx.deleteShader(shader);
    return null;
  }

  return shader;
}


//
// Load shaders with the passed sources and create a program with them. Return 
// this program in the 'program' property of the returned context.
//
// For each string in the passed attribs array, bind an attrib with that name 
// at that index. Once the attribs are bound, link the program and then use it.
//
// Set the clear color to the passed array (4 values) and set the clear depth 
// to the passed value.
// Enable depth testing
//
wv["Setup"] = function(gl, vshader, fshader, attribs, clearColor, clearDepth)
{
    // create our shaders
    var vertexShader   = wv.loadShader(gl, "Vertex",    gl.VERTEX_SHADER,
                                       vshader);
    var fragmentShader = wv.loadShader(gl, "Fragement", gl.FRAGMENT_SHADER,
                                       fshader);

    // Create the program object
    var program = gl.createProgram();

    // Attach our two shaders to the program
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);

    // Bind attributes
    for (var i = 0; i < attribs.length; ++i)
        gl.bindAttribLocation(program, i, attribs[i]);

    // Link the program
    gl.linkProgram(program);

    // Check the link status
    var linked = gl.getProgramParameter(program, gl.LINK_STATUS);
    if (!linked && !gl.isContextLost()) {
        // something went wrong with the link
        var error = gl.getProgramInfoLog (program);
        wv.logger("Error in program linking:" + error);

        gl.deleteProgram(program);
        gl.deleteProgram(fragmentShader);
        gl.deleteProgram(vertexShader);

        return null;
    }

    gl.useProgram(program);

    gl.clearColor(clearColor[0], clearColor[1], clearColor[2], clearColor[3]);
    gl.clearDepth(clearDepth);

    gl.enable(gl.DEPTH_TEST);

    return program;
}


//
// initialize the Web Viewer
//
wv["Initialize"] = function()
{
  var requestId;
  
  // "globals" used:

  wv.width    = -1;                                     // "canvas" size
  wv.height   = -1;
  wv.vbonum   =  0;                                     // vbo counter
  wv.scale    =  1.0;                                   // global view scale
  wv.picked   = undefined;                              // picked object
  wv.located  = undefined;                              // located coordinates
  wv.sgUpdate =  0;                                     // sceneGraph update
  wv.sceneUpd =  1;                                     // scene updated -- rerender
  wv.centerV  =  0;                                     // centering flag
  if (wv.debug     == undefined) wv.debug     = 0;      // debug flag
  if (wv.pick      == undefined) wv.pick      = 0;      // picking flag (0-off, 1-on)
  if (wv.locate    == undefined) wv.locate    = 0;      // locating flag
  if (wv.eye       == undefined) wv.eye       = [0.0, 0.0, 0.0];
  if (wv.center    == undefined) wv.center    = [0.0, 0.0, 0.0];
  if (wv.up        == undefined) wv.up        = [0,0, 1.0, 0.0];

  // define our plotting attributes
  wv.plotAttrs  = { ON:1,          TRANSPARENT:2, SHADING:4,
                    ORIENTATION:8, POINTS:16,     LINES:32   };

  // initialize our scene graph
  wv.sceneGraph = {};

  // initialize WebGL with the id of the Canvas Element
  var gl = wv.initWebGL(wv.canvasID);
  if (!gl) return;

  //
  // the shaders
  var vShaderSrc = [
"    uniform mat4   u_modelViewMatrix;           // not currently used",
"    uniform mat4   u_modelViewProjMatrix;",
"    uniform mat4   u_normalMatrix;",
"    uniform vec3   lightDir;",
"    uniform vec3   conNormal;                   // constant normal",
"    uniform vec3   conColor;                    // constant color",
"    uniform vec3   bacColor;                    // back face color",
"    uniform float  wAmbient;                    // Ambient light weight",
"    uniform float  wColor;                      // Constant color switch",
"    uniform float  bColor;                      // Backface color switch",
"    uniform float  wNormal;                     // Constant normal switch",
"    uniform float  wLight;                      // lighting switch",
"    uniform float  xpar;                        // transparency factor",
"    uniform float  linAdj;                      // line Z adjustment",
"    uniform float  pointSize;                   // point size in pixels",
"    uniform int    picking;                     // picking flag",
"",
"    attribute vec4 vPosition;",
"    attribute vec4 vColor;",
"    attribute vec3 vNormal;",
"",
"    varying float  z_Screen;",
"    varying vec4   v_Color;",
"    varying vec4   v_bColor;",
"",
"    void main()",
"    {",
"        // set the pixel position",    
"        gl_Position     = u_modelViewProjMatrix * vPosition;",
"        z_Screen        = gl_Position[2];",
"        gl_Position[2] += linAdj;",
"        if (wLight <= 0.0) gl_PointSize = pointSize; // set the point size",
"        if (picking != 0) return;",
"",
"        // assumes that colors are coming in as unsigned bytes",
"        vec4 color = vColor/255.0;",
"",
"        if (wLight <= 0.0) {",
"          // no lighting",
"          v_Color  = color*wColor + vec4(conColor,1)*(1.0-wColor);",
"          v_bColor = v_Color;",
"        } else {",
"          // setup bi-directional lighting",
"          //   a simple ambient/diffuse lighting model is used with:",
"          //      single 'white' source & no 'material' color",
"          //      linear mixture of ambient & diffuse based on weight",
"          vec3 lDirection = normalize(lightDir);",
"          vec3 norm       = vNormal*wNormal + conNormal*(1.0-wNormal);",
"          vec3 normal     = normalize(u_normalMatrix * vec4(norm,1)).xyz;",
"          float dot       = abs(dot(normal, lDirection));",
"",
"          // make the color to be rendered",
"          color           = color*wColor + vec4(conColor,1)*(1.0-wColor);",
"          v_Color         = color*dot + color*wAmbient;",
"          v_bColor        = v_Color;",
"          // are we coloring the backface?",
"          if (bColor != 0.0) {",
"              color       = vec4(bacColor,1);",
"              v_bColor    = color*dot + color*wAmbient;",
"          }",
"        }",
"        v_Color.a  = xpar;",
"        v_bColor.a = xpar;",
"    }"
  ].join("\n");

  var fShaderSrc = [
"    precision mediump float;",
"    uniform float bColor;                       // Backface color switch",
"    uniform int   picking;                      // picking flag",
"    uniform int   vbonum;                       // vbo number",
"",
"    varying float z_Screen;",
"    varying vec4  v_Color;",
"    varying vec4  v_bColor;",
"",
"    void main()",
"    {",
"        if (picking == 0) {",
"          gl_FragColor = v_Color;",
"          if ((bColor != 0.0) && !gl_FrontFacing) gl_FragColor = v_bColor;",
"        } else if (picking < 0) {",
"          gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);",
"          // save away x/100, y/100, z or 1/w value",
"          float              compnt = gl_FragCoord.x/100.;    // pixel",
"          if (picking == -2) compnt = gl_FragCoord.y/100.;    // pixel",
"//        if (picking == -3) compnt = gl_FragCoord.z;         // ?",
"          if (picking == -3) compnt = gl_FragCoord.w*z_Screen;",
"          if (picking == -4) compnt = gl_FragCoord.w;",
"          int pmsign = 0;",
"          if (compnt < 0.0) {",
"            compnt = -compnt;",
"            pmsign = 1;",
"          }",
"          if (compnt > 127.0) return;",
"          gl_FragColor.r = floor(compnt);",
"          if (pmsign == 1) gl_FragColor.r += 128.0;",
"          compnt         = fract(compnt)*256.0;",
"          gl_FragColor.g = floor(compnt);",
"          compnt         = fract(compnt)*256.0;",
"          gl_FragColor.b = floor(compnt);",
"          gl_FragColor.a = floor(fract(compnt)*256.0);",
"          if (gl_FragColor.a == 256.0) {",
"            gl_FragColor.a  = 0.0;",
"            gl_FragColor.b += 1.0;",
"          }",
"          if (gl_FragColor.b >= 256.0) {",
"            gl_FragColor.b -= 256.0;",
"            gl_FragColor.g += 1.0;",
"          }",
"          if (gl_FragColor.g >= 256.0) {",
"            gl_FragColor.g -= 256.0;",
"            gl_FragColor.r += 1.0;",
"          }",
"          if (gl_FragColor.a == 0.0) gl_FragColor.a = 1.0;",
"          gl_FragColor /= 255.;",
"        } else {",
"          int high       = vbonum/256;",
"          gl_FragColor.r = float(high)/255.;",
"          gl_FragColor.g = float(vbonum - high*256)/255.;",
"          gl_FragColor.b = 0.0;",
"          gl_FragColor.a = 0.0;",
"//        high           = gl_PrimitiveID/256;",
"//        gl_FragColor.b = float(high)/255.;",
"//        gl_FragColor.a = float(gl_PrimitiveID - high*256)/255.;",
"        }",
"    }"
  ].join("\n");

  //
  // setup the shaders and other stuff for rendering
  wv.program = wv.Setup(gl,
                        // The sources of the vertex and fragment shaders.
                        vShaderSrc, fShaderSrc,
                        // The vertex attribute names used by the shaders.
                        // The order they appear here corresponds to their indices.
                        [ "vPosition", "vColor", "vNormal" ],
                        // The clear color and depth values.
                        [ 0.0, 0.0, 0.0, 0.0 ], 1.0);

  //
  // Set up the uniform variables for the shaders
  gl.uniform3f(gl.getUniformLocation(wv.program,  "lightDir"), 0.0, 0.3, 1.0);
  gl.uniform1f(gl.getUniformLocation(wv.program,  "wAmbient"), 0.25);

  wv.u_xparLoc = gl.getUniformLocation(wv.program,      "xpar");
  gl.uniform1f(wv.u_xparLoc, 1.0);
  wv.u_linAdjLoc = gl.getUniformLocation(wv.program,    "linAdj");
  gl.uniform1f(wv.u_linAdjLoc, 0.0);
  wv.u_conNormalLoc = gl.getUniformLocation(wv.program, "conNormal");
  gl.uniform3f(wv.u_conNormalLoc, 0.0, 0.0, 1.0);
  wv.u_conColorLoc = gl.getUniformLocation(wv.program,  "conColor");
  gl.uniform3f(wv.u_conColorLoc, 0.0, 1.0, 0.0);
  wv.u_bacColorLoc = gl.getUniformLocation(wv.program,  "bacColor");
  gl.uniform3f(wv.u_bacColorLoc, 0.5, 0.5, 0.5);
  wv.u_wColorLoc = gl.getUniformLocation(wv.program,    "wColor");
  gl.uniform1f(wv.u_wColorLoc, 1.0);
  wv.u_bColorLoc = gl.getUniformLocation(wv.program,    "bColor");
  gl.uniform1f(wv.u_bColorLoc, 0.0);
  wv.u_wNormalLoc = gl.getUniformLocation(wv.program,   "wNormal");
  gl.uniform1f(wv.u_wNormalLoc, 1.0);
  wv.u_wLightLoc = gl.getUniformLocation(wv.program,    "wLight");
  gl.uniform1f(wv.u_wLightLoc, 1.0);
  wv.u_pointSizeLoc = gl.getUniformLocation(wv.program, "pointSize");
  gl.uniform1f(wv.u_pointSizeLoc, 2.0);
  wv.u_pickingLoc = gl.getUniformLocation(wv.program,   "picking");
  gl.uniform1i(wv.u_pickingLoc, 0);
  wv.u_vbonumLoc = gl.getUniformLocation(wv.program,    "vbonum");
  gl.uniform1i(wv.u_vbonumLoc, 0);

  //
  // Create some matrices to use later and save their locations
  wv.u_modelViewMatrixLoc =
                      gl.getUniformLocation(wv.program, "u_modelViewMatrix");
  wv.mvMatrix          = new J3DIMatrix4();
  wv.mvMatrix.makeIdentity();
  wv.u_normalMatrixLoc = gl.getUniformLocation(wv.program, "u_normalMatrix");
  wv.normalMatrix      = new J3DIMatrix4();
  wv.u_modelViewProjMatrixLoc =
                  gl.getUniformLocation(wv.program, "u_modelViewProjMatrix");
  wv.mvpMatrix         = new J3DIMatrix4();
        
  return gl;
}


//
// startup function
//
wv["Start"] = function()
{
  var c = document.getElementById(wv.canvasID);
  c.addEventListener('webglcontextlost',     wv.handleContextLost,     false);
  c.addEventListener('webglcontextrestored', wv.handleContextRestored, false);
  
  BrowserDetect.init();
  wv.logger(" Running: " + BrowserDetect.browser + " " + BrowserDetect.version +
                  " on " + BrowserDetect.OS);

  //
  // init the web viewer
  var gl = wv.Initialize();
  if (!gl) return;
  var nRbits = gl.getParameter(gl.RED_BITS);
  var nGbits = gl.getParameter(gl.GREEN_BITS);
  var nBbits = gl.getParameter(gl.BLUE_BITS);
  var nZbits = gl.getParameter(gl.DEPTH_BITS);
  wv.logger(" WebGL Number of Bits: Red " +nRbits+"  Green "  +nGbits+
                                 "  Blue "+nBbits+"  Zbuffer "+nZbits);
  wv.lineBump = -0.0002;
  if (nZbits < 24) wv.lineBump *= 2.0;
  
  //
  // initialize the UI
  wv.InitUI();

  //
  // setup our render loop
  var f = function() {
  
    if (wv.fov != undefined) wv.drawPicture(gl);

    // update the UI and matrices
    wv.UpdateUI();
    
    //update scene graph
    wv.UpdateScene(gl);

    requestId = window.requestAnimFrame(f, c);
  };
  f();


  wv["handleContextLost"] = function(e)
  {
    e.preventDefault();
    if (requestId !== undefined) {
      window.cancelRequestAnimFrame(requestId);
      requestId = undefined;
    }
  }

  wv["handleContextRestored"] = function()
  {
    wv.Initialize();
    f();
  }
}
