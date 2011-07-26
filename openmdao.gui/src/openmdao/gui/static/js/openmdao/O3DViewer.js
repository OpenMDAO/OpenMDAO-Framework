
o3djs.base.o3d = o3d;
o3djs.require('o3djs.webgl');
o3djs.require('o3djs.util');
o3djs.require('o3djs.math');
o3djs.require('o3djs.quaternions');
o3djs.require('o3djs.rendergraph');
o3djs.require('o3djs.pack');
o3djs.require('o3djs.scene');
o3djs.require('o3djs.primitives');
o3djs.require('o3djs.picking');
o3djs.require('o3djs.debug');
o3djs.require('o3djs.camera');
o3djs.require('o3djs.io');
o3djs.require('o3djs.loader');
o3djs.require('o3djs.targetCamera');
o3djs.require('o3djs.canvas');



/* 
Copyright (c) 2010. All rights reserved.
LICENSE: NASA Open Source License
*/

var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

/**
 * 
 * @version 0.0.0
 * @constructor
 */
openmdao.O3DViewer = function(id,model) {
    /***********************************************************************
     *  private
     ***********************************************************************/
     
    var self = this,
        elm = jQuery("#"+id).width(screen.width).height(screen.height),
        menuDiv = jQuery("<nav2 id='"+id+"-menu'>"),
        o3dDiv =jQuery('<div id="o3d" style="width: 100%; height: 100%;">'),
        messageDiv = jQuery("<div>"),
        helpHTML = "<div>"+
            "Drag the mouse, or use the W, A, S, and D keys to rotate<br/>"+
            "Right-click and drag, or use the I, J, K, and L keys to pan<br/>"+
            "Middle-Button and drag, scrollwheel, or + and -  keys to zoom<br/>"+
            "When using keyboard, hold SHIFT to move model faster<br/>"+
            "Press R to reset the view"+
            "</div>"
    
    var  menus = [
        { text: "Help", onclick: "jQuery('"+helpHTML+"').dialog({'title':'Geometry Viewer','width':400,'height':150})" }
    ]
        
    var modelTransform;
    var g_simple;
    var g_url;
    var g_debugCount = 0;

    var g_root;
    var g_o3d;
    var g_math;
    var g_quaternions;
    var g_client;
    var g_pack = null;
    var g_mainPack;
    var g_viewInfo;
    var g_lightPosParam;
    var g_o3dWidth = -1;
    var g_o3dHeight = -1;
    var g_o3dElement;
    var g_finished = false;  // for selenium

    var g_camera;

    function showMessage(msg,color) {
        messageDiv.html(msg);
        messageDiv.css({"color":color});
        messageDiv.dialog({'title':'Geometry Viewer'})
    }
    
    function startDragging(e) { //mousedown
        g_camera.click(e);
    }

    function drag(e) { //mousemove
        g_camera.drag(e);
    }

    function stopDragging(e) {  //mouseup
        g_camera.stopDrag();
    }

    function scrollMe(e) { //mouse scrolling
        g_camera.mouseWheel(e);
    }

    function keyPressedCallback(event) { //keyboard pressed 
        event = event || window.event;
        g_camera.keyPressed(event);
    }

    function updateProjection() {
        // Create a perspective projection matrix.
        g_viewInfo.drawContext.projection = g_math.matrix4.perspective(
            g_math.degToRad(45), g_o3dWidth / g_o3dHeight, g_camera.nearPlane,
            g_camera.farPlane);
    }

    function loadFile(context, path) {
        function callback(pack, parent, exception) {
            //set the loading message
            if (exception) {
                showMessage("Could not load " + path + "\n" + exception,"red");
            } 
            else {
                showMessage("Loading finished.","green");

                // Generate draw elements and setup material draw lists.
                o3djs.pack.preparePack(pack, g_viewInfo);

                setCamera();

                setClientSize();
                g_camera.update();
                updateProjection();

                // Manually connect all the materials' lightWorldPos params to the context'
                var materials = pack.getObjectsByClassName('o3d.Material');
                for (var m = 0; m < materials.length; ++m) {
                    var material = materials[m];
                    var param = material.getParam('lightWorldPos');
                    if (param) {
                        param.bind(g_lightPosParam);
                    }
                }

                g_finished = true;  // for selenium

                // Comment out the next line to dump lots of info.
                if (false) {
                    o3djs.dump.dump('---dumping context---\n');
                    o3djs.dump.dumpParamObject(context);

                    o3djs.dump.dump('---dumping root---\n');
                    o3djs.dump.dumpTransformTree(g_client.root);

                    o3djs.dump.dump('---dumping render root---\n');
                    o3djs.dump.dumpRenderNodeTree(g_client.renderGraphRoot);

                    o3djs.dump.dump('---dump g_pack shapes---\n');
                    var shapes = pack.getObjectsByClassName('o3d.Shape');
                    for (var t = 0; t < shapes.length; t++) {
                        o3djs.dump.dumpShape(shapes[t]);
                    }

                    o3djs.dump.dump('---dump g_pack materials---\n');
                    var materials = pack.getObjectsByClassName('o3d.Material');
                    for (var t = 0; t < materials.length; t++) {
                        o3djs.dump.dump (
                        '  ' + t + ' : ' + materials[t].className +
                        ' : "' + materials[t].name + '"\n');
                        o3djs.dump.dumpParams(materials[t], '    ');
                    }

                    o3djs.dump.dump('---dump g_pack textures---\n');
                    var textures = pack.getObjectsByClassName('o3d.Texture');
                    for (var t = 0; t < textures.length; t++) {
                        o3djs.dump.dumpTexture(textures[t]);
                    }

                    o3djs.dump.dump('---dump g_pack effects---\n');
                    var effects = pack.getObjectsByClassName('o3d.Effect');
                    for (var t = 0; t < effects.length; t++) {
                        o3djs.dump.dump ('  ' + t + ' : ' + effects[t].className +
                        ' : "' + effects[t].name + '"\n');
                        o3djs.dump.dumpParams(effects[t], '    ');
                    }
                }
            }
        }

        g_pack = g_client.createPack();

        // Create a new transform for the loaded file
        var parent = g_pack.createObject('Transform');
        modelTransform = parent;
        parent.parent = g_client.root;
        if (path != null) { //more output for the loading information text
            showMessage("Processing " + path + "<br/>(this may take a minute, please be patient)","white");
            try {
                //counter, if any json files happen to have anamation, this counter will govern the anamation speed
                var secondCounter = g_pack.createObject('SecondCounter');
                secondCounter.countMode = o3d.Counter.CYCLE;
                secondCounter.start = 0;
                secondCounter.end = 1;
                
                o3djs.scene.loadScene( g_client, g_pack, parent, path, callback,
                {opt_async: false});

            } 
            catch (e) {
                showMessage("Loading failed: " + e,"red");
            }
        }
        return parent;
    }

    function setClientSize() {
        var newWidth  = parseInt(g_client.width);
        var newHeight = parseInt(g_client.height);

        if (newWidth != g_o3dWidth || newHeight != g_o3dHeight) {
            g_o3dWidth = newWidth;
            g_o3dHeight = newHeight;

            updateProjection();

            // Sets a new area size for arcball.
            g_camera.aball.setAreaSize(g_o3dWidth, g_o3dHeight);
        }
    }

    /**
    *  Called every frame.
    */
    function onRender() {
        // If we don't check the size of the client area every frame we don't get a
        // chance to adjust the perspective matrix fast enough to keep up with the
        // browser resizing us.
        setClientSize();
    }

    /**
    * Creates the client area.
    */
    function init() {
        o3djs.webgl.makeClients(initStep2);
        // The following call enables a debug WebGL context, which makes
        // debugging much easier.
        // o3djs.webgl.makeClients(initStep2, undefined, undefined, undefined, undefined, undefined, true);
    }

    /**
    * Initializes O3D and loads the scene into the transform graph.
    * @param {Array} clientElements Array of o3d object elements.
    */
    function initStep2(clientElements) {
        g_o3dElement = clientElements[0];
        g_o3d = g_o3dElement.o3d;
        g_math = o3djs.math;
        g_quaternions = o3djs.quaternions;
        g_client = g_o3dElement.client;

        g_mainPack = g_client.createPack();

        // Create the render graph for a view.
        g_viewInfo = o3djs.rendergraph.createBasicView(
            g_mainPack,
            g_client.root,
            g_client.renderGraphRoot);

        g_mainPack = g_client.createPack();

        var root = g_client.root;

        // Set the light at the same position as the camera to create a headlight
        // that illuminates the object straight on.
        var paramObject = g_mainPack.createObject('ParamObject');

        g_lightPosParam = paramObject.createParam('lightWorldPos', 'ParamFloat3');

        g_camera = o3djs.targetCamera.create(g_viewInfo, g_lightPosParam, root, g_client);

        setClientSize();

        doload()

        g_viewInfo.clearBuffer.clearColor = [0, 0, 0, 1]; //set background to black

        //setup the mouse and keyboard listeners
        o3djs.event.addEventListener(g_o3dElement, 'mousedown', startDragging);
        o3djs.event.addEventListener(g_o3dElement, 'mousemove', drag);
        o3djs.event.addEventListener(g_o3dElement, 'mouseup', stopDragging);
        o3djs.event.addEventListener(g_o3dElement, 'wheel', scrollMe);
        window.document.onkeypress = keyPressedCallback;
        g_client.setRenderCallback(onRender);
        
        o3dDiv.width(screen.width).height(screen.height)
    }


    /**
    * Removes any callbacks so they don't get called after the page has unloaded.'
    */
    function uninit() {
        if (g_client) {
            g_client.cleanup();
        }
    }

    function doload() {
        if (g_root) {
            g_root.parent = null;
            g_root = null;
        }
        if (g_pack) {
            g_pack.destroy();
            g_pack = null;
        }
        //var url = document.getElementById('url').value;
        if (g_url !== 'undefined') {
            g_root = loadFile(g_viewInfo.drawContext, g_url);
        }
    }

    function setCamera() {
        g_camera.set();
    }
    
    elm.html("")
    elm.append(menuDiv);
    new openmdao.Menu(menuDiv.attr('id'),model,menus)    
    elm.append(o3dDiv);
    init()
    
    /***********************************************************************
     *  privileged
     ***********************************************************************/
    
    this.load = function(url) {
        if (g_root) {
            g_root.parent = null;
            g_root = null;
        }
        if (g_pack) {
            g_pack.destroy();
            g_pack = null;
        }
        //var url = document.getElementById('url').value;
        if (url !== 'undefined') {
            g_root = loadFile(g_viewInfo.drawContext, url);
        }
    }
    
}
