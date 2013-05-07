/*
 *      wv: The Web Viewer
 *
 *              WebSocket functions
 *
 *      Copyright 2011-2012, Massachusetts Institute of Technology
 *      Licensed under The GNU Lesser General Public License, version 2.1
 *      See http://www.opensource.org/licenses/lgpl-2.1.php
 *
 */


function wsUtOnOpen(evt)
{
  logger(" UI-text WebSocket Connected!");
}


function wsUtOnClose(evt)
{
  logger(" UI-text WebSocket Disconnected!");
}


function wsUtOnMessage(evt)
{
/*
  log(" UI-text WebSocket getMessage: " + evt.data);
 */
  wvServerMessage(evt.data);
}


function wsUtOnError(evt)
{
  logger(" UI-text WebSocket Error: " + evt.data);
}


function wsGpOnOpen(evt)
{
  logger(" Gprim-binary WebSocket Connected!");
}


function wsGpOnClose(evt)
{
  logger(" Gprim-binary WebSocket Disconnected!");
}


function wsGpOnMessage(evt)
{
/*  
  var Uint8View = new Uint8Array(evt.data);
  log(" Gprim-binary WebSocket getMessage: " + evt.type + 
      "  -- bytelength = " + evt.data.byteLength); 
  log("                       end = " + Uint8View[evt.data.byteLength-1]);
 */
/*
  g.messageQ.push(evt.data.slice(0));
  delete evt.data;
 */

  // ignore non-binary messages here
  // FIXME: we really need to handler this at the framework level, possibly
  //    by including some extra info in the topic string, like  <protocol_name>:topic
  //    For example, for pyV3D messages,  pyV3D:var_path_name
  if (typeof evt.data === 'ArrayBuffer' || evt.data instanceof ArrayBuffer) {
    console.debug("pushing ", evt.data);
    g.messageQ.push(evt.data);
  }
}


function wsGpOnError(evt)
{
  alert(" Not connected to Server: Try reloading the page!");
  logger(" Gprim-binary WebSocket Error: " + evt.data);
}


//
// Init Web Socket interface
function getSockets(wsURLp)
{

  var ws_ctor = window['MozWebSocket'] ? window['MozWebSocket'] : window['WebSocket'];

  var socketGp        = new ws_ctor(wsURLp, "gprim-binary-protocol");
  socketGp.binaryType = 'arraybuffer';
  socketGp.onopen     = function(evt) { wsGpOnOpen(evt)    };
  socketGp.onclose    = function(evt) { wsGpOnClose(evt)   };
  socketGp.onmessage  = function(evt) { wsGpOnMessage(evt) };
  socketGp.onerror    = function(evt) { wsGpOnError(evt)   };
  g.socketGp          = socketGp;
  
  var socketUt       = new ws_ctor(wsURLp, "ui-text-protocol");
  socketUt.onopen    = function(evt) { wsUtOnOpen(evt)    };
  socketUt.onclose   = function(evt) { wsUtOnClose(evt)   };
  socketUt.onmessage = function(evt) { wsUtOnMessage(evt) };
  socketUt.onerror   = function(evt) { wsUtOnError(evt)   };
  g.socketUt         = socketUt;
  
  g.messageQ         = [];              // a place to put the binary messages
}


//
// Convert an Array Buffer to a String
function convert2string(array)
{
  var string = "";

  for (var i = 0; i < array.length; i++)
  {
    if (array[i] == 0) break;
    string += String.fromCharCode(array[i]);
  }

  return string;
}


//
// Use WebSockets to determine if the sceneGraph needs updating
function wvUpdateScene(gl)
{
  if (g.messageQ.length == 0) return;
  
  g.sceneUpd    = 1;
  var message   = g.messageQ[g.messageQ.length-1];
  var uint8View = new Uint8Array(message);
  var byteLen   = message.byteLength;
  if ((uint8View[byteLen-4] == 0) && (uint8View[byteLen-3] == 0) &&
      (uint8View[byteLen-2] == 0) && (uint8View[byteLen-1] == 7)) {
    // adjust message queue
    var messages = g.messageQ;
    g.messageQ   = [];
    
    log(" MessageQ len = " + messages.length);
    // update scene
    for (var i = 0; i < messages.length; i++)
    {
      message = messages[i];
      var start = g.MSG_START;
      while (start < message.byteLength)
      {
        var int32View = new Int32Array(message, start, 
                                       (message.byteLength-start)/4);
        var opcode = int32View[0] >> 24;
        var stripe = int32View[0] & 0xFFFFFF;
        log("   Message = " + i + "  Start = " + start + "  Opcode = " + opcode +
            "   Stripe = " + stripe);
        switch (opcode) {
          case 8:
            // init with FOV
            var float32View = new Float32Array(message, start+4, 12);
            g.fov       = float32View[ 0];
            g.zNear     = float32View[ 1];
            g.zFar      = float32View[ 2];
            g.eye[0]    = float32View[ 3];
            g.eye[1]    = float32View[ 4];
            g.eye[2]    = float32View[ 5];
            g.center[0] = float32View[ 6];
            g.center[1] = float32View[ 7];
            g.center[2] = float32View[ 8];
            g.up[0]     = float32View[ 9];
            g.up[1]     = float32View[10];
            g.up[2]     = float32View[11];
            start      += 52;
            break;
          case 1:
            // new gPrim
            g.sgUpdate   = 1;
            var gtype    = int32View[1] >> 24;
            var nameLen  = int32View[1] & 0xFFFF;
            var uint8nam = new Uint8Array(message, start+8, nameLen);
            var name     = convert2string(uint8nam);
            var next     = nameLen/4 + 2;
            var numBytes = nameLen + 12;
            var size     = 4;
            if (gtype == 1) size = 14;
            if (gtype == 2) size = 17;
            var float32  = new Float32Array(message, start+numBytes, size);
            if (g.sceneGraph[name] != undefined) 
            {
               log(" Error: SceneGraph already has: " + name);
            }
            else
            {
              g.sceneGraph[name] = createGPrim(gtype, stripe, int32View[next]);
              g.sceneGraph[name].pSize     = float32[0];
              g.sceneGraph[name].pColor[0] = float32[1];
              g.sceneGraph[name].pColor[1] = float32[2];
              g.sceneGraph[name].pColor[2] = float32[3];
              if (gtype > 0) 
              {
                g.sceneGraph[name].lWidth    = float32[ 4];
                g.sceneGraph[name].lColor[0] = float32[ 5];
                g.sceneGraph[name].lColor[1] = float32[ 6];
                g.sceneGraph[name].lColor[2] = float32[ 7];
                g.sceneGraph[name].fColor[0] = float32[ 8];
                g.sceneGraph[name].fColor[1] = float32[ 9];
                g.sceneGraph[name].fColor[2] = float32[10];
                g.sceneGraph[name].bColor[0] = float32[11];
                g.sceneGraph[name].bColor[1] = float32[12];
                g.sceneGraph[name].bColor[2] = float32[13];
              }
              if (gtype > 1)
              {
                g.sceneGraph[name].normal[0] = float32[14];
                g.sceneGraph[name].normal[1] = float32[15];
                g.sceneGraph[name].normal[2] = float32[16];
              }
            }

            start += numBytes + size*4;
            break;
          case 2:
            // delete gPrim
            g.sgUpdate  = 1;
            var nameLen = int32View[1] & 0xFFFF;
            if (nameLen == 0)
            {
              for (var gprim in g.sceneGraph)
              {
                deleteGPrim(gl, gprim);
              }
              g.sceneGraph = {};
            }
            else
            {
              var uint8nam = new Uint8Array(message, start+8, nameLen);
              var name     = convert2string(uint8nam);
              deleteGPrim(gl, name);
              // remove the graphics primitive from the scene graph
              delete g.sceneGraph[name];
            }

            start += 8 + nameLen;
            break;
          case 3:
            // new VBO Data
            var gtype    =  int32View[1] >> 24;
            var vflags   = (int32View[1] >> 16) & 0xFF;
            var nameLen  =  int32View[1] & 0xFFFF;
            var uint8nam = new Uint8Array(message, start+8, nameLen);
            var name     = convert2string(uint8nam);
            var vertices = undefined;
            var colors   = undefined;
            var indices  = undefined;
            var normals  = undefined;
            var size     = 0;
            var numBytes = nameLen + 8;
            if ((vflags&1) != 0)
            {
              size     = int32View[numBytes/4];
              log("     vertices size = " + size  + "  gtype = " + gtype);
              vertices  = new Float32Array(message, start+numBytes+4, size);
              numBytes += 4+size*4;
            }
            if ((vflags&2) != 0)
            {
              size      = int32View[numBytes/4];
              log("     indices size = " + size  + "  gtype = " + gtype);
              indices   = new Uint16Array(message, start+numBytes+4, size);
              numBytes += 4+size*2;
              if ((size%2) != 0) numBytes += 2;
            }
            if ((vflags&4) != 0)
            {
              size      = int32View[numBytes/4];
              log("     colors size = " + size  + "  gtype = " + gtype);
              colors    = new Uint8Array(message, start+numBytes+4, size);
              numBytes += 4+size;
              if ((size%4) != 0) numBytes += 4 - size%4;
            }
            if ((vflags&8) != 0)
            {
              size      = int32View[numBytes/4];
              log("     normals size = " + size + "  gtype = " + gtype);
              normals   = new Float32Array(message, start+numBytes+4, size);
              numBytes += 4+size*4;
            }
            newStripe(gl, name, stripe, gtype, vertices, colors, indices, normals);

            start += numBytes;
            break;
          case 4:
            // VBO update (only one at a time)
            var gtype    =  int32View[1] >> 24;
            var vflags   = (int32View[1] >> 16) & 0xFF;
            var nameLen  =  int32View[1] & 0xFFFF;
            var uint8nam = new Uint8Array(message, start+8, nameLen);
            var name     = convert2string(uint8nam);
            var vtype    = 0;
            if ((vflags&2) != 0) vtype = 1;
            if ((vflags&4) != 0) vtype = 2;
            if ((vflags&8) != 0) vtype = 3;
            var next = nameLen/4 + 2;
            var size = int32View[next];
            log("     gPrim = "+name+"  vflags = " + vflags + "  gtype = " + 
                gtype + "  size = " + size);
            switch (vtype) {
              case 0:
                var data = new Float32Array(message, start+nameLen+12, size);
                editGPrim(gl, name, stripe, gtype, 0, data);
                size *= 4;
                break;
              case 1:
                var data = new Uint16Array(message, start+nameLen+12, size);
                editGPrim(gl, name, stripe, gtype, 1, data);
                var oldSize = size;
                size *= 2;
                if ((oldSize%2) != 0) size += 2;
                break;
              case 2:
                var data = new Uint8Array(message, start+nameLen+12, size);
                editGPrim(gl, name, stripe, gtype, 2, data);
                if ((size%4) != 0) size += 4 - size%4;
                break;
              case 3:
                var data = new Float32Array(message, start+nameLen+12, size);
                editGPrim(gl, name, stripe, gtype, 3, data);
                size *= 4;
                break;
            }
            
            start += 12 + nameLen + size;
            break;
          case 5:
            // Complete Update (same as 1 and 2) -- num of stripes must be the same
            var nameLen  = int32View[1] & 0xFFFF;
            var uint8nam = new Uint8Array(message, start+8, nameLen);
            var name     = convert2string(uint8nam);
            var next     = nameLen/4 + 2;
            if (g.sceneGraph[name] != undefined)
            {
              deleteGPrim(gl, name);
              g.sceneGraph[name].attrs = int32View[next];
            }

            start += 12 + nameLen;
            break;
          case 6:
            // complete stripe delete
            var gtype    = int32View[1] >> 24;
            var nameLen  = int32View[1] & 0xFFFF;
            var uint8nam = new Uint8Array(message, start+8, nameLen);
            var name     = convert2string(uint8nam);
            deleteStripe(gl, name, stripe, gtype)

            start += 8 + nameLen;
            break;
          default:
            // opcode 0 & 7, just move on
            start += 4;
            break;
        }
        
      }
      
    }

  }
}
  
