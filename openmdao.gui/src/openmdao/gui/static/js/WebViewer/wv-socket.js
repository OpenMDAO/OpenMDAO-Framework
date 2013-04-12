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
  //var Uint8View = new Uint8Array(evt.data);
  //var msg, newbuf, dat;
  //var old = msgpack.use_utf8
  //msgpack.use_utf8 = 0  // turn off utf8 decoding
  //msg = msgpack.unpack(Uint8View);
  //msgpack.use_utf8 = old

  //newbuf = new ArrayBuffer(msg.length);
  //dat = new Uint8Array(newbuf);
  //// TODO: see if there's a faster way to convert str to Uint8Array
  //for(var i=0,j=msg.length;i<j;++i) {
  //   dat[i] = msg.charCodeAt(i);
  //}
  //logger(" Gprim-binary WebSocket getMessage: " + evt.type + 
  //    "  -- bytelength = " + evt.data.length); 
 
  //g.messageQ.push(newbuf);
  //console.debug("**** type of msg = "+(typeof evt.data));
  //console.debug("**** message instanceof String  = ");
  //console.debug(evt.data instanceof String);
  g.messageQ.push(evt.data);
}


function wsGpOnError(evt)
{
  alert(" Not connected to Server: Try reloading the page!");
  logger(" Gprim-binary WebSocket Error: " + evt.data);
}


//
// Init Web Socket interface
function getSockets(wsURLp, fname)
{
  var ws_ctor = window['MozWebSocket'] ? window['MozWebSocket'] : window['WebSocket'];
  var socketGp, socketUT;

  socketGp = new ws_ctor(wsURLp+'/ws_geometry/'+fname, 'pyv3d-bin-1.0'); 
  socketUt = new ws_ctor(wsURLp+'/ws_geometry/'+fname, 'pyv3d-txt-1.0');
    
  socketGp.binaryType = 'arraybuffer';
  socketGp.onopen     = function(evt) { wsGpOnOpen(evt);    };
  socketGp.onclose    = function(evt) { wsGpOnClose(evt);   };
  socketGp.onmessage  = function(evt) { wsGpOnMessage(evt); };
  socketGp.onerror    = function(evt) { wsGpOnError(evt);   };
  g.socketGp          = socketGp;
  
  socketUt.onopen    = function(evt) { wsUtOnOpen(evt);    };
  socketUt.onclose   = function(evt) { wsUtOnClose(evt);   };
  socketUt.onmessage = function(evt) { wsUtOnMessage(evt); };
  socketUt.onerror   = function(evt) { wsUtOnError(evt);   };
  g.socketUt         = socketUt;
}


/*
//
// Convert an Array Buffer to a String
function convert2string(array)
{
  var string = "";

  for (var i = 0, j=array.length; i < j; i++)
  {
    if (array[i] === 0) break;
    string += String.fromCharCode(array[i]);
  }

  return string;
}
*/

