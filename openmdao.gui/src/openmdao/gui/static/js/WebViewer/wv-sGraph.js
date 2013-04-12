/*
 *      wv: The Web Viewer
 *
 *              Scene Graph Functions
 *
 *      Copyright 2011-2012, Massachusetts Institute of Technology
 *      Licensed under The GNU Lesser General Public License, version 2.1
 *      See http://www.opensource.org/licenses/lgpl-2.1.php
 *
 */

//
// replace data in a gPrim
//
function editGPrim(ctx, name, stripe, gType, vType, data)
{

  if (g.sceneGraph[name] === undefined) 
  {
    logger(" Edit with no gPrim in SceneGraph: name = " + name);
    return;
  }
  var graphic = g.sceneGraph[name];
  if ((stripe >= graphic.nStrip) || (stripe < 0))
  {
    logger(" Edit with Stripe = " + stripe + 
           " in gPrim with name = " + name);
    return;
  }
  
  if (graphic.GPtype === 0)
  {
    // points
    if (gType !== 0) 
    {
      logger(" Point Edit with gType = " + gType + 
             " in gPrim with name = " + name);
      return;
    }
    if (vType === 0)
    {
      if (toType(data) != "Float32Array") {
        logger(" Point Edit Vertices Type = " + toType(data) +
               " should be Float32Array!");
        return;
      }
      ctx.deleteBuffer(graphic.points[stripe].vertex);
      delete graphic.points[stripe].vertex;
      graphic.points[stripe].nVerts = data.length/3;
      graphic.points[stripe].vertex = ctx.createBuffer();
      ctx.bindBuffer(ctx.ARRAY_BUFFER, graphic.points[stripe].vertex);
      ctx.bufferData(ctx.ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    } 
    else if (vType === 1)
    {
      if (toType(data) != "Uint16Array") {
        logger(" Point Edit Indices Type = " + toType(data) +
               " should be Uint16Array!");
        return;
      }
      if (graphic.points[stripe].nIndices !== undefined)
      {
        ctx.deleteBuffer(graphic.points[stripe].index);
        delete graphic.points[stripe].index;
      } 
      graphic.points[stripe].nIndices = data.length;
      graphic.points[stripe].index    = ctx.createBuffer();
      ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, graphic.points[stripe].index);
      ctx.bufferData(ctx.ELEMENT_ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    }
    else if (vType === 2)
    {
      if (toType(data) != "Uint8Array") {
        logger(" Point Edit Colors Type = " + toType(data) +
               " should be Uint8Array!");
        return;
      }
      if (graphic.points[stripe].color !== undefined)
      {
        ctx.deleteBuffer(graphic.points[stripe].color);
        delete graphic.points[stripe].color;
      }
      graphic.points[stripe].nVerts = data.length/3;
      graphic.points[stripe].color  = ctx.createBuffer();
      ctx.bindBuffer(ctx.ARRAY_BUFFER, graphic.points[stripe].color);
      ctx.bufferData(ctx.ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    }
    else
    {
      logger(" Point Edit with vType = " + vType + 
             " in gPrim with name = " + name);
      return;
    }
  }

  else if (graphic.GPtype === 1)
  {
    // lines
    if (gType === 0) 
    {
      if (vType !== 1)
      {
        logger(" Line/Point Edit with vType = " + vType + 
               " in gPrim with name = " + name);
        return;
      }
      if (toType(data) != "Uint16Array") {
        logger(" Line/Point Edit Indices Type = " + toType(data) +
               " should be Uint16Array!");
        return;
      }
      if (graphic.points[stripe].nIndices !== undefined)
      {
        ctx.deleteBuffer(graphic.points[stripe].index);
        delete graphic.points[stripe].index;
      }
      graphic.points[stripe].nIndices = data.length;
      graphic.points[stripe].index    = ctx.createBuffer();
      ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, graphic.points[stripe].index);
      ctx.bufferData(ctx.ELEMENT_ARRAY_BUFFER, data, ctx.STATIC_DRAW);
      return;      
    }
    else if (gType !== 1) 
    {
      logger(" Line Edit with gType = " + gType + 
             " in gPrim with name = " + name);
      return;
    }
    if (vType === 0)
    {
      if (toType(data) != "Float32Array") {
        logger(" Line Edit Vertices Type = " + toType(data) +
               " should be Float32Array!");
        return;
      }
      ctx.deleteBuffer(graphic.lines[stripe].vertex);
      delete graphic.lines[stripe].vertex;
      graphic.lines[stripe].nVerts = data.length/3;
      graphic.lines[stripe].vertex = ctx.createBuffer();
      ctx.bindBuffer(ctx.ARRAY_BUFFER, graphic.lines[stripe].vertex);
      ctx.bufferData(ctx.ARRAY_BUFFER, data, ctx.STATIC_DRAW);
      if (graphic.triangles.vertex !== undefined) {
        ctx.deleteBuffer(graphic.triangles.vertex);
        delete graphic.triangles.vertex;
        ctx.deleteBuffer(graphic.triangles.normal);
        delete graphic.triangles.normal;
        graphic.triangles.vertex = undefined;
      }
    } 
    else if (vType === 1)
    {
      if (toType(data) != "Uint16Array") {
        logger(" Line Edit Indices Type = " + toType(data) +
               " should be Uint16Array!");
        return;
      }
      if (graphic.lines[stripe].nIndices !== undefined)
      {
        ctx.deleteBuffer(graphic.lines[stripe].index);
        delete graphic.lines[stripe].index;
      }
      graphic.lines[stripe].nIndices = data.length;
      graphic.lines[stripe].index    = ctx.createBuffer();
      ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, graphic.lines[stripe].index);
      ctx.bufferData(ctx.ELEMENT_ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    }
    else if (vType === 2)
    {
      if (toType(data) != "Uint8Array") {
        logger(" Line Edit Colors Type = " + toType(data) +
               " should be Uint8Array!");
        return;
      }
      if (graphic.lines[stripe].color !== undefined)
      {
        ctx.deleteBuffer(graphic.lines[stripe].color);
        delete graphic.lines[stripe].color;
      }
      graphic.lines[stripe].nVerts = data.length/3; 
      graphic.lines[stripe].color  = ctx.createBuffer();
      ctx.bindBuffer(ctx.ARRAY_BUFFER, graphic.lines[stripe].color);
      ctx.bufferData(ctx.ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    }
    else if ((vType === 3) && (stripe === 0))
    {
      if (toType(data) != "Float32Array") {
        logger(" Line Edit Triangle Type = " + toType(data) +
               " should be Float32Array!");
        return;
      }
      if (graphic.triangles.vertex !== undefined) {
        ctx.deleteBuffer(graphic.triangles.vertex);
        delete graphic.triangles.vertex;
        ctx.deleteBuffer(graphic.triangles.normal);
        delete graphic.triangles.normal;
      }
      var len  = data.length/2;
      var vert = data.subarray(0, len);
      var norm = data.subarray(len);
      graphic.triangles.nVerts = len/3;
      graphic.triangles.vertex = ctx.createBuffer();
      ctx.bindBuffer(ctx.ARRAY_BUFFER, graphic.triangles.vertex);
      ctx.bufferData(ctx.ARRAY_BUFFER, vert, ctx.STATIC_DRAW);
      graphic.triangles.normal = ctx.createBuffer(); 
      ctx.bindBuffer(ctx.ARRAY_BUFFER, graphic.triangles.normal);
      ctx.bufferData(ctx.ARRAY_BUFFER, norm, ctx.STATIC_DRAW);      
    }
    else
    {
      logger(" Line Edit with vType = " + vType + 
             " in gPrim with name = " + name);
      return;
    }
  }

  else 
  {
    // triangles
    if (gType === 0) 
    {
      if (vType !== 1)
      {
        logger(" Triangle/Point Edit with vType = " + vType + 
               " in gPrim with name = " + name);
        return;
      }
      if (toType(data) != "Uint16Array") {
        logger(" Triangle/Point Edit Indices Type = " + toType(data) +
               " should be Uint16Array!");
        return;
      }
      if (graphic.points[stripe].nIndices !== undefined)
      {
        ctx.deleteBuffer(graphic.points[stripe].index);
        delete graphic.points[stripe].index;
      }
      graphic.points[stripe].nIndices = data.length;
      graphic.points[stripe].index    = ctx.createBuffer();
      ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, graphic.points[stripe].index);
      ctx.bufferData(ctx.ELEMENT_ARRAY_BUFFER, data, ctx.STATIC_DRAW);
      return;      
    }
    else if (gType === 1)
    {
      if (vType !== 1)
      {
        logger(" Triangle/Line Edit with vType = " + vType + 
               " in gPrim with name = " + name);
        return;
      }
      if (toType(data) != "Uint16Array") {
        logger(" Triangle/Line Edit Indices Type = " + toType(data) +
               " should be Uint16Array!");
        return;
      }
      if (graphic.lines[stripe].nIndices !== undefined)
      {
        ctx.deleteBuffer(graphic.lines[stripe].index);
        delete graphic.lines[stripe].index;
      }
      graphic.lines[stripe].nIndices = data.length;
      graphic.lines[stripe].index    = ctx.createBuffer();
      ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, graphic.lines[stripe].index);
      ctx.bufferData(ctx.ELEMENT_ARRAY_BUFFER, data, ctx.STATIC_DRAW);
      return; 
    }
    else if (gType > 2) 
    {
      logger(" Triangle Edit with gType = " + gType + 
             " in gPrim with name = " + name);
      return;
    }
    if (vType === 0)
    {
      if (toType(data) != "Float32Array") {
        logger(" Triangle Edit Vertices Type = " + toType(data) +
               " should be Float32Array!");
        return;
      }
      ctx.deleteBuffer(graphic.triangles[stripe].vertex);
      delete graphic.triangles[stripe].vertex;
      graphic.triangles[stripe].nVerts = data.length/3;
      graphic.triangles[stripe].vertex = ctx.createBuffer();
      ctx.bindBuffer(ctx.ARRAY_BUFFER, graphic.triangles[stripe].vertex);
      ctx.bufferData(ctx.ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    } 
    else if (vType === 1)
    {
      if (toType(data) != "Uint16Array") {
        logger(" Triangle Edit Indices Type = " + toType(data) +
               " should be Uint16Array!");
        return;
      }
      if (graphic.triangles[stripe].nIndices !== undefined)
      {
        ctx.deleteBuffer(graphic.triangles[stripe].index);
        delete graphic.triangles[stripe].index;
      }
      graphic.triangles[stripe].nIndices = data.length;
      graphic.triangles[stripe].index    = ctx.createBuffer();
      ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, graphic.triangles[stripe].index);
      ctx.bufferData(ctx.ELEMENT_ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    }
    else if (vType === 2)
    {
      if (toType(data) != "Uint8Array") {
        logger(" Triangle Edit Colors Type = " + toType(data) +
               " should be Uint8Array!");
        return;
      }
      if (graphic.triangles[stripe].color !== undefined)
      {
        ctx.deleteBuffer(graphic.triangles[stripe].color);
        delete graphic.triangles[stripe].color;
      }
      graphic.triangles[stripe].nVerts = data.length/3;
      graphic.triangles[stripe].color  = ctx.createBuffer();
      ctx.bindBuffer(ctx.ARRAY_BUFFER, graphic.triangles[stripe].color);
      ctx.bufferData(ctx.ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    }
    else if (vType === 3)
    {
      if (toType(data) != "Float32Array") {
        logger(" Triangle Edit Normals Type = " + toType(data) +
               " should be Float32Array!");
        return;
      }
      if (graphic.triangles[stripe].normal !== undefined)
      {
        ctx.deleteBuffer(graphic.triangles[stripe].normal);
        delete graphic.triangles[stripe].normal;
      }
      graphic.triangles[stripe].nVerts = data.length/3;
      graphic.triangles[stripe].normal = ctx.createBuffer();
      ctx.bindBuffer(ctx.ARRAY_BUFFER, graphic.triangles[stripe].normal);
      ctx.bufferData(ctx.ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    }
    else
    {
      logger(" Triangle Edit with vType = " + vType + 
             " in gPrim with name = " + name);
      return;
    }
  }
}


//
// Delete a gPrim
//
function deleteGPrim(ctx, name)
{
  if (g.sceneGraph[name] === undefined) 
  {
    logger(" Delete with no gPrim in SceneGraph: name = " + name);
    return;
  }
  var i;
  var graphic = g.sceneGraph[name];
  var nStripe = graphic.nStrip;
  
  if (graphic.GPtype === 0)
  {
    for (i = 0; i < nStripe; i++) releaseVBO(ctx, graphic.points[i]);
  }
  else if (graphic.GPtype == 1)
  {
    for (i = 0; i < nStripe; i++) 
    {
      if (graphic.points[i] !== undefined) 
        releaseIndexVBO(ctx, graphic.points[i]);
      releaseVBO(ctx, graphic.lines[i]);
    }
    if (graphic.trianges !== undefined)
      releaseVBO(ctx, graphic.triangles);
  }
  else
  {
    for (i = 0; i < nStripe; i++) 
    {
      if (graphic.points[i] !== undefined) 
        releaseIndexVBO(ctx, graphic.points[i]);
      if (graphic.lines[i] !== undefined) 
        releaseIndexVBO(ctx, graphic.lines[i]);
      releaseVBO(ctx, graphic.triangles[i]);
    }

  }
  
}


//
// Delete a stripe
//
function deleteStripe(ctx, name, stripe, gtype)
{
  if (g.sceneGraph[name] === undefined) 
  {
    logger(" deleteStripe with no gPrim in SceneGraph: name = " + name);
    return;
  }
  var graphic = g.sceneGraph[name];
  if ((stripe >= graphic.nStrip) || (stripe < 0))
  {
    logger(" deleteStripe in SceneGraph: name = " + name +
           "  stripe = " + stripe);
    return;
  }

  if (graphic.GPtype === 0)
  {
    releaseVBO(ctx, graphic.points[stripe]);
    graphic.points[stripe] = undefined;
  }
  else if (graphic.GPtype === 1)
  {
    if (gtype === 0)
    {
      if (graphic.points[stripe] !== undefined)
        releaseIndexVBO(ctx, graphic.points[stripe]);
      graphic.points[stripe] = undefined;
    }
    else
    {
      if (graphic.points[stripe] !== undefined)
        releaseIndexVBO(ctx, graphic.points[stripe]);
      releaseVBO(ctx, graphic.lines[stripe]);
      graphic.points[stripe] = undefined;
      graphic.lines[stripe]  = undefined;
    }
  }
  else
  {
    if (gtype === 0)
    {
      if (graphic.points[stripe] !== undefined)
        releaseIndexVBO(ctx, graphic.points[stripe]);
      graphic.points[stripe] = undefined;
    }
    else if (gtype === 1)
    {
      if (graphic.lines[stripe] !== undefined)
        releaseIndexVBO(ctx, graphic.lines[stripe]);
      graphic.lines[stripe] = undefined;
    }
    else
    {
      if (graphic.points[stripe] !== undefined)
        releaseIndexVBO(ctx, graphic.points[stripe]);
      if (graphic.lines[stripe]  !== undefined)
        releaseIndexVBO(ctx, graphic.lines[stripe]);
      releaseVBO(ctx, graphic.triangles[stripe]);
      graphic.points[stripe]    = undefined;
      graphic.lines[stripe]     = undefined;
      graphic.triangles[stripe] = undefined;
    }
  }

}


//
// Fill in a stripe for a new gPrim
//
function newStripe(ctx, name, stripe, gType, vertices, colors, indices, normals)
{
  if (g.sceneGraph[name] === undefined) 
  {
    logger(" newStripe with no gPrim in SceneGraph: name = " + name);
    return;
  }
  var graphic = g.sceneGraph[name];
  if ((stripe >= graphic.nStrip) || (stripe < 0))
  {
    logger(" newStripe with Stripe = " + stripe + 
           " in gPrim with name = " + name);
    return;
  }

  if (graphic.GPtype === 0)
  {
    // points
    if (gType !== 0) 
    {
      logger(" Point newStripe with gType = " + gType + 
             " in gPrim with name = " + name);
      return;
    }
    graphic.points[stripe] = createVBO(ctx, vertices, colors, indices, undefined);
  }
  
  else if (graphic.GPtype == 1)
  {
    // lines
    if (gType === 0)
    {
      graphic.points[stripe] = createIndexVBO(ctx, indices);
    }
    else if (gType !== 1) 
    {
      logger(" Line newStripe with gType = " + gType + 
             " in gPrim with name = " + name);
    }
    else 
    {
      graphic.lines[stripe] = createVBO(ctx, vertices, colors, indices, undefined);
      if ((stripe === 0) && (normals !== undefined))
      {
        var len  = normals.length/2;
        var vert = normals.subarray(0, len);
        var norm = normals.subarray(len);
        graphic.triangles = createVBO(ctx, vert, undefined, undefined, norm);      
      }
    }
  }

  else
  {
    // triangles
    if (gType === 0)
    {
      graphic.points[stripe] = createIndexVBO(ctx, indices);
    }
    else if (gType === 1) 
    {
      graphic.lines[stripe] = createIndexVBO(ctx, indices);
    }
    else if (gType !== 2)
    {
      logger(" Triangle newStripe with gType = " + gType + 
             " in gPrim with name = " + name);
      return;
    }
    else
    {
      graphic.triangles[stripe] = createVBO(ctx, vertices, colors, indices, normals);
    }
  }

}

//
// Check message queue to determine if the sceneGraph needs updating
function wvUpdateScene(gl)
{
  if (g.messageQ.length === 0) return;
  g.sceneUpd    = 1;
  var uint8nam, name, gtype, next, size, numBytes, vflags, data;
  var message   = g.messageQ[g.messageQ.length-1];
  var uint8View = new Uint8Array(message);
  var byteLen   = message.byteLength;
  var nameLen;
  if ((uint8View[byteLen-4] === 0) && (uint8View[byteLen-3] === 0) &&
      (uint8View[byteLen-2] === 0) && (uint8View[byteLen-1] === 7)) {
    // adjust message queue
    var messages = g.messageQ;
    g.messageQ   = [];
    
    log(" MessageQ len = " + messages.length);
    // update scene
    for (var i = 0; i < messages.length; i++) {
      message = messages[i];
      byteLen = message.byteLength;
      var start = g.MSG_START;  // beginning of message is topic name, so start at MSG_START
      while (start < byteLen)
      {
        var int32View = new Int32Array(message, start, 
                                       (byteLen-start)/4);
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
            nameLen  = int32View[1] & 0xFFFF;
            gtype    = int32View[1] >> 24;
            uint8nam = new Uint8Array(message, start+8, nameLen);
            name     = String.fromCharCode.apply(null, uint8nam);  //convert2string(uint8nam);
            next     = nameLen/4 + 2;
            numBytes = nameLen + 12;
            size     = 4;
            if (gtype == 1) size = 14;
            if (gtype == 2) size = 17;
            var float32  = new Float32Array(message, start+numBytes, size);
            if (g.sceneGraph[name] !== undefined) 
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
            nameLen = int32View[1] & 0xFFFF;
            if (nameLen === 0)
            {
              for (var gprim in g.sceneGraph)
              {
                deleteGPrim(gl, gprim);
              }
              g.sceneGraph = {};
            }
            else
            {
              uint8nam = new Uint8Array(message, start+8, nameLen);
              name     = String.fromCharCode.apply(null, uint8nam); // convert2string(uint8nam);
              deleteGPrim(gl, name);
              // remove the graphics primitive from the scene graph
              delete g.sceneGraph[name];
            }

            start += 8 + nameLen;
            break;
          case 3:
            // new VBO Data
            nameLen  =  int32View[1] & 0xFFFF;
            gtype    =  int32View[1] >> 24;
            vflags   = (int32View[1] >> 16) & 0xFF;
            uint8nam = new Uint8Array(message, start+8, nameLen);
            name     = String.fromCharCode.apply(null, uint8nam); // convert2string(uint8nam);
            var vertices, colors, indices, normals;
            size     = 0;
            numBytes = nameLen + 8;
            if ((vflags&1) !== 0)
            {
              size     = int32View[numBytes/4];
              log("     vertices size = " + size  + "  gtype = " + gtype);
              vertices  = new Float32Array(message, start+numBytes+4, size);
              numBytes += 4+size*4;
            }
            if ((vflags&2) !== 0)
            {
              size      = int32View[numBytes/4];
              log("     indices size = " + size  + "  gtype = " + gtype);
              indices   = new Uint16Array(message, start+numBytes+4, size);
              numBytes += 4+size*2;
              if ((size%2) !== 0) numBytes += 2;
            }
            if ((vflags&4) !== 0)
            {
              size      = int32View[numBytes/4];
              log("     colors size = " + size  + "  gtype = " + gtype);
              colors    = new Uint8Array(message, start+numBytes+4, size);
              numBytes += 4+size;
              if ((size%4) !== 0) numBytes += 4 - size%4;
            }
            if ((vflags&8) !== 0)
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
            nameLen  =  int32View[1] & 0xFFFF;
            gtype    =  int32View[1] >> 24;
            vflags   = (int32View[1] >> 16) & 0xFF;
            uint8nam = new Uint8Array(message, start+8, nameLen);
            name     = String.fromCharCode.apply(null, uint8nam); // convert2string(uint8nam);
            var vtype    = 0;
            if ((vflags&2) !== 0) vtype = 1;
            if ((vflags&4) !== 0) vtype = 2;
            if ((vflags&8) !== 0) vtype = 3;
            next = nameLen/4 + 2;
            size = int32View[next];
            log("     gPrim = "+name+"  vflags = " + vflags + "  gtype = " + 
                gtype + "  size = " + size);
            switch (vtype) {
              case 0:
                data = new Float32Array(message, start+nameLen+12, size);
                editGPrim(gl, name, stripe, gtype, 0, data);
                size *= 4;
                break;
              case 1:
                data = new Uint16Array(message, start+nameLen+12, size);
                editGPrim(gl, name, stripe, gtype, 1, data);
                var oldSize = size;
                size *= 2;
                if ((oldSize%2) !== 0) size += 2;
                break;
              case 2:
                data = new Uint8Array(message, start+nameLen+12, size);
                editGPrim(gl, name, stripe, gtype, 2, data);
                if ((size%4) !== 0) size += 4 - size%4;
                break;
              case 3:
                data = new Float32Array(message, start+nameLen+12, size);
                editGPrim(gl, name, stripe, gtype, 3, data);
                size *= 4;
                break;
            }
            
            start += 12 + nameLen + size;
            break;
          case 5:
            // Complete Update (same as 1 and 2) -- num of stripes must be the same
            nameLen  = int32View[1] & 0xFFFF;
            uint8nam = new Uint8Array(message, start+8, nameLen);
            name     = String.fromCharCode.apply(null, uint8nam); // convert2string(uint8nam);
            next     = nameLen/4 + 2;
            if (g.sceneGraph[name] !== undefined)
            {
              deleteGPrim(gl, name);
              g.sceneGraph[name].attrs = int32View[next];
            }

            start += 12 + nameLen;
            break;
          case 6:
            // complete stripe delete
            nameLen  = int32View[1] & 0xFFFF;
            gtype    = int32View[1] >> 24;
            uint8nam = new Uint8Array(message, start+8, nameLen);
            name     = String.fromCharCode.apply(null, uint8nam); // convert2string(uint8nam);
            deleteStripe(gl, name, stripe, gtype);

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
  
