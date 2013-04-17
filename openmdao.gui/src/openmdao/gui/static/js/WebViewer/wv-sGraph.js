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

  if (g.sceneGraph[name] == undefined) 
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
  
  if (graphic.GPtype == 0)
  {
    // points
    if (gType != 0) 
    {
      logger(" Point Edit with gType = " + gType + 
             " in gPrim with name = " + name);
      return;
    }
    if (vType == 0)
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
    else if (vType == 1)
    {
      if (toType(data) != "Uint16Array") {
        logger(" Point Edit Indices Type = " + toType(data) +
               " should be Uint16Array!");
        return;
      }
      if (graphic.points[stripe].nIndices != undefined)
      {
        ctx.deleteBuffer(graphic.points[stripe].index);
        delete graphic.points[stripe].index;
      } 
      graphic.points[stripe].nIndices = data.length;
      graphic.points[stripe].index    = ctx.createBuffer();
      ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, graphic.points[stripe].index);
      ctx.bufferData(ctx.ELEMENT_ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    }
    else if (vType == 2)
    {
      if (toType(data) != "Uint8Array") {
        logger(" Point Edit Colors Type = " + toType(data) +
               " should be Uint8Array!");
        return;
      }
      if (graphic.points[stripe].color != undefined)
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

  else if (graphic.GPtype == 1)
  {
    // lines
    if (gType == 0) 
    {
      if (vType != 1)
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
      if (graphic.points[stripe].nIndices != undefined)
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
    else if (gType != 1) 
    {
      logger(" Line Edit with gType = " + gType + 
             " in gPrim with name = " + name);
      return;
    }
    if (vType == 0)
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
      if (graphic.triangles.vertex != undefined) {
        ctx.deleteBuffer(graphic.triangles.vertex);
        delete graphic.triangles.vertex;
        ctx.deleteBuffer(graphic.triangles.normal);
        delete graphic.triangles.normal;
        graphic.triangles.vertex = undefined;
      }
    } 
    else if (vType == 1)
    {
      if (toType(data) != "Uint16Array") {
        logger(" Line Edit Indices Type = " + toType(data) +
               " should be Uint16Array!");
        return;
      }
      if (graphic.lines[stripe].nIndices != undefined)
      {
        ctx.deleteBuffer(graphic.lines[stripe].index);
        delete graphic.lines[stripe].index;
      }
      graphic.lines[stripe].nIndices = data.length;
      graphic.lines[stripe].index    = ctx.createBuffer();
      ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, graphic.lines[stripe].index);
      ctx.bufferData(ctx.ELEMENT_ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    }
    else if (vType == 2)
    {
      if (toType(data) != "Uint8Array") {
        logger(" Line Edit Colors Type = " + toType(data) +
               " should be Uint8Array!");
        return;
      }
      if (graphic.lines[stripe].color != undefined)
      {
        ctx.deleteBuffer(graphic.lines[stripe].color);
        delete graphic.lines[stripe].color;
      }
      graphic.lines[stripe].nVerts = data.length/3; 
      graphic.lines[stripe].color  = ctx.createBuffer();
      ctx.bindBuffer(ctx.ARRAY_BUFFER, graphic.lines[stripe].color);
      ctx.bufferData(ctx.ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    }
    else if ((vType == 3) && (stripe == 0))
    {
      if (toType(data) != "Float32Array") {
        logger(" Line Edit Triangle Type = " + toType(data) +
               " should be Float32Array!");
        return;
      }
      if (graphic.triangles.vertex != undefined) {
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
    if (gType == 0) 
    {
      if (vType != 1)
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
      if (graphic.points[stripe].nIndices != undefined)
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
    else if (gType == 1)
    {
      if (vType != 1)
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
      if (graphic.lines[stripe].nIndices != undefined)
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
    if (vType == 0)
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
    else if (vType == 1)
    {
      if (toType(data) != "Uint16Array") {
        logger(" Triangle Edit Indices Type = " + toType(data) +
               " should be Uint16Array!");
        return;
      }
      if (graphic.triangles[stripe].nIndices != undefined)
      {
        ctx.deleteBuffer(graphic.triangles[stripe].index);
        delete graphic.triangles[stripe].index;
      }
      graphic.triangles[stripe].nIndices = data.length;
      graphic.triangles[stripe].index    = ctx.createBuffer();
      ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, graphic.triangles[stripe].index);
      ctx.bufferData(ctx.ELEMENT_ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    }
    else if (vType == 2)
    {
      if (toType(data) != "Uint8Array") {
        logger(" Triangle Edit Colors Type = " + toType(data) +
               " should be Uint8Array!");
        return;
      }
      if (graphic.triangles[stripe].color != undefined)
      {
        ctx.deleteBuffer(graphic.triangles[stripe].color);
        delete graphic.triangles[stripe].color;
      }
      graphic.triangles[stripe].nVerts = data.length/3;
      graphic.triangles[stripe].color  = ctx.createBuffer();
      ctx.bindBuffer(ctx.ARRAY_BUFFER, graphic.triangles[stripe].color);
      ctx.bufferData(ctx.ARRAY_BUFFER, data, ctx.STATIC_DRAW);
    }
    else if (vType == 3)
    {
      if (toType(data) != "Float32Array") {
        logger(" Triangle Edit Normals Type = " + toType(data) +
               " should be Float32Array!");
        return;
      }
      if (graphic.triangles[stripe].normal != undefined)
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
  if (g.sceneGraph[name] == undefined) 
  {
    logger(" Delete with no gPrim in SceneGraph: name = " + name);
    return;
  }
  var graphic = g.sceneGraph[name];
  var nStripe = graphic.nStrip;
  
  if (graphic.GPtype == 0)
  {
    for (var i = 0; i < nStripe; i++) releaseVBO(ctx, graphic.points[i]);
  }
  else if (graphic.GPtype == 1)
  {
    for (var i = 0; i < nStripe; i++) 
    {
      if (graphic.points[i] != undefined) 
        releaseIndexVBO(ctx, graphic.points[i]);
      releaseVBO(ctx, graphic.lines[i]);
    }
    if (graphic.trianges != undefined)
      releaseVBO(ctx, graphic.triangles);
  }
  else
  {
    for (var i = 0; i < nStripe; i++) 
    {
      if (graphic.points[i] != undefined) 
        releaseIndexVBO(ctx, graphic.points[i]);
      if (graphic.lines[i] != undefined) 
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
  if (g.sceneGraph[name] == undefined) 
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

  if (graphic.GPtype == 0)
  {
    releaseVBO(ctx, graphic.points[stripe]);
    graphic.points[stripe] = undefined;
  }
  else if (graphic.GPtype == 1)
  {
    if (gtype == 0)
    {
      if (graphic.points[stripe] != undefined)
        releaseIndexVBO(ctx, graphic.points[stripe]);
      graphic.points[stripe] = undefined;
    }
    else
    {
      if (graphic.points[stripe] != undefined)
        releaseIndexVBO(ctx, graphic.points[stripe]);
      releaseVBO(ctx, graphic.lines[stripe]);
      graphic.points[stripe] = undefined;
      graphic.lines[stripe]  = undefined;
    }
  }
  else
  {
    if (gtype == 0)
    {
      if (graphic.points[stripe] != undefined)
        releaseIndexVBO(ctx, graphic.points[stripe]);
      graphic.points[stripe] = undefined;
    }
    else if (gtype == 1)
    {
      if (graphic.lines[stripe] != undefined)
        releaseIndexVBO(ctx, graphic.lines[stripe]);
      graphic.lines[stripe] = undefined;
    }
    else
    {
      if (graphic.points[stripe] != undefined)
        releaseIndexVBO(ctx, graphic.points[stripe]);
      if (graphic.lines[stripe]  != undefined)
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
  if (g.sceneGraph[name] == undefined) 
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

  if (graphic.GPtype == 0)
  {
    // points
    if (gType != 0) 
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
    if (gType == 0)
    {
      graphic.points[stripe] = createIndexVBO(ctx, indices);
    }
    else if (gType != 1) 
    {
      logger(" Line newStripe with gType = " + gType + 
             " in gPrim with name = " + name);
    }
    else 
    {
      graphic.lines[stripe] = createVBO(ctx, vertices, colors, indices, undefined);
      if ((stripe == 0) && (normals != undefined))
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
    if (gType == 0)
    {
      graphic.points[stripe] = createIndexVBO(ctx, indices);
    }
    else if (gType == 1) 
    {
      graphic.lines[stripe] = createIndexVBO(ctx, indices);
    }
    else if (gType != 2)
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
