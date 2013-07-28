/*
 *      wv: The Web Viewer
 *
 *              Scene Graph Functions
 *
 *      Copyright 2011-2013, Massachusetts Institute of Technology
 *      Licensed under The GNU Lesser General Public License, version 2.1
 *      See http://www.opensource.org/licenses/lgpl-2.1.php
 *
 */

//
// replace data in a gPrim
//
wv["editGPrim"] = function(ctx, name, stripe, gType, vType, data)
{

  if (wv.sceneGraph[name] == undefined) 
  {
    wv.logger(" Edit with no gPrim in SceneGraph: name = " + name);
    return;
  }
  var graphic = wv.sceneGraph[name];
  if ((stripe >= graphic.nStrip) || (stripe < 0))
  {
    wv.logger(" Edit with Stripe = " + stripe + 
              " in gPrim with name = " + name);
    return;
  }
  
  if (graphic.GPtype == 0)
  {
    // points
    if (gType != 0) 
    {
      wv.logger(" Point Edit with gType = " + gType + 
                " in gPrim with name = " + name);
      return;
    }
    if (vType == 0)
    {
      if (wv.toType(data) != "Float32Array") {
        wv.logger(" Point Edit Vertices Type = " + wv.toType(data) +
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
      if (wv.toType(data) != "Uint16Array") {
        wv.logger(" Point Edit Indices Type = " + wv.toType(data) +
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
      if (wv.toType(data) != "Uint8Array") {
        wv.logger(" Point Edit Colors Type = " + wv.toType(data) +
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
      wv.logger(" Point Edit with vType = " + vType + 
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
        wv.logger(" Line/Point Edit with vType = " + vType + 
                  " in gPrim with name = " + name);
        return;
      }
      if (wv.toType(data) != "Uint16Array") {
        wv.logger(" Line/Point Edit Indices Type = " + wv.toType(data) +
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
      wv.logger(" Line Edit with gType = " + gType + 
                " in gPrim with name = " + name);
      return;
    }
    if (vType == 0)
    {
      if (wv.toType(data) != "Float32Array") {
        wv.logger(" Line Edit Vertices Type = " + wv.toType(data) +
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
      if (wv.toType(data) != "Uint16Array") {
        wv.logger(" Line Edit Indices Type = " + wv.toType(data) +
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
      if (wv.toType(data) != "Uint8Array") {
        wv.logger(" Line Edit Colors Type = " + wv.toType(data) +
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
      if (wv.toType(data) != "Float32Array") {
        wv.logger(" Line Edit Triangle Type = " + wv.toType(data) +
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
      wv.logger(" Line Edit with vType = " + vType + 
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
        wv.logger(" Triangle/Point Edit with vType = " + vType + 
                  " in gPrim with name = " + name);
        return;
      }
      if (wv.toType(data) != "Uint16Array") {
        wv.logger(" Triangle/Point Edit Indices Type = " + wv.toType(data) +
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
        wv.logger(" Triangle/Line Edit with vType = " + vType + 
                  " in gPrim with name = " + name);
        return;
      }
      if (wv.toType(data) != "Uint16Array") {
        wv.logger(" Triangle/Line Edit Indices Type = " + wv.toType(data) +
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
      wv.logger(" Triangle Edit with gType = " + gType + 
                " in gPrim with name = " + name);
      return;
    }
    if (vType == 0)
    {
      if (wv.toType(data) != "Float32Array") {
        wv.logger(" Triangle Edit Vertices Type = " + wv.toType(data) +
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
      if (wv.toType(data) != "Uint16Array") {
        wv.logger(" Triangle Edit Indices Type = " + wv.toType(data) +
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
      if (wv.toType(data) != "Uint8Array") {
        wv.logger(" Triangle Edit Colors Type = " + wv.toType(data) +
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
      if (wv.toType(data) != "Float32Array") {
        wv.logger(" Triangle Edit Normals Type = " + wv.toType(data) +
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
      wv.logger(" Triangle Edit with vType = " + vType + 
                " in gPrim with name = " + name);
      return;
    }
  }
}


//
// Delete a gPrim
//
wv["deleteGPrim"] = function(ctx, name)
{
  if (wv.sceneGraph[name] == undefined) 
  {
    wv.logger(" Delete with no gPrim in SceneGraph: name = " + name);
    return;
  }
  var graphic = wv.sceneGraph[name];
  var nStripe = graphic.nStrip;
  
  if (graphic.GPtype == 0)
  {
    for (var i = 0; i < nStripe; i++) wv.releaseVBO(ctx, graphic.points[i]);
  }
  else if (graphic.GPtype == 1)
  {
    for (var i = 0; i < nStripe; i++) 
    {
      if (graphic.points[i] != undefined) 
        wv.releaseIndexVBO(ctx, graphic.points[i]);
      wv.releaseVBO(ctx, graphic.lines[i]);
    }
    if (graphic.trianges != undefined)
      wv.releaseVBO(ctx, graphic.triangles);
  }
  else
  {
    for (var i = 0; i < nStripe; i++) 
    {
      if (graphic.points[i] != undefined) 
        wv.releaseIndexVBO(ctx, graphic.points[i]);
      if (graphic.lines[i] != undefined) 
        wv.releaseIndexVBO(ctx, graphic.lines[i]);
      wv.releaseVBO(ctx, graphic.triangles[i]);
    }

  }
  
}


//
// Delete a stripe
//
wv["deleteStripe"] = function(ctx, name, stripe, gtype)
{
  if (wv.sceneGraph[name] == undefined) 
  {
    wv.logger(" deleteStripe with no gPrim in SceneGraph: name = " + name);
    return;
  }
  var graphic = wv.sceneGraph[name];
  if ((stripe >= graphic.nStrip) || (stripe < 0))
  {
    wv.logger(" deleteStripe in SceneGraph: name = " + name +
              "  stripe = " + stripe);
    return;
  }

  if (graphic.GPtype == 0)
  {
    wv.releaseVBO(ctx, graphic.points[stripe]);
    graphic.points[stripe] = undefined;
  }
  else if (graphic.GPtype == 1)
  {
    if (gtype == 0)
    {
      if (graphic.points[stripe] != undefined)
        wv.releaseIndexVBO(ctx, graphic.points[stripe]);
      graphic.points[stripe] = undefined;
    }
    else
    {
      if (graphic.points[stripe] != undefined)
        wv.releaseIndexVBO(ctx, graphic.points[stripe]);
      wv.releaseVBO(ctx, graphic.lines[stripe]);
      graphic.points[stripe] = undefined;
      graphic.lines[stripe]  = undefined;
    }
  }
  else
  {
    if (gtype == 0)
    {
      if (graphic.points[stripe] != undefined)
        wv.releaseIndexVBO(ctx, graphic.points[stripe]);
      graphic.points[stripe] = undefined;
    }
    else if (gtype == 1)
    {
      if (graphic.lines[stripe] != undefined)
        wv.releaseIndexVBO(ctx, graphic.lines[stripe]);
      graphic.lines[stripe] = undefined;
    }
    else
    {
      if (graphic.points[stripe] != undefined)
        wv.releaseIndexVBO(ctx, graphic.points[stripe]);
      if (graphic.lines[stripe]  != undefined)
        wv.releaseIndexVBO(ctx, graphic.lines[stripe]);
      wv.releaseVBO(ctx, graphic.triangles[stripe]);
      graphic.points[stripe]    = undefined;
      graphic.lines[stripe]     = undefined;
      graphic.triangles[stripe] = undefined;
    }
  }

}


//
// Fill in a stripe for a new gPrim
//
wv["newStripe"] = function(ctx, name, stripe, gType, vertices, colors, indices,
                           normals)
{
  if (wv.sceneGraph[name] == undefined) 
  {
    wv.logger(" newStripe with no gPrim in SceneGraph: name = " + name);
    return;
  }
  var graphic = wv.sceneGraph[name];
  if ((stripe >= graphic.nStrip) || (stripe < 0))
  {
    wv.logger(" newStripe with Stripe = " + stripe + 
              " in gPrim with name = " + name);
    return;
  }

  if (graphic.GPtype == 0)
  {
    // points
    if (gType != 0) 
    {
      wv.logger(" Point newStripe with gType = " + gType + 
                " in gPrim with name = " + name);
      return;
    }
    graphic.points[stripe] = wv.createVBO(ctx, vertices, colors, indices,
                                          undefined);
  }
  
  else if (graphic.GPtype == 1)
  {
    // lines
    if (gType == 0)
    {
      graphic.points[stripe] = wv.createIndexVBO(ctx, indices);
    }
    else if (gType != 1) 
    {
      wv.logger(" Line newStripe with gType = " + gType + 
                " in gPrim with name = " + name);
    }
    else 
    {
      graphic.lines[stripe] = wv.createVBO(ctx, vertices, colors, indices,
                                           undefined);
      if ((stripe == 0) && (normals != undefined))
      {
        var len  = normals.length/2;
        var vert = normals.subarray(0, len);
        var norm = normals.subarray(len);
        graphic.triangles = wv.createVBO(ctx, vert, undefined, undefined, norm);
      }
    }
  }

  else
  {
    // triangles
    if (gType == 0)
    {
      graphic.points[stripe] = wv.createIndexVBO(ctx, indices);
    }
    else if (gType == 1) 
    {
      graphic.lines[stripe] = wv.createIndexVBO(ctx, indices);
    }
    else if (gType != 2)
    {
      wv.logger(" Triangle newStripe with gType = " + gType + 
                " in gPrim with name = " + name);
      return;
    }
    else
    {
      graphic.triangles[stripe] = wv.createVBO(ctx, vertices, colors, indices,
                                               normals);
    }
  }

}
