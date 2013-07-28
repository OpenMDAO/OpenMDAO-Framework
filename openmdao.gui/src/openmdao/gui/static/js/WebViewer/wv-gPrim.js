/*
 *      wv: The Web Viewer
 *
 *              Graphics Primitives
 *
 *      Copyright 2011-2013, Massachusetts Institute of Technology
 *      Licensed under The GNU Lesser General Public License, version 2.1
 *      See http://www.opensource.org/licenses/lgpl-2.1.php
 *
 */


//
// returns the type of an object
//
wv["toType"] = function(obj)
{ 
  return ({}).toString.call(obj).match(/\s([a-zA-Z0-9]+)/)[1];
}


//
// creates an Index-only VBO
//
wv["createIndexVBO"] = function(ctx, indices)
{
  var returned = { };

  if (wv.toType(indices) != "Uint16Array") {
    wv.logger(" Indices Type = " + wv.toType(indices) +
              " should be Uint16Array (createIndexVBO)!");
    return undefined;
  }
  returned.nIndices = indices.length;
  returned.index    = ctx.createBuffer();
  ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, returned.index);
  ctx.bufferData(ctx.ELEMENT_ARRAY_BUFFER, indices, ctx.STATIC_DRAW);
  
  return returned;
}


//
// creates the full VBO 
//
wv["createVBO"] = function(ctx, vertices, colors, indices, normals)
{
  var returned = { };

  if (wv.toType(vertices) != "Float32Array") {
    wv.logger(" Vertices Type = " + wv.toType(vertices) +
              " should be Float32Array (createVBO)!");
    return undefined;
  }

  returned.nVerts   = vertices.length/3;
  returned.nIndices = 0;
  returned.vertex   = ctx.createBuffer();
  ctx.bindBuffer(ctx.ARRAY_BUFFER, returned.vertex);
  ctx.bufferData(ctx.ARRAY_BUFFER, vertices, ctx.STATIC_DRAW);

  if (colors != undefined) {
    if (returned.nVerts != colors.length/3) {
      wv.logger(" Mismatch with Colors: nVerts = " + returned.nVerts +
                " nColors = " + colors.length/3 + " (createVBO)!");
      return undefined;
    }
    if (wv.toType(colors) != "Uint8Array") {
      wv.logger(" Colors Type = " + wv.toType(colors) +
                " should be Uint8Array (createVBO)!");
      return undefined;
    }
    returned.color  = ctx.createBuffer();
    ctx.bindBuffer(ctx.ARRAY_BUFFER, returned.color);
    ctx.bufferData(ctx.ARRAY_BUFFER, colors, ctx.STATIC_DRAW);
  }

  if (normals != undefined) {
    if (returned.nVerts != normals.length/3) {
      wv.logger(" Mismatch with Normals: nVerts = " + returned.nVerts +
                " nNormals = " + normals.length/3 + " (createVBO)!");
      return undefined;
    }
    if (wv.toType(normals) != "Float32Array") {
      wv.logger(" Normals Type = " + wv.toType(normals) +
                " should be Float32Array (createVBO)!");
      return undefined;
    }
    returned.normal = ctx.createBuffer();
    ctx.bindBuffer(ctx.ARRAY_BUFFER, returned.normal);
    ctx.bufferData(ctx.ARRAY_BUFFER, normals, ctx.STATIC_DRAW);
  }

  if (indices != undefined) {
    if (wv.toType(indices) != "Uint16Array") {
      wv.logger(" Indices Type = " + wv.toType(indices) +
                " should be Uint16Array (createVBO)!");
      return undefined;
    }
    returned.nIndices = indices.length;
    returned.index    = ctx.createBuffer();
    ctx.bindBuffer(ctx.ELEMENT_ARRAY_BUFFER, returned.index);
    ctx.bufferData(ctx.ELEMENT_ARRAY_BUFFER, indices, ctx.STATIC_DRAW);
  }

  return returned;
}


//
// release an Indexed VBO
//
wv["releaseIndexVBO"] = function(ctx, vbo)
{
  ctx.deleteBuffer(vbo.index);
  delete vbo.index;
}


//
// release the VBO object
//
wv["releaseVBO"] = function(ctx, vbo)
{
  ctx.deleteBuffer(vbo.vertex);
  delete vbo.nVerts;
  delete vbo.vertex;

  if (vbo.color != undefined) {
    ctx.deleteBuffer(vbo.color);
    delete vbo.color;
  }

  if (vbo.normal != undefined) {
    ctx.deleteBuffer(vbo.normal);
    delete vbo.normal;
  }

  if (vbo.index != undefined) {
    ctx.deleteBuffer(vbo.index);
    delete vbo.index;
  }
  delete vbo.nIndices;
}


//
// creates a Graphic Primitive object
//
wv["createGPrim"] = function(type, nStripe, attrs)
{
  var returned = { };
  
  returned.GPtype = type;
  returned.nStrip = nStripe;
  returned.attrs  = attrs;
  returned.pSize  = 3;
  returned.pColor = [1.0, 1.0, 1.0];
  switch (type) {
    case 0:
      returned.points = [];
      for (var i = 0; i < nStripe; i++) {
        returned.points[i] = undefined;
      }
      break;
    case 1:
      returned.lWidth    = 1;
      returned.lColor    = [1.0, 1.0, 1.0];
      returned.fColor    = [1.0, 0.0, 0.0];
      returned.bColor    = [0.5, 0.5, 0.5];
      returned.points    = [];
      returned.lines     = [];
      returned.triangles = undefined;
      for (var i = 0; i < nStripe; i++) {
        returned.points[i] = undefined;
        returned.lines[i]  = undefined;
      }
      break;
    case 2:
      returned.lWidth    = 1;
      returned.lColor    = [1.0, 1.0, 1.0];
      returned.fColor    = [1.0, 0.0, 0.0];
      returned.bColor    = [0.5, 0.5, 0.5];
      returned.normal    = [1.0, 0.0, 0.0];
      returned.points    = [];
      returned.lines     = [];
      returned.triangles = [];
      for (var i = 0; i < nStripe; i++) {
        returned.points[i]    = undefined;
        returned.lines[i]     = undefined;
        returned.triangles[i] = undefined;
      }
      break;
    default:
      wv.logger(" createGPrim invoked with type = " + type + "!");
      return undefined;
  }

  return returned;
}
