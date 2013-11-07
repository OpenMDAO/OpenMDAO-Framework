/*
 *      wv: The Web Viewer
 *
 *      	Draw Scene Graph functions
 *
 *      Copyright 2011-2013, Massachusetts Institute of Technology
 *      Licensed under The GNU Lesser General Public License, version 2.1
 *      See http://www.opensource.org/licenses/lgpl-2.1.php
 *
 */


// do GPtype == 0
wv["plotPoints"] = function(gl, graphic)
{

  gl.uniform1f(wv.u_wLightLoc, 0.0);            // no lighting
  gl.uniform3f(wv.u_conColorLoc,  graphic.pColor[0],
               graphic.pColor[1], graphic.pColor[2]);
  gl.uniform1f(wv.u_pointSizeLoc, graphic.pSize);

  gl.disableVertexAttribArray(2);
  for (var i = 0; i < graphic.nStrip; i++)
  {
    var vbo = graphic.points[i];
    if (wv.vbonum != 0) 
    {
      gl.uniform1i(wv.u_vbonumLoc, wv.vbonum);
      wv.vbonum++;
    }

    gl.disableVertexAttribArray(1);
    gl.bindBuffer(gl.ARRAY_BUFFER, vbo.vertex);
    gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(0);
    
    if (((graphic.attrs & wv.plotAttrs.SHADING) != 0) &&
        (vbo.color != undefined))
    {
      gl.bindBuffer(gl.ARRAY_BUFFER, vbo.color);
      gl.vertexAttribPointer(1, 3, gl.UNSIGNED_BYTE, false, 0, 0);
      gl.enableVertexAttribArray(1);
    } else {
      gl.uniform1f(wv.u_wColorLoc, 0.0);
    }

    if (vbo.index == undefined)
    {
      gl.drawArrays(gl.POINTS, 0, vbo.nVerts);
    } else {
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, vbo.index);
      gl.drawElements(gl.POINTS, vbo.nIndices, gl.UNSIGNED_SHORT, 0);
    }
    wv.checkGLError(gl, "plotPoints - after draw on points");
    gl.uniform1f(wv.u_wColorLoc, 1.0);
  }
  gl.uniform1f(wv.u_wLightLoc, 1.0);
}


// do GPtype == 1
wv["plotLines"] = function(gl, graphic)
{
  gl.uniform1f(wv.u_wLightLoc, 0.0);            // no lighting

  //
  // do the lines
  //
  if ((graphic.attrs & wv.plotAttrs.ON) != 0)
  {
    gl.uniform1f(wv.u_linAdjLoc, wv.lineBump*(wv.zFar-wv.zNear)/wv.scale);
    gl.uniform3f(wv.u_conColorLoc,  graphic.lColor[0],
                 graphic.lColor[1], graphic.lColor[2]);
    gl.lineWidth(graphic.lWidth);
    gl.disableVertexAttribArray(2);
    for (var i = 0; i < graphic.nStrip; i++)
    {
      var vbo = graphic.lines[i];
      if (wv.vbonum != 0) 
      {
        gl.uniform1i(wv.u_vbonumLoc, wv.vbonum);
        wv.vbonum++;
      }

      gl.disableVertexAttribArray(1);
      gl.bindBuffer(gl.ARRAY_BUFFER, vbo.vertex);
      gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 0, 0);
      gl.enableVertexAttribArray(0);

      if (((graphic.attrs & wv.plotAttrs.SHADING) != 0) &&
          (vbo.color != undefined))
      {
        gl.bindBuffer(gl.ARRAY_BUFFER, vbo.color);
        gl.vertexAttribPointer(1, 3, gl.UNSIGNED_BYTE, false, 0, 0);
        gl.enableVertexAttribArray(1);
      } else {
        gl.uniform1f(wv.u_wColorLoc, 0.0);
      }

      if (vbo.index == undefined)
      {
        gl.drawArrays(gl.LINES, 0, vbo.nVerts);
      } else {
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, vbo.index);
        gl.drawElements(gl.LINES, vbo.nIndices, gl.UNSIGNED_SHORT, 0);
      }
      wv.checkGLError(gl, "plotLines - after draw on lines");
      gl.uniform1f(wv.u_wColorLoc, 1.0);
    }
    gl.uniform1f(wv.u_linAdjLoc, 0.0);
  }

  //
  // do the points
  //
  if ((graphic.attrs & wv.plotAttrs.POINTS) != 0)
  {
    gl.uniform1f(wv.u_wColorLoc, 0.0);
    gl.uniform3f(wv.u_conColorLoc,  graphic.pColor[0],
                 graphic.pColor[1], graphic.pColor[2]);
    gl.uniform1f(wv.u_pointSizeLoc,  graphic.pSize);
    gl.disableVertexAttribArray(2);
    gl.disableVertexAttribArray(1);
    for (var i = 0; i < graphic.nStrip; i++)
    {
      var vbop = graphic.points[i];
      if (vbop == undefined) continue;
      if (wv.vbonum != 0) 
      {
        gl.uniform1i(wv.u_vbonumLoc, wv.vbonum);
        wv.vbonum++;
      }
      var vbol = graphic.lines[i];
      gl.bindBuffer(gl.ARRAY_BUFFER, vbol.vertex);
      gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 0, 0);
      gl.enableVertexAttribArray(0);
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, vbop.index);
      gl.drawElements(gl.POINTS, vbop.nIndices, gl.UNSIGNED_SHORT, 0);
      wv.checkGLError(gl, "plotLines - after draw on points");
    }
  }

  gl.uniform1f(wv.u_wLightLoc, 1.0);
  gl.uniform1f(wv.u_wColorLoc, 1.0);

  //
  // do the decorations (a single stripe of triangles)
  //
  if ((graphic.attrs & wv.plotAttrs.ORIENTATION) != 0)
    if (graphic.triangles != undefined) 
    {
      gl.uniform3f(wv.u_conColorLoc,  graphic.fColor[0],
                   graphic.fColor[1], graphic.fColor[2]);
      gl.uniform3f(wv.u_bacColorLoc,  graphic.bColor[0],
                   graphic.bColor[1], graphic.bColor[2]);
      gl.uniform1f(wv.u_bColorLoc, 1.0);

      gl.disableVertexAttribArray(1);
      gl.bindBuffer(gl.ARRAY_BUFFER, graphic.triangles.vertex);
      gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 0, 0);
      gl.enableVertexAttribArray(0);
      gl.bindBuffer(gl.ARRAY_BUFFER, graphic.triangles.normal);
      gl.vertexAttribPointer(2, 3, gl.FLOAT, false, 0, 0);
      gl.enableVertexAttribArray(2);
      gl.uniform1f(wv.u_wColorLoc, 0.0);
      gl.drawArrays(gl.TRIANGLES, 0, graphic.triangles.nVerts);
      wv.checkGLError(gl, "plotLines - after draw on tris");
      gl.uniform1f(wv.u_wColorLoc, 1.0);

      gl.uniform1f(wv.u_bColorLoc, 0.0);
    }

}


// do GPtype == 2
wv["plotTriangles"] = function(gl, graphic)
{

  //
  // do the triangles first
  //
  if ((graphic.attrs & wv.plotAttrs.ON) != 0)
  {
    gl.uniform3f(wv.u_conNormalLoc, graphic.normal[0],
                 graphic.normal[1], graphic.normal[2]);
    gl.uniform3f(wv.u_conColorLoc,  graphic.fColor[0],
                 graphic.fColor[1], graphic.fColor[2]);
    if ((graphic.attrs & wv.plotAttrs.ORIENTATION) != 0) 
    {
      gl.uniform3f(wv.u_bacColorLoc,  graphic.bColor[0],
                   graphic.bColor[1], graphic.bColor[2]);
      gl.uniform1f(wv.u_bColorLoc, 1.0);
    }

    for (var i = 0; i < graphic.nStrip; i++)
    {
      var vbo = graphic.triangles[i];
    
      if (wv.vbonum != 0) 
      {
        gl.uniform1i(wv.u_vbonumLoc, wv.vbonum);
        wv.vbonum++;
      }

      gl.disableVertexAttribArray(2);
      gl.disableVertexAttribArray(1);
      gl.bindBuffer(gl.ARRAY_BUFFER, vbo.vertex);
      gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 0, 0);
      gl.enableVertexAttribArray(0);

      if (vbo.normal == undefined) 
      {
        gl.uniform1f(wv.u_wNormalLoc, 0.0);
      } else {
        gl.bindBuffer(gl.ARRAY_BUFFER, vbo.normal);
        gl.vertexAttribPointer(2, 3, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(2);
      }

      if (((graphic.attrs & wv.plotAttrs.SHADING) != 0) &&
          (vbo.color != undefined))
      {
        gl.bindBuffer(gl.ARRAY_BUFFER, vbo.color);
        gl.vertexAttribPointer(1, 3, gl.UNSIGNED_BYTE, false, 0, 0);
        gl.enableVertexAttribArray(1);
      } else {
        gl.uniform1f(wv.u_wColorLoc, 0.0);
      }

      if (vbo.index == undefined)
      {
        gl.drawArrays(gl.TRIANGLES, 0, vbo.nVerts);
      } else {
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, vbo.index);
        gl.drawElements(gl.TRIANGLES, vbo.nIndices, gl.UNSIGNED_SHORT, 0);
      }
      wv.checkGLError(gl, "plotTriangles - after draw on tris");
      if (vbo.normal == undefined) gl.uniform1f(wv.u_wNormalLoc, 1.0);
      gl.uniform1f(wv.u_wColorLoc, 1.0);
    }

    if ((graphic.attrs & wv.plotAttrs.ORIENTATION) != 0) 
    {
      gl.uniform1f(wv.u_bColorLoc, 0.0);
    }
  }

  //
  // do the lines
  //
  if ((graphic.attrs & wv.plotAttrs.LINES) != 0)
  {
    gl.uniform1f(wv.u_wLightLoc, 0.0);
    gl.uniform1f(wv.u_wColorLoc, 0.0);
    gl.uniform3f(wv.u_conColorLoc,  graphic.lColor[0],
                 graphic.lColor[1], graphic.lColor[2]);
    gl.uniform1f(wv.u_linAdjLoc, wv.lineBump*(wv.zFar-wv.zNear)/wv.scale);
    gl.lineWidth(graphic.lWidth);
    gl.disableVertexAttribArray(2);
    gl.disableVertexAttribArray(1);
    for (var i = 0; i < graphic.nStrip; i++)
    {
      var vbol = graphic.lines[i];
      if (vbol == undefined) continue;
      if (wv.vbonum != 0) 
      {
        gl.uniform1i(wv.u_vbonumLoc, wv.vbonum);
        wv.vbonum++;
      }
      var vbot = graphic.triangles[i];
      gl.bindBuffer(gl.ARRAY_BUFFER, vbot.vertex);
      gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 0, 0);
      gl.enableVertexAttribArray(0);
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, vbol.index);
      gl.drawElements(gl.LINES, vbol.nIndices, gl.UNSIGNED_SHORT, 0);
      wv.checkGLError(gl, "plotTriangles - after draw on lines");
    }
    gl.uniform1f(wv.u_linAdjLoc, 0.0);
    gl.uniform1f(wv.u_wLightLoc, 1.0);
    gl.uniform1f(wv.u_wColorLoc, 1.0);
  }

  //
  // do the points
  //
  if ((graphic.attrs & wv.plotAttrs.POINTS) != 0)
  {
    gl.uniform1f(wv.u_wLightLoc, 0.0);
    gl.uniform1f(wv.u_wColorLoc, 0.0);
    gl.uniform3f(wv.u_conColorLoc,  graphic.pColor[0],
                 graphic.pColor[1], graphic.pColor[2]);
    gl.uniform1f(wv.u_pointSizeLoc, graphic.pSize);
    gl.disableVertexAttribArray(2);
    gl.disableVertexAttribArray(1);
    for (var i = 0; i < graphic.nStrip; i++)
    {
      var vbop = graphic.points[i];
      if (vbop == undefined) continue;
      if (wv.vbonum != 0) 
      {
        gl.uniform1i(wv.u_vbonumLoc, wv.vbonum);
        wv.vbonum++;
      }
      var vbot = graphic.triangles[i];
      gl.bindBuffer(gl.ARRAY_BUFFER, vbot.vertex);
      gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 0, 0);
      gl.enableVertexAttribArray(0);
      gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, vbop.index);
      gl.drawElements(gl.POINTS, vbop.nIndices, gl.UNSIGNED_SHORT, 0);
      wv.checkGLError(gl, "plotTriangles - after draw on points");
    }
    gl.uniform1f(wv.u_wLightLoc, 1.0);
    gl.uniform1f(wv.u_wColorLoc, 1.0);
  }

}


//
// traverses the Scene Graph
wv["traverseSG"] = function(gl, xpar)
{
  var nOther = 0;

  if (xpar == -1) {
    gl.uniform1i(wv.u_pickingLoc, 1);
    wv.vbonum = 1;
    for (var gprim in wv.sceneGraph)
    {
      var graphic = wv.sceneGraph[gprim];
      switch (graphic.GPtype) {
        case 0:
          if ((graphic.attrs & wv.plotAttrs.ON) == 0) break;
          wv.plotPoints(gl, graphic);
          break;
        case 1:
          if (((graphic.attrs & wv.plotAttrs.ON)     == 0) &&
              ((graphic.attrs & wv.plotAttrs.POINTS) == 0)) break;
          wv.plotLines(gl, graphic);
          break;
        case 2:
          if (((graphic.attrs & wv.plotAttrs.ON)     == 0) &&
              ((graphic.attrs & wv.plotAttrs.LINES)  == 0) &&
              ((graphic.attrs & wv.plotAttrs.POINTS) == 0)) break;
          wv.plotTriangles(gl, graphic);
          break;
        default:
          break;
      }
    }
    gl.uniform1i(wv.u_pickingLoc, 0);
    wv.vbonum = 0;
    return 0;
  }

  for (var gprim in wv.sceneGraph)
  {
    var graphic = wv.sceneGraph[gprim];
    switch (graphic.GPtype) {
      case 0:
        if ((graphic.attrs & wv.plotAttrs.ON) == 0) break;
        if ((graphic.attrs & wv.plotAttrs.TRANSPARENT) != xpar)
        {
          nOther++;
          break;
        }
        wv.plotPoints(gl, graphic);
        break;
      case 1:
        if (((graphic.attrs & wv.plotAttrs.ON)     == 0) &&
            ((graphic.attrs & wv.plotAttrs.POINTS) == 0)) break;
        if  ((graphic.attrs & wv.plotAttrs.TRANSPARENT) != xpar)
        {
          nOther++;
          break;
        }
        wv.plotLines(gl, graphic);
        break;
      case 2:
        if (((graphic.attrs & wv.plotAttrs.ON)     == 0) &&
            ((graphic.attrs & wv.plotAttrs.LINES)  == 0) &&
            ((graphic.attrs & wv.plotAttrs.POINTS) == 0)) break;
        if  ((graphic.attrs & wv.plotAttrs.TRANSPARENT) != xpar)
        {
          nOther++;
          break;
        }
        wv.plotTriangles(gl, graphic);
        break;
      default:
        break;
    }
  }

  return nOther;
}


//
// get picked object from vbo number
wv["getPickedObject"] = function(vbonum)
{
  var num = 1;
  for (var gprim in wv.sceneGraph)
  {
    var graphic = wv.sceneGraph[gprim];
    switch (graphic.GPtype) {
      case 0:
        if ((graphic.attrs & wv.plotAttrs.ON) == 0) break;
        for (var i = 0; i < graphic.nStrip; i++)
        {
          if (num == vbonum) {
            var picked   = {};
            picked.gprim = gprim;
            picked.strip = i;
            picked.type  = 0;
            wv.picked    = picked;
            return;
          }
          num++;
        }
        break;
      case 1:
        if (((graphic.attrs & wv.plotAttrs.ON)     == 0) &&
            ((graphic.attrs & wv.plotAttrs.POINTS) == 0)) break;
        if  ((graphic.attrs & wv.plotAttrs.ON) != 0) {
          for (var i = 0; i < graphic.nStrip; i++)
          {
            if (num == vbonum) {
              var picked   = {};
              picked.gprim = gprim;
              picked.strip = i;
              picked.type  = 1;
              wv.picked    = picked;
              return;
            }
            num++;
          }
        }
        if ((graphic.attrs & wv.plotAttrs.POINTS) != 0)
        {
          for (var i = 0; i < graphic.nStrip; i++)
          {
            var vbop = graphic.points[i];
            if (vbop == undefined) continue;
            if (num == vbonum) {
              var picked   = {};
              picked.gprim = gprim;
              picked.strip = i;
              picked.type  = 0;
              wv.picked    = picked;
              return;
            }
            num++;
          }
        }
        break;
      case 2:
        if (((graphic.attrs & wv.plotAttrs.ON)     == 0) &&
            ((graphic.attrs & wv.plotAttrs.LINES)  == 0) &&
            ((graphic.attrs & wv.plotAttrs.POINTS) == 0)) break;
        if  ((graphic.attrs & wv.plotAttrs.ON) != 0) {
          for (var i = 0; i < graphic.nStrip; i++)
          {
            if (num == vbonum) {
              var picked   = {};
              picked.gprim = gprim;
              picked.strip = i;
              picked.type  = 2;
              wv.picked    = picked;
              return;
            }
            num++;
          }
        }
        if ((graphic.attrs & wv.plotAttrs.LINES) != 0)
        {
          for (var i = 0; i < graphic.nStrip; i++)
          {
            var vbol = graphic.lines[i];
            if (vbol == undefined) continue;
            if (num == vbonum) {
              var picked   = {};
              picked.gprim = gprim;
              picked.strip = i;
              picked.type  = 1;
              wv.picked    = picked;
              return;
            }
            num++;
          }
        }
        if ((graphic.attrs & wv.plotAttrs.POINTS) != 0)
        {
          for (var i = 0; i < graphic.nStrip; i++)
          {
            var vbop = graphic.points[i];
            if (vbop == undefined) continue;
            if (num == vbonum) {
              var picked   = {};
              picked.gprim = gprim;
              picked.strip = i;
              picked.type  = 0;
              wv.picked    = picked;
              return;
            }
            num++;
          }
        }
        break;
      default:
        break;
    }
  }
}


//
// setup perspective for drawing
//
wv["InitDraw"] = function()
{
  wv.perspectiveMatrix.perspective(wv.fov, wv.width/wv.height,
                                   wv.zNear, wv.zFar);
  wv.perspectiveMatrix.lookat(wv.eye[0],    wv.eye[1],    wv.eye[2], 
                              wv.center[0], wv.center[1], wv.center[2], 
                              wv.up[0],     wv.up[1],     wv.up[2]);
}


//
// draws the scene using the globals "g"
wv["drawPicture"] = function(gl)
{
  // Make sure the canvas is sized correctly.
  wv.Reshape(gl);

  // Make a model/view matrix and pass it in
  wv.UpdateView();
  if ((wv.sceneUpd == 0) && (wv.pick == 0) && (wv.locate == 0) &&
      (wv.centerV  == 0)) return;
  wv.mvMatrix.setUniform(gl, wv.u_modelViewMatrixLoc, false);

  // Construct the normal matrix from the model-view matrix and pass it in
  wv.normalMatrix.load(wv.mvMatrix);
  wv.normalMatrix.scale(1.0/wv.scale, 1.0/wv.scale, 1.0/wv.scale);
  wv.normalMatrix.invert();
  wv.normalMatrix.transpose();
  wv.normalMatrix.setUniform(gl, wv.u_normalMatrixLoc, false);

  // Construct the model-view * projection matrix and pass it in
  wv.mvpMatrix.load(wv.perspectiveMatrix);
  wv.mvpMatrix.scale(1.0, 1.0, 1.0/wv.scale);
  wv.mvpMatrix.multiply(wv.mvMatrix);
  wv.mvpMatrix.setUniform(gl, wv.u_modelViewProjMatrixLoc, false);
  
  // the view needs to be centered or location is on
  wv.located = undefined;
  if ((wv.centerV != 0) || (wv.locate != 0))
  {    
    // get the model coordinates by screen coordinates
    var scrX      = 2.0*wv.cursorX/(wv.width -1.0) - 1.0;
    var scrY      = 2.0*wv.cursorY/(wv.height-1.0) - 1.0;
    var scrCoords = [scrX, scrY, 0.0, 0.0];
    var buf = new Uint8Array(4);
    // get z only -- can get screen x and y and 1/w is not needed
    for (var i = 2; i < 3; i++) {
      // Clear the canvas
      gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
      gl.clearColor(0.0, 0.0, 0.0, 0.0);
      gl.uniform1i(wv.u_pickingLoc, -i-1);
      for (var gprim in wv.sceneGraph)
      {
        var graphic = wv.sceneGraph[gprim];
        switch (graphic.GPtype) {
          case 0:
            if ((graphic.attrs & wv.plotAttrs.ON) == 0) break;
            wv.plotPoints(gl, graphic);
            break;
          case 1:
            if (((graphic.attrs & wv.plotAttrs.ON)     == 0) &&
                ((graphic.attrs & wv.plotAttrs.POINTS) == 0)) break;
            wv.plotLines(gl, graphic);
            break;
          case 2:
            if (((graphic.attrs & wv.plotAttrs.ON)     == 0) &&
                ((graphic.attrs & wv.plotAttrs.LINES)  == 0) &&
                ((graphic.attrs & wv.plotAttrs.POINTS) == 0)) break;
            wv.plotTriangles(gl, graphic);
            break;
          default:
            break;
        }
      }
      gl.readPixels(wv.cursorX, wv.cursorY, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, buf);
      if (buf[3] == 0) 
      {
        if (wv.centerV != 0) alert(" Center: Nothing touched!");
        break;
      } else {
        // may be losing precision -- fixed on 7.24 bits
        var pmsign = 0;
        if (buf[0] >= 128)
        {
          pmsign  = 1;
          buf[0] -= 128;
        }
        scrCoords[i] = buf[0] + buf[1]/256.0 + buf[2]/(256.0*256.0) +
                       buf[3]/(256.0*256.0*256.0);
        if (i < 2) scrCoords[i] *= 100.0;
        if (pmsign == 1) scrCoords[i] = -scrCoords[i];
      }
    }
    gl.uniform1i(wv.u_pickingLoc, 0);
    
    if (buf[3] != 0)
    {
      var mat = new J3DIMatrix4();
      mat.load(wv.mvpMatrix);
      mat.invert();
      var vec = new J3DIVector3(scrCoords[0], scrCoords[1], scrCoords[2]);
      vec.multVecMatrix(mat);
      var coords = vec.getAsFloat32Array();
      if (wv.locate  != 0) wv.located = coords;
      if (wv.centerV != 0)
      {
//      jack(gl, coords[0], coords[1], coords[2], 0.1);   // simpleUI debug
        var mVal = wv.mvMatrix.getAsArray();
        mVal[12] = -(mVal[0]*coords[0] + mVal[4]*coords[1] + mVal[ 8]*coords[2]);
        mVal[13] = -(mVal[1]*coords[0] + mVal[5]*coords[1] + mVal[ 9]*coords[2]);
        mVal[14] = -(mVal[2]*coords[0] + mVal[6]*coords[1] + mVal[10]*coords[2]);
        wv.mvMatrix.load(mVal);
/*
        wv.zNear    += coords[2] - wv.center[2];
        wv.zFar     += coords[2] - wv.center[2];
        wv.eye[0]   += coords[0] - wv.center[0];
        wv.eye[1]   += coords[1] - wv.center[1];
        wv.eye[2]   += coords[2] - wv.center[2];
        wv.center[0] = coords[0];
        wv.center[1] = coords[1];
        wv.center[2] = coords[2];
        wv.perspectiveMatrix.makeIdentity();
        wv.perspectiveMatrix.perspective(wv.fov, wv.width/wv.height, 
                                         wv.zNear, wv.zFar);
        wv.perspectiveMatrix.lookat(wv.eye[0],    wv.eye[1],    wv.eye[2], 
                                    wv.center[0], wv.center[1], wv.center[2], 
                                    wv.up[0],     wv.up[1],     wv.up[2]);
*/
      }
    }

    if (wv.centerV != 0)
    {
      wv.sceneUpd = 1;
      wv.centerV  = 0;
      return;
    }
  }

  // Draw the scene for picking
  if (wv.pick != 0)
  {
    // Clear the canvas
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    gl.clearColor(0.0, 0.0, 0.0, 0.0);
    wv.traverseSG(gl, -1);
    
    buf = new Uint8Array(4);
    gl.readPixels(wv.cursorX, wv.cursorY, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, buf);
    wv.picked = undefined;
    if ((buf[0] != 0) || (buf[1] != 0))
    {
      var numvbo = buf[0];
      numvbo = numvbo*256 + buf[1];
      var primID = buf[2];
      primID = primID*256 + buf[3];
      wv.getPickedObject(numvbo);
      if (wv.picked != undefined) wv.picked.primID = primID;
    }
  } else {
    wv.picked   = undefined;
    wv.sceneUpd = 0;
  }
  
  // Draw the scene

  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
  if (wv.traverseSG(gl, 0) != 0)
  {
    gl.uniform1f(wv.u_xparLoc, 0.5);
    gl.enable(gl.BLEND);
//  gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE);
    wv.traverseSG(gl, wv.plotAttrs.TRANSPARENT);
    gl.uniform1f(wv.u_xparLoc, 1.0);
    gl.disable(gl.BLEND);
  }
  
  // allow for custom drawing

  wv.UpdateCanvas(gl);

}

