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
openmdao.Component=function(typepath,name,x,y)
{
  this.typepath = typepath;
  this.name = name;
  this.coordinates = [x,y];
}
