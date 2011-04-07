draw2d.PolygonConnectionDecorator=function(){
};
draw2d.PolygonConnectionDecorator.prototype=new draw2d.ConnectionDecorator();
draw2d.PolygonConnectionDecorator.prototype.paint=function(g){
g.setColor(new draw2d.Color(128,255,255));
g.fillPolygon([3,15,30,15,3],[0,5,0,-5,0]);
g.setColor(new draw2d.Color(128,128,255));
g.setStroke(1);
g.drawPolygon([3,15,30,15,3],[0,5,0,-5,0]);
};
