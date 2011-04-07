draw2d.CrossConnectionDecorator=function(){
};
draw2d.CrossConnectionDecorator.prototype=new draw2d.ConnectionDecorator();
draw2d.CrossConnectionDecorator.prototype.paint=function(g){
g.drawLine(15,8,15,-8);
};
