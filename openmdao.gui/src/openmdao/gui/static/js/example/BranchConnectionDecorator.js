draw2d.BranchConnectionDecorator=function(){
};
draw2d.BranchConnectionDecorator.prototype=new draw2d.ConnectionDecorator();
draw2d.BranchConnectionDecorator.prototype.paint=function(g){
g.drawLine(0,-8,15,0);
g.drawLine(0,8,15,0);
g.drawLine(15,8,15,-8);
};
