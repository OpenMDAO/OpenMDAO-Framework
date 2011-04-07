draw2d.DecoratedConnection=function(){
draw2d.Connection.call(this);
this.setTargetDecorator(new draw2d.ArrowConnectionDecorator());
this.setRouter(new draw2d.FanConnectionRouter());
};
draw2d.DecoratedConnection.prototype=new draw2d.Connection();
draw2d.DecoratedConnection.prototype.type="DecoratedConnection";
