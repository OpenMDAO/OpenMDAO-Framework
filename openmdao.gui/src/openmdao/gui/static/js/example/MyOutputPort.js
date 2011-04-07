draw2d.MyOutputPort=function(_5455){
draw2d.OutputPort.call(this,_5455);
};
draw2d.MyOutputPort.prototype=new draw2d.OutputPort();
draw2d.MyOutputPort.prototype.onDrop=function(port){
if(this.getMaxFanOut()<=this.getFanOut()){
return;
}
if(this.parentNode.id==port.parentNode.id){
}else{
var _5457=new draw2d.CommandConnect(this.parentNode.workflow,this,port);
_5457.setConnection(new draw2d.DecoratedConnection());
this.parentNode.workflow.getCommandStack().execute(_5457);
}
};
