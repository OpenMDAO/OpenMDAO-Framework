draw2d.MyInputPort=function(_5458){
draw2d.InputPort.call(this,_5458);
};
draw2d.MyInputPort.prototype=new draw2d.InputPort();
draw2d.MyInputPort.prototype.onDrop=function(port){
if(port.getMaxFanOut&&port.getMaxFanOut()<=port.getFanOut()){
return;
}
if(this.parentNode.id==port.parentNode.id){
}else{
var _545a=new draw2d.CommandConnect(this.parentNode.workflow,port,this);
_545a.setConnection(new draw2d.DecoratedConnection());
this.parentNode.workflow.getCommandStack().execute(_545a);
}
};
