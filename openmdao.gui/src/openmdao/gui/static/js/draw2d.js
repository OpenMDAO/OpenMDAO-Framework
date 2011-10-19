
/**This notice must be untouched at all times.
This is the COMPRESSED version of the Draw2D Library
WebSite: http://www.draw2d.org
Copyright: 2006 Andreas Herz. All rights reserved.
Created: 5.11.2006 by Andreas Herz (Web: http://www.freegroup.de )
LICENSE: LGPL
**/
var draw2d=new Object();
var _errorStack_=[];
function pushErrorStack(e,_4cdf){
_errorStack_.push(_4cdf+"\n");
throw e;
}
draw2d.AbstractEvent=function(){
this.type=null;
this.target=null;
this.relatedTarget=null;
this.cancelable=false;
this.timeStamp=null;
this.returnValue=true;
};
draw2d.AbstractEvent.prototype.initEvent=function(sType,_4ce1){
this.type=sType;
this.cancelable=_4ce1;
this.timeStamp=(new Date()).getTime();
};
draw2d.AbstractEvent.prototype.preventDefault=function(){
if(this.cancelable){
this.returnValue=false;
}
};
draw2d.AbstractEvent.fireDOMEvent=function(_4ce2,_4ce3){
if(document.createEvent){
var evt=document.createEvent("Events");
evt.initEvent(_4ce2,true,true);
_4ce3.dispatchEvent(evt);
}else{
if(document.createEventObject){
var evt=document.createEventObject();
_4ce3.fireEvent("on"+_4ce2,evt);
}
}
};
draw2d.EventTarget=function(){
this.eventhandlers={};
};
draw2d.EventTarget.prototype.addEventListener=function(sType,_4ce6){
if(typeof this.eventhandlers[sType]=="undefined"){
this.eventhandlers[sType]=[];
}
this.eventhandlers[sType][this.eventhandlers[sType].length]=_4ce6;
};
draw2d.EventTarget.prototype.dispatchEvent=function(_4ce7){
_4ce7.target=this;
if(typeof this.eventhandlers[_4ce7.type]!="undefined"){
for(var i=0;i<this.eventhandlers[_4ce7.type].length;i++){
this.eventhandlers[_4ce7.type][i](_4ce7);
}
}
return _4ce7.returnValue;
};
draw2d.EventTarget.prototype.removeEventListener=function(sType,_4cea){
if(typeof this.eventhandlers[sType]!="undefined"){
var _4ceb=[];
for(var i=0;i<this.eventhandlers[sType].length;i++){
if(this.eventhandlers[sType][i]!=_4cea){
_4ceb[_4ceb.length]=this.eventhandlers[sType][i];
}
}
this.eventhandlers[sType]=_4ceb;
}
};
String.prototype.trim=function(){
return (this.replace(new RegExp("^([\\s]+)|([\\s]+)$","gm"),""));
};
String.prototype.lefttrim=function(){
return (this.replace(new RegExp("^[\\s]+","gm"),""));
};
String.prototype.righttrim=function(){
return (this.replace(new RegExp("[\\s]+$","gm"),""));
};
String.prototype.between=function(left,right,_50c7){
if(!_50c7){
_50c7=0;
}
var li=this.indexOf(left,_50c7);
if(li==-1){
return null;
}
var ri=this.indexOf(right,li);
if(ri==-1){
return null;
}
return this.substring(li+left.length,ri);
};
draw2d.UUID=function(){
};
draw2d.UUID.prototype.type="draw2d.UUID";
draw2d.UUID.create=function(){
var _5b65=function(){
return (((1+Math.random())*65536)|0).toString(16).substring(1);
};
return (_5b65()+_5b65()+"-"+_5b65()+"-"+_5b65()+"-"+_5b65()+"-"+_5b65()+_5b65()+_5b65());
};
draw2d.ArrayList=function(){
this.increment=10;
this.size=0;
this.data=new Array(this.increment);
};
draw2d.ArrayList.EMPTY_LIST=new draw2d.ArrayList();
draw2d.ArrayList.prototype.type="draw2d.ArrayList";
draw2d.ArrayList.prototype.reverse=function(){
var _4e27=new Array(this.size);
for(var i=0;i<this.size;i++){
_4e27[i]=this.data[this.size-i-1];
}
this.data=_4e27;
};
draw2d.ArrayList.prototype.getCapacity=function(){
return this.data.length;
};
draw2d.ArrayList.prototype.getSize=function(){
return this.size;
};
draw2d.ArrayList.prototype.isEmpty=function(){
return this.getSize()===0;
};
draw2d.ArrayList.prototype.getLastElement=function(){
if(this.data[this.getSize()-1]!==null){
return this.data[this.getSize()-1];
}
};
draw2d.ArrayList.prototype.asArray=function(){
this.trimToSize();
return this.data;
};
draw2d.ArrayList.prototype.getFirstElement=function(){
if(this.data[0]!==null&&this.data[0]!==undefined){
return this.data[0];
}
return null;
};
draw2d.ArrayList.prototype.get=function(i){
return this.data[i];
};
draw2d.ArrayList.prototype.add=function(obj){
if(this.getSize()==this.data.length){
this.resize();
}
this.data[this.size++]=obj;
};
draw2d.ArrayList.prototype.addAll=function(obj){
for(var i=0;i<obj.getSize();i++){
this.add(obj.get(i));
}
};
draw2d.ArrayList.prototype.remove=function(obj){
var index=this.indexOf(obj);
if(index>=0){
return this.removeElementAt(index);
}
return null;
};
draw2d.ArrayList.prototype.insertElementAt=function(obj,index){
if(this.size==this.capacity){
this.resize();
}
for(var i=this.getSize();i>index;i--){
this.data[i]=this.data[i-1];
}
this.data[index]=obj;
this.size++;
};
draw2d.ArrayList.prototype.removeElementAt=function(index){
var _4e33=this.data[index];
for(var i=index;i<(this.getSize()-1);i++){
this.data[i]=this.data[i+1];
}
this.data[this.getSize()-1]=null;
this.size--;
return _4e33;
};
draw2d.ArrayList.prototype.removeAllElements=function(){
this.size=0;
for(var i=0;i<this.data.length;i++){
this.data[i]=null;
}
};
draw2d.ArrayList.prototype.indexOf=function(obj){
for(var i=0;i<this.getSize();i++){
if(this.data[i]==obj){
return i;
}
}
return -1;
};
draw2d.ArrayList.prototype.contains=function(obj){
for(var i=0;i<this.getSize();i++){
if(this.data[i]==obj){
return true;
}
}
return false;
};
draw2d.ArrayList.prototype.resize=function(){
newData=new Array(this.data.length+this.increment);
for(var i=0;i<this.data.length;i++){
newData[i]=this.data[i];
}
this.data=newData;
};
draw2d.ArrayList.prototype.trimToSize=function(){
if(this.data.length==this.size){
return;
}
var temp=new Array(this.getSize());
for(var i=0;i<this.getSize();i++){
temp[i]=this.data[i];
}
this.size=temp.length;
this.data=temp;
};
draw2d.ArrayList.prototype.sort=function(f){
var i,j;
var _4e3f;
var _4e40;
var _4e41;
var _4e42;
for(i=1;i<this.getSize();i++){
_4e40=this.data[i];
_4e3f=_4e40[f];
j=i-1;
_4e41=this.data[j];
_4e42=_4e41[f];
while(j>=0&&_4e42>_4e3f){
this.data[j+1]=this.data[j];
j--;
if(j>=0){
_4e41=this.data[j];
_4e42=_4e41[f];
}
}
this.data[j+1]=_4e40;
}
};
draw2d.ArrayList.prototype.clone=function(){
var _4e43=new draw2d.ArrayList(this.size);
for(var i=0;i<this.size;i++){
_4e43.add(this.data[i]);
}
return _4e43;
};
draw2d.ArrayList.prototype.overwriteElementAt=function(obj,index){
this.data[index]=obj;
};
draw2d.ArrayList.prototype.getPersistentAttributes=function(){
return {data:this.data,increment:this.increment,size:this.getSize()};
};
function trace(_5b10){
var _5b11=openwindow("about:blank",700,400);
_5b11.document.writeln("<pre>"+_5b10+"</pre>");
}
function openwindow(url,width,_5b14){
var left=(screen.width-width)/2;
var top=(screen.height-_5b14)/2;
property="left="+left+", top="+top+", toolbar=0,scrollbars=0,location=0,statusbar=0,menubar=0,resizable=1,alwaysRaised,width="+width+",height="+_5b14;
return window.open(url,"_blank",property);
}
function dumpObject(obj){
trace("----------------------------------------------------------------------------");
trace("- Object dump");
trace("----------------------------------------------------------------------------");
for(var i in obj){
try{
if(typeof obj[i]!="function"){
trace(i+" --&gt; "+obj[i]);
}
}
catch(e){
}
}
for(var i in obj){
try{
if(typeof obj[i]=="function"){
trace(i+" --&gt; "+obj[i]);
}
}
catch(e){
}
}
trace("----------------------------------------------------------------------------");
}
draw2d.Drag=function(){
};
draw2d.Drag.current=null;
draw2d.Drag.currentTarget=null;
draw2d.Drag.currentHover=null;
draw2d.Drag.currentCompartment=null;
draw2d.Drag.dragging=false;
draw2d.Drag.isDragging=function(){
return this.dragging;
};
draw2d.Drag.setCurrent=function(_6071){
this.current=_6071;
this.dragging=true;
};
draw2d.Drag.getCurrent=function(){
return this.current;
};
draw2d.Drag.clearCurrent=function(){
this.current=null;
this.dragging=false;
this.currentTarget=null;
};
draw2d.Draggable=function(_6072,_6073){
this.id=draw2d.UUID.create();
this.node=null;
draw2d.EventTarget.call(this);
this.construct(_6072,_6073);
this.diffX=0;
this.diffY=0;
this.targets=new draw2d.ArrayList();
};
draw2d.Draggable.prototype=new draw2d.EventTarget();
draw2d.Draggable.prototype.construct=function(_6074){
if(_6074===null||_6074===undefined){
return;
}
this.element=_6074;
var oThis=this;
var _6076=function(){
var _6077=new draw2d.DragDropEvent();
_6077.initDragDropEvent("dblclick",true);
oThis.dispatchEvent(_6077);
var _6078=arguments[0]||window.event;
_6078.cancelBubble=true;
_6078.returnValue=false;
};
var _6079=function(){
var _607a=arguments[0]||window.event;
var _607b=new draw2d.DragDropEvent();
if(oThis.node!==null){
var _607c=oThis.node.getWorkflow().getAbsoluteX();
var _607d=oThis.node.getWorkflow().getAbsoluteY();
var _607e=oThis.node.getWorkflow().getScrollLeft();
var _607f=oThis.node.getWorkflow().getScrollTop();
_607b.x=_607a.clientX-oThis.element.offsetLeft+_607e-_607c;
_607b.y=_607a.clientY-oThis.element.offsetTop+_607f-_607d;
}
if(_607a.button===2){
_607b.initDragDropEvent("contextmenu",true);
oThis.dispatchEvent(_607b);
}else{
_607b.initDragDropEvent("dragstart",true);
if(oThis.dispatchEvent(_607b)){
oThis.diffX=_607a.clientX-oThis.element.offsetLeft;
oThis.diffY=_607a.clientY-oThis.element.offsetTop;
draw2d.Drag.setCurrent(oThis);
if(oThis.isAttached==true){
oThis.detachEventHandlers();
}
oThis.attachEventHandlers();
}
}
_607a.cancelBubble=true;
_607a.returnValue=false;
};
var _6080=function(){
if(draw2d.Drag.getCurrent()===null){
var _6081=arguments[0]||window.event;
if(draw2d.Drag.currentHover!==null&&oThis!==draw2d.Drag.currentHover){
var _6082=new draw2d.DragDropEvent();
_6082.initDragDropEvent("mouseleave",false,oThis);
draw2d.Drag.currentHover.dispatchEvent(_6082);
}
if(oThis!==null&&oThis!==draw2d.Drag.currentHover){
var _6082=new draw2d.DragDropEvent();
_6082.initDragDropEvent("mouseenter",false,oThis);
oThis.dispatchEvent(_6082);
}
draw2d.Drag.currentHover=oThis;
}else{
}
};
if(this.element.addEventListener){
this.element.addEventListener("mousemove",_6080,false);
this.element.addEventListener("mousedown",_6079,false);
this.element.addEventListener("dblclick",_6076,false);
}else{
if(this.element.attachEvent){
this.element.attachEvent("onmousemove",_6080);
this.element.attachEvent("onmousedown",_6079);
this.element.attachEvent("ondblclick",_6076);
}else{
throw "Drag not supported in this browser.";
}
}
};
draw2d.Draggable.prototype.onDrop=function(_6083,_6084){
};
draw2d.Draggable.prototype.attachEventHandlers=function(){
var oThis=this;
oThis.isAttached=true;
this.tempMouseMove=function(){
var _6086=arguments[0]||window.event;
var _6087=new draw2d.Point(_6086.clientX-oThis.diffX,_6086.clientY-oThis.diffY);
if(oThis.node!==null&&oThis.node.getCanSnapToHelper()){
_6087=oThis.node.getWorkflow().snapToHelper(oThis.node,_6087);
}
oThis.element.style.left=_6087.x+"px";
oThis.element.style.top=_6087.y+"px";
if(oThis.node!==null){
var _6088=oThis.node.getWorkflow().getScrollLeft();
var _6089=oThis.node.getWorkflow().getScrollTop();
var _608a=oThis.node.getWorkflow().getAbsoluteX();
var _608b=oThis.node.getWorkflow().getAbsoluteY();
var _608c=oThis.getDropTarget(_6086.clientX+_6088-_608a,_6086.clientY+_6089-_608b);
var _608d=oThis.getCompartment(_6086.clientX+_6088-_608a,_6086.clientY+_6089-_608b);
if(draw2d.Drag.currentTarget!==null&&_608c!=draw2d.Drag.currentTarget){
var _608e=new draw2d.DragDropEvent();
_608e.initDragDropEvent("dragleave",false,oThis);
draw2d.Drag.currentTarget.dispatchEvent(_608e);
}
if(_608c!==null&&_608c!==draw2d.Drag.currentTarget){
var _608e=new draw2d.DragDropEvent();
_608e.initDragDropEvent("dragenter",false,oThis);
_608c.dispatchEvent(_608e);
}
draw2d.Drag.currentTarget=_608c;
if(draw2d.Drag.currentCompartment!==null&&_608d!==draw2d.Drag.currentCompartment){
var _608e=new draw2d.DragDropEvent();
_608e.initDragDropEvent("figureleave",false,oThis);
draw2d.Drag.currentCompartment.dispatchEvent(_608e);
}
if(_608d!==null&&_608d.node!=oThis.node&&_608d!==draw2d.Drag.currentCompartment){
var _608e=new draw2d.DragDropEvent();
_608e.initDragDropEvent("figureenter",false,oThis);
_608d.dispatchEvent(_608e);
}
draw2d.Drag.currentCompartment=_608d;
}
var _608f=new draw2d.DragDropEvent();
_608f.initDragDropEvent("drag",false);
oThis.dispatchEvent(_608f);
};
oThis.tempMouseUp=function(){
oThis.detachEventHandlers();
var _6090=arguments[0]||window.event;
if(oThis.node!==null){
var _6091=oThis.node.getWorkflow().getScrollLeft();
var _6092=oThis.node.getWorkflow().getScrollTop();
var _6093=oThis.node.getWorkflow().getAbsoluteX();
var _6094=oThis.node.getWorkflow().getAbsoluteY();
var _6095=oThis.getDropTarget(_6090.clientX+_6091-_6093,_6090.clientY+_6092-_6094);
var _6096=oThis.getCompartment(_6090.clientX+_6091-_6093,_6090.clientY+_6092-_6094);
if(_6095!==null){
var _6097=new draw2d.DragDropEvent();
_6097.initDragDropEvent("drop",false,oThis);
_6095.dispatchEvent(_6097);
}
if(_6096!==null&&_6096.node!==oThis.node){
var _6097=new draw2d.DragDropEvent();
_6097.initDragDropEvent("figuredrop",false,oThis);
_6096.dispatchEvent(_6097);
}
if(draw2d.Drag.currentTarget!==null){
var _6097=new draw2d.DragDropEvent();
_6097.initDragDropEvent("dragleave",false,oThis);
draw2d.Drag.currentTarget.dispatchEvent(_6097);
draw2d.Drag.currentTarget=null;
}
}
var _6098=new draw2d.DragDropEvent();
_6098.initDragDropEvent("dragend",false);
oThis.dispatchEvent(_6098);
oThis.onDrop(_6090.clientX,_6090.clientY);
draw2d.Drag.currentCompartment=null;
draw2d.Drag.clearCurrent();
};
if(document.body.addEventListener){
document.body.addEventListener("mousemove",this.tempMouseMove,false);
document.body.addEventListener("mouseup",this.tempMouseUp,false);
}else{
if(document.body.attachEvent){
document.body.attachEvent("onmousemove",this.tempMouseMove);
document.body.attachEvent("onmouseup",this.tempMouseUp);
}else{
throw new Error("Drag doesn't support this browser.");
}
}
};
draw2d.Draggable.prototype.detachEventHandlers=function(){
this.isAttached=false;
if(document.body.removeEventListener){
document.body.removeEventListener("mousemove",this.tempMouseMove,false);
document.body.removeEventListener("mouseup",this.tempMouseUp,false);
}else{
if(document.body.detachEvent){
document.body.detachEvent("onmousemove",this.tempMouseMove);
document.body.detachEvent("onmouseup",this.tempMouseUp);
}else{
throw new Error("Drag doesn't support this browser.");
}
}
};
draw2d.Draggable.prototype.getDropTarget=function(x,y){
for(var i=0;i<this.targets.getSize();i++){
var _609c=this.targets.get(i);
if(_609c.node.isOver(x,y)&&_609c.node!==this.node){
return _609c;
}
}
return null;
};
draw2d.Draggable.prototype.getCompartment=function(x,y){
var _609f=null;
for(var i=0;i<this.node.getWorkflow().compartments.getSize();i++){
var _60a1=this.node.getWorkflow().compartments.get(i);
if(_60a1.isOver(x,y)&&_60a1!==this.node){
if(_609f===null){
_609f=_60a1;
}else{
if(_609f.getZOrder()<_60a1.getZOrder()){
_609f=_60a1;
}
}
}
}
return _609f===null?null:_609f.dropable;
};
draw2d.Draggable.prototype.getLeft=function(){
return this.element.offsetLeft;
};
draw2d.Draggable.prototype.getTop=function(){
return this.element.offsetTop;
};
draw2d.DragDropEvent=function(){
draw2d.AbstractEvent.call(this);
};
draw2d.DragDropEvent.prototype=new draw2d.AbstractEvent();
draw2d.DragDropEvent.prototype.initDragDropEvent=function(sType,_60a3,_60a4){
this.initEvent(sType,_60a3);
this.relatedTarget=_60a4;
};
draw2d.DropTarget=function(_60a5){
draw2d.EventTarget.call(this);
this.construct(_60a5);
};
draw2d.DropTarget.prototype=new draw2d.EventTarget();
draw2d.DropTarget.prototype.construct=function(_60a6){
this.element=_60a6;
};
draw2d.DropTarget.prototype.getLeft=function(){
var el=this.element;
var ol=el.offsetLeft;
while((el=el.offsetParent)!==null){
ol+=el.offsetLeft;
}
return ol;
};
draw2d.DropTarget.prototype.getTop=function(){
var el=this.element;
var ot=el.offsetTop;
while((el=el.offsetParent)!==null){
ot+=el.offsetTop;
}
return ot;
};
draw2d.DropTarget.prototype.getHeight=function(){
return this.element.offsetHeight;
};
draw2d.DropTarget.prototype.getWidth=function(){
return this.element.offsetWidth;
};
draw2d.PositionConstants=function(){
};
draw2d.PositionConstants.NORTH=1;
draw2d.PositionConstants.SOUTH=4;
draw2d.PositionConstants.WEST=8;
draw2d.PositionConstants.EAST=16;
draw2d.Color=function(red,green,blue){
if(typeof green=="undefined"){
var rgb=this.hex2rgb(red);
this.red=rgb[0];
this.green=rgb[1];
this.blue=rgb[2];
}else{
this.red=red;
this.green=green;
this.blue=blue;
}
};
draw2d.Color.prototype.type="draw2d.Color";
draw2d.Color.prototype.getHTMLStyle=function(){
return "rgb("+this.red+","+this.green+","+this.blue+")";
};
draw2d.Color.prototype.getRed=function(){
return this.red;
};
draw2d.Color.prototype.getGreen=function(){
return this.green;
};
draw2d.Color.prototype.getBlue=function(){
return this.blue;
};
draw2d.Color.prototype.getIdealTextColor=function(){
var _60ed=105;
var _60ee=(this.red*0.299)+(this.green*0.587)+(this.blue*0.114);
return (255-_60ee<_60ed)?new draw2d.Color(0,0,0):new draw2d.Color(255,255,255);
};
draw2d.Color.prototype.hex2rgb=function(_60ef){
_60ef=_60ef.replace("#","");
return ({0:parseInt(_60ef.substr(0,2),16),1:parseInt(_60ef.substr(2,2),16),2:parseInt(_60ef.substr(4,2),16)});
};
draw2d.Color.prototype.hex=function(){
return (this.int2hex(this.red)+this.int2hex(this.green)+this.int2hex(this.blue));
};
draw2d.Color.prototype.int2hex=function(v){
v=Math.round(Math.min(Math.max(0,v),255));
return ("0123456789ABCDEF".charAt((v-v%16)/16)+"0123456789ABCDEF".charAt(v%16));
};
draw2d.Color.prototype.darker=function(_60f1){
var red=parseInt(Math.round(this.getRed()*(1-_60f1)));
var green=parseInt(Math.round(this.getGreen()*(1-_60f1)));
var blue=parseInt(Math.round(this.getBlue()*(1-_60f1)));
if(red<0){
red=0;
}else{
if(red>255){
red=255;
}
}
if(green<0){
green=0;
}else{
if(green>255){
green=255;
}
}
if(blue<0){
blue=0;
}else{
if(blue>255){
blue=255;
}
}
return new draw2d.Color(red,green,blue);
};
draw2d.Color.prototype.lighter=function(_60f5){
var red=parseInt(Math.round(this.getRed()*(1+_60f5)));
var green=parseInt(Math.round(this.getGreen()*(1+_60f5)));
var blue=parseInt(Math.round(this.getBlue()*(1+_60f5)));
if(red<0){
red=0;
}else{
if(red>255){
red=255;
}
}
if(green<0){
green=0;
}else{
if(green>255){
green=255;
}
}
if(blue<0){
blue=0;
}else{
if(blue>255){
blue=255;
}
}
return new draw2d.Color(red,green,blue);
};
draw2d.Point=function(x,y){
this.x=x;
this.y=y;
};
draw2d.Point.prototype.type="draw2d.Point";
draw2d.Point.prototype.getX=function(){
return this.x;
};
draw2d.Point.prototype.getY=function(){
return this.y;
};
draw2d.Point.prototype.getPosition=function(p){
var dx=p.x-this.x;
var dy=p.y-this.y;
if(Math.abs(dx)>Math.abs(dy)){
if(dx<0){
return draw2d.PositionConstants.WEST;
}
return draw2d.PositionConstants.EAST;
}
if(dy<0){
return draw2d.PositionConstants.NORTH;
}
return draw2d.PositionConstants.SOUTH;
};
draw2d.Point.prototype.equals=function(o){
return this.x==o.x&&this.y==o.y;
};
draw2d.Point.prototype.getDistance=function(other){
return Math.sqrt((this.x-other.x)*(this.x-other.x)+(this.y-other.y)*(this.y-other.y));
};
draw2d.Point.prototype.getTranslated=function(other){
return new draw2d.Point(this.x+other.x,this.y+other.y);
};
draw2d.Point.prototype.getPersistentAttributes=function(){
return {x:this.x,y:this.y};
};
draw2d.Dimension=function(x,y,w,h){
draw2d.Point.call(this,x,y);
this.w=w;
this.h=h;
};
draw2d.Dimension.prototype=new draw2d.Point();
draw2d.Dimension.prototype.type="draw2d.Dimension";
draw2d.Dimension.prototype.translate=function(dx,dy){
this.x+=dx;
this.y+=dy;
return this;
};
draw2d.Dimension.prototype.resize=function(dw,dh){
this.w+=dw;
this.h+=dh;
return this;
};
draw2d.Dimension.prototype.setBounds=function(rect){
this.x=rect.x;
this.y=rect.y;
this.w=rect.w;
this.h=rect.h;
return this;
};
draw2d.Dimension.prototype.isEmpty=function(){
return this.w<=0||this.h<=0;
};
draw2d.Dimension.prototype.getWidth=function(){
return this.w;
};
draw2d.Dimension.prototype.getHeight=function(){
return this.h;
};
draw2d.Dimension.prototype.getRight=function(){
return this.x+this.w;
};
draw2d.Dimension.prototype.getBottom=function(){
return this.y+this.h;
};
draw2d.Dimension.prototype.getTopLeft=function(){
return new draw2d.Point(this.x,this.y);
};
draw2d.Dimension.prototype.getCenter=function(){
return new draw2d.Point(this.x+this.w/2,this.y+this.h/2);
};
draw2d.Dimension.prototype.getBottomRight=function(){
return new draw2d.Point(this.x+this.w,this.y+this.h);
};
draw2d.Dimension.prototype.equals=function(o){
return this.x==o.x&&this.y==o.y&&this.w==o.w&&this.h==o.h;
};
draw2d.SnapToHelper=function(_6335){
this.workflow=_6335;
};
draw2d.SnapToHelper.NORTH=1;
draw2d.SnapToHelper.SOUTH=4;
draw2d.SnapToHelper.WEST=8;
draw2d.SnapToHelper.EAST=16;
draw2d.SnapToHelper.CENTER=32;
draw2d.SnapToHelper.NORTH_EAST=draw2d.SnapToHelper.NORTH|draw2d.SnapToHelper.EAST;
draw2d.SnapToHelper.NORTH_WEST=draw2d.SnapToHelper.NORTH|draw2d.SnapToHelper.WEST;
draw2d.SnapToHelper.SOUTH_EAST=draw2d.SnapToHelper.SOUTH|draw2d.SnapToHelper.EAST;
draw2d.SnapToHelper.SOUTH_WEST=draw2d.SnapToHelper.SOUTH|draw2d.SnapToHelper.WEST;
draw2d.SnapToHelper.NORTH_SOUTH=draw2d.SnapToHelper.NORTH|draw2d.SnapToHelper.SOUTH;
draw2d.SnapToHelper.EAST_WEST=draw2d.SnapToHelper.EAST|draw2d.SnapToHelper.WEST;
draw2d.SnapToHelper.NSEW=draw2d.SnapToHelper.NORTH_SOUTH|draw2d.SnapToHelper.EAST_WEST;
draw2d.SnapToHelper.prototype.snapPoint=function(_6336,_6337,_6338){
return _6337;
};
draw2d.SnapToHelper.prototype.snapRectangle=function(_6339,_633a){
return _6339;
};
draw2d.SnapToHelper.prototype.onSetDocumentDirty=function(){
};
draw2d.SnapToGrid=function(_610f){
draw2d.SnapToHelper.call(this,_610f);
};
draw2d.SnapToGrid.prototype=new draw2d.SnapToHelper();
draw2d.SnapToGrid.prototype.type="draw2d.SnapToGrid";
draw2d.SnapToGrid.prototype.snapPoint=function(_6110,_6111,_6112){
_6112.x=this.workflow.gridWidthX*Math.floor(((_6111.x+this.workflow.gridWidthX/2)/this.workflow.gridWidthX));
_6112.y=this.workflow.gridWidthY*Math.floor(((_6111.y+this.workflow.gridWidthY/2)/this.workflow.gridWidthY));
return 0;
};
draw2d.SnapToGrid.prototype.snapRectangle=function(_6113,_6114){
_6114.x=_6113.x;
_6114.y=_6113.y;
_6114.w=_6113.w;
_6114.h=_6113.h;
return 0;
};
draw2d.SnapToGeometryEntry=function(type,_556f){
this.type=type;
this.location=_556f;
};
draw2d.SnapToGeometryEntry.prototype.getLocation=function(){
return this.location;
};
draw2d.SnapToGeometryEntry.prototype.getType=function(){
return this.type;
};
draw2d.SnapToGeometry=function(_4df0){
draw2d.SnapToHelper.call(this,_4df0);
this.rows=null;
this.cols=null;
};
draw2d.SnapToGeometry.prototype=new draw2d.SnapToHelper();
draw2d.SnapToGeometry.THRESHOLD=5;
draw2d.SnapToGeometry.prototype.snapPoint=function(_4df1,_4df2,_4df3){
if(this.rows===null||this.cols===null){
this.populateRowsAndCols();
}
if((_4df1&draw2d.SnapToHelper.EAST)!==0){
var _4df4=this.getCorrectionFor(this.cols,_4df2.getX()-1,1);
if(_4df4!==draw2d.SnapToGeometry.THRESHOLD){
_4df1&=~draw2d.SnapToHelper.EAST;
_4df3.x+=_4df4;
}
}
if((_4df1&draw2d.SnapToHelper.WEST)!==0){
var _4df5=this.getCorrectionFor(this.cols,_4df2.getX(),-1);
if(_4df5!==draw2d.SnapToGeometry.THRESHOLD){
_4df1&=~draw2d.SnapToHelper.WEST;
_4df3.x+=_4df5;
}
}
if((_4df1&draw2d.SnapToHelper.SOUTH)!==0){
var _4df6=this.getCorrectionFor(this.rows,_4df2.getY()-1,1);
if(_4df6!==draw2d.SnapToGeometry.THRESHOLD){
_4df1&=~draw2d.SnapToHelper.SOUTH;
_4df3.y+=_4df6;
}
}
if((_4df1&draw2d.SnapToHelper.NORTH)!==0){
var _4df7=this.getCorrectionFor(this.rows,_4df2.getY(),-1);
if(_4df7!==draw2d.SnapToGeometry.THRESHOLD){
_4df1&=~draw2d.SnapToHelper.NORTH;
_4df3.y+=_4df7;
}
}
return _4df1;
};
draw2d.SnapToGeometry.prototype.snapRectangle=function(_4df8,_4df9){
var _4dfa=_4df8.getTopLeft();
var _4dfb=_4df8.getBottomRight();
var _4dfc=this.snapPoint(draw2d.SnapToHelper.NORTH_WEST,_4df8.getTopLeft(),_4dfa);
_4df9.x=_4dfa.x;
_4df9.y=_4dfa.y;
var _4dfd=this.snapPoint(draw2d.SnapToHelper.SOUTH_EAST,_4df8.getBottomRight(),_4dfb);
if(_4dfc&draw2d.SnapToHelper.WEST){
_4df9.x=_4dfb.x-_4df8.getWidth();
}
if(_4dfc&draw2d.SnapToHelper.NORTH){
_4df9.y=_4dfb.y-_4df8.getHeight();
}
return _4dfc|_4dfd;
};
draw2d.SnapToGeometry.prototype.populateRowsAndCols=function(){
this.rows=[];
this.cols=[];
var _4dfe=this.workflow.getDocument().getFigures();
var index=0;
for(var i=0;i<_4dfe.getSize();i++){
var _4e01=_4dfe.get(i);
if(_4e01!=this.workflow.getCurrentSelection()){
var _4e02=_4e01.getBounds();
this.cols[index*3]=new draw2d.SnapToGeometryEntry(-1,_4e02.getX());
this.rows[index*3]=new draw2d.SnapToGeometryEntry(-1,_4e02.getY());
this.cols[index*3+1]=new draw2d.SnapToGeometryEntry(0,_4e02.x+(_4e02.getWidth()-1)/2);
this.rows[index*3+1]=new draw2d.SnapToGeometryEntry(0,_4e02.y+(_4e02.getHeight()-1)/2);
this.cols[index*3+2]=new draw2d.SnapToGeometryEntry(1,_4e02.getRight()-1);
this.rows[index*3+2]=new draw2d.SnapToGeometryEntry(1,_4e02.getBottom()-1);
index++;
}
}
};
draw2d.SnapToGeometry.prototype.getCorrectionFor=function(_4e03,value,side){
var _4e06=draw2d.SnapToGeometry.THRESHOLD;
var _4e07=draw2d.SnapToGeometry.THRESHOLD;
for(var i=0;i<_4e03.length;i++){
var entry=_4e03[i];
var _4e0a;
if(entry.type===-1&&side!==0){
_4e0a=Math.abs(value-entry.location);
if(_4e0a<_4e06){
_4e06=_4e0a;
_4e07=entry.location-value;
}
}else{
if(entry.type===0&&side===0){
_4e0a=Math.abs(value-entry.location);
if(_4e0a<_4e06){
_4e06=_4e0a;
_4e07=entry.location-value;
}
}else{
if(entry.type===1&&side!==0){
_4e0a=Math.abs(value-entry.location);
if(_4e0a<_4e06){
_4e06=_4e0a;
_4e07=entry.location-value;
}
}
}
}
}
return _4e07;
};
draw2d.SnapToGeometry.prototype.onSetDocumentDirty=function(){
this.rows=null;
this.cols=null;
};
draw2d.Border=function(){
this.color=null;
};
draw2d.Border.prototype.type="draw2d.Border";
draw2d.Border.prototype.dispose=function(){
this.color=null;
};
draw2d.Border.prototype.getHTMLStyle=function(){
return "";
};
draw2d.Border.prototype.setColor=function(c){
this.color=c;
};
draw2d.Border.prototype.getColor=function(){
return this.color;
};
draw2d.Border.prototype.refresh=function(){
};
draw2d.LineBorder=function(width){
draw2d.Border.call(this);
this.width=1;
if(width){
this.width=width;
}
this.figure=null;
};
draw2d.LineBorder.prototype=new draw2d.Border();
draw2d.LineBorder.prototype.type="draw2d.LineBorder";
draw2d.LineBorder.prototype.dispose=function(){
draw2d.Border.prototype.dispose.call(this);
this.figure=null;
};
draw2d.LineBorder.prototype.setLineWidth=function(w){
this.width=w;
if(this.figure!==null){
this.figure.html.style.border=this.getHTMLStyle();
}
};
draw2d.LineBorder.prototype.getHTMLStyle=function(){
if(this.getColor()!==null){
return this.width+"px solid "+this.getColor().getHTMLStyle();
}
return this.width+"px solid black";
};
draw2d.LineBorder.prototype.refresh=function(){
this.setLineWidth(this.width);
};
draw2d.Figure=function(){
this.construct();
};
draw2d.Figure.prototype.type="draw2d.Figure";
draw2d.Figure.ZOrderBaseIndex=100;
draw2d.Figure.setZOrderBaseIndex=function(index){
draw2d.Figure.ZOrderBaseIndex=index;
};
draw2d.Figure.prototype.construct=function(){
this.lastDragStartTime=0;
this.x=0;
this.y=0;
this.width=10;
this.height=10;
this.border=null;
this.id=draw2d.UUID.create();
this.html=this.createHTMLElement();
this.canvas=null;
this.workflow=null;
this.draggable=null;
this.parent=null;
this.isMoving=false;
this.canSnapToHelper=true;
this.snapToGridAnchor=new draw2d.Point(0,0);
this.timer=-1;
this.model=null;
this.alpha=1;
this.alphaBeforeOnDrag=1;
this.properties={};
this.moveListener=new draw2d.ArrayList();
this.setDimension(this.width,this.height);
this.setDeleteable(true);
this.setCanDrag(true);
this.setResizeable(true);
this.setSelectable(true);
};
draw2d.Figure.prototype.dispose=function(){
this.canvas=null;
this.workflow=null;
this.moveListener=null;
if(this.draggable!==null){
this.draggable.removeEventListener("mouseenter",this.tmpMouseEnter);
this.draggable.removeEventListener("mouseleave",this.tmpMouseLeave);
this.draggable.removeEventListener("dragend",this.tmpDragend);
this.draggable.removeEventListener("dragstart",this.tmpDragstart);
this.draggable.removeEventListener("drag",this.tmpDrag);
this.draggable.removeEventListener("dblclick",this.tmpDoubleClick);
this.draggable.node=null;
this.draggable.target.removeAllElements();
}
this.draggable=null;
if(this.border!==null){
this.border.dispose();
}
this.border=null;
if(this.parent!==null){
this.parent.removeChild(this);
}
};
draw2d.Figure.prototype.getProperties=function(){
return this.properties;
};
draw2d.Figure.prototype.getProperty=function(key){
return this.properties[key];
};
draw2d.Figure.prototype.setProperty=function(key,value){
this.properties[key]=value;
this.setDocumentDirty();
};
draw2d.Figure.prototype.getId=function(){
return this.id;
};
draw2d.Figure.prototype.setId=function(id){
this.id=id;
if(this.html!==null){
this.html.id=id;
}
};
draw2d.Figure.prototype.setCanvas=function(_5de2){
this.canvas=_5de2;
};
draw2d.Figure.prototype.getWorkflow=function(){
return this.workflow;
};
draw2d.Figure.prototype.setWorkflow=function(_5de3){
if(this.draggable===null){
this.html.tabIndex="0";
var oThis=this;
this.keyDown=function(event){
event.cancelBubble=true;
event.returnValue=true;
oThis.onKeyDown(event.keyCode,event.ctrlKey);
};
if(this.html.addEventListener){
this.html.addEventListener("keydown",this.keyDown,false);
}else{
if(this.html.attachEvent){
this.html.attachEvent("onkeydown",this.keyDown);
}
}
this.draggable=new draw2d.Draggable(this.html,draw2d.Draggable.DRAG_X|draw2d.Draggable.DRAG_Y);
this.draggable.node=this;
this.tmpContextMenu=function(_5de6){
oThis.onContextMenu(oThis.x+_5de6.x,_5de6.y+oThis.y);
};
this.tmpMouseEnter=function(_5de7){
oThis.onMouseEnter();
};
this.tmpMouseLeave=function(_5de8){
oThis.onMouseLeave();
};
this.tmpDragend=function(_5de9){
oThis.onDragend();
};
this.tmpDragstart=function(_5dea){
var w=oThis.workflow;
w.showMenu(null);
if(w.toolPalette&&w.toolPalette.activeTool){
_5dea.returnValue=false;
w.onMouseDown(oThis.x+_5dea.x,_5dea.y+oThis.y);
w.onMouseUp(oThis.x+_5dea.x,_5dea.y+oThis.y);
return;
}
if(!(oThis instanceof draw2d.ResizeHandle)&&!(oThis instanceof draw2d.Port)){
var line=w.getBestLine(oThis.x+_5dea.x,_5dea.y+oThis.y);
if(line!==null){
_5dea.returnValue=false;
w.setCurrentSelection(line);
w.showLineResizeHandles(line);
w.onMouseDown(oThis.x+_5dea.x,_5dea.y+oThis.y);
return;
}else{
if(oThis.isSelectable()){
w.showResizeHandles(oThis);
w.setCurrentSelection(oThis);
}
}
}
_5dea.returnValue=oThis.onDragstart(_5dea.x,_5dea.y);
};
this.tmpDrag=function(_5ded){
oThis.onDrag();
};
this.tmpDoubleClick=function(_5dee){
oThis.onDoubleClick();
};
this.draggable.addEventListener("contextmenu",this.tmpContextMenu);
this.draggable.addEventListener("mouseenter",this.tmpMouseEnter);
this.draggable.addEventListener("mouseleave",this.tmpMouseLeave);
this.draggable.addEventListener("dragend",this.tmpDragend);
this.draggable.addEventListener("dragstart",this.tmpDragstart);
this.draggable.addEventListener("drag",this.tmpDrag);
this.draggable.addEventListener("dblclick",this.tmpDoubleClick);
}
this.workflow=_5de3;
};
draw2d.Figure.prototype.createHTMLElement=function(){
var item=document.createElement("div");
item.id=this.id;
item.style.position="absolute";
item.style.left=this.x+"px";
item.style.top=this.y+"px";
item.style.height=this.width+"px";
item.style.width=this.height+"px";
item.style.margin="0px";
item.style.padding="0px";
item.style.outline="none";
item.style.zIndex=""+draw2d.Figure.ZOrderBaseIndex;
return item;
};
draw2d.Figure.prototype.setParent=function(_5df0){
this.parent=_5df0;
};
draw2d.Figure.prototype.getParent=function(){
return this.parent;
};
draw2d.Figure.prototype.getZOrder=function(){
return this.html.style.zIndex;
};
draw2d.Figure.prototype.setZOrder=function(index){
this.html.style.zIndex=index;
};
draw2d.Figure.prototype.hasFixedPosition=function(){
return false;
};
draw2d.Figure.prototype.getMinWidth=function(){
return 5;
};
draw2d.Figure.prototype.getMinHeight=function(){
return 5;
};
draw2d.Figure.prototype.getHTMLElement=function(){
if(this.html===null){
this.html=this.createHTMLElement();
}
return this.html;
};
draw2d.Figure.prototype.paint=function(){
};
draw2d.Figure.prototype.setBorder=function(_5df2){
if(this.border!==null){
this.border.figure=null;
}
this.border=_5df2;
this.border.figure=this;
this.border.refresh();
this.setDocumentDirty();
};
draw2d.Figure.prototype.onRemove=function(_5df3){
};
draw2d.Figure.prototype.onContextMenu=function(x,y){
var menu=this.getContextMenu();
if(menu!==null){
this.workflow.showMenu(menu,x,y);
}
};
draw2d.Figure.prototype.getContextMenu=function(){
return null;
};
draw2d.Figure.prototype.onDoubleClick=function(){
};
draw2d.Figure.prototype.onMouseEnter=function(){
};
draw2d.Figure.prototype.onMouseLeave=function(){
};
draw2d.Figure.prototype.onDrag=function(){
this.x=this.draggable.getLeft();
this.y=this.draggable.getTop();
if(this.isMoving==false){
this.isMoving=true;
this.alphaBeforeOnDrag=this.getAlpha();
this.setAlpha(this.alphaBeforeOnDrag*0.5);
}
this.fireMoveEvent();
};
draw2d.Figure.prototype.onDragend=function(){
if(this.getWorkflow().getEnableSmoothFigureHandling()===true){
var oThis=this;
var _5df8=function(){
if(oThis.alpha<oThis.alphaBeforeOnDrag){
oThis.setAlpha(Math.min(1,oThis.alpha+0.05));
}else{
window.clearInterval(oThis.timer);
oThis.timer=-1;
}
};
if(oThis.timer>0){
window.clearInterval(oThis.timer);
}
oThis.timer=window.setInterval(_5df8,20);
}else{
this.setAlpha(this.alphaBeforeOnDrag);
}
this.command.setPosition(this.x,this.y);
this.workflow.commandStack.execute(this.command);
this.command=null;
this.isMoving=false;
this.workflow.hideSnapToHelperLines();
this.fireMoveEvent();
};
draw2d.Figure.prototype.onDragstart=function(x,y){
this.command=this.createCommand(new draw2d.EditPolicy(draw2d.EditPolicy.MOVE));
return this.command!==null;
};
draw2d.Figure.prototype.setCanDrag=function(flag){
this.canDrag=flag;
if(flag){
this.html.style.cursor="move";
}else{
this.html.style.cursor="";
}
};
draw2d.Figure.prototype.getCanDrag=function(){
return this.canDrag;
};
draw2d.Figure.prototype.setAlpha=function(_5dfc){
if(this.alpha===_5dfc){
return;
}
this.alpha=Math.max(0,Math.min(1,_5dfc));
if(this.alpha==1){
this.html.style.filter="";
this.html.style.opacity="";
}else{
this.html.style.filter="alpha(opacity="+Math.round(this.alpha*100)+")";
this.html.style.opacity=this.alpha;
}
};
draw2d.Figure.prototype.getAlpha=function(){
return this.alpha;
};
draw2d.Figure.prototype.setDimension=function(w,h){
this.width=Math.max(this.getMinWidth(),w);
this.height=Math.max(this.getMinHeight(),h);
if(this.html===null){
return;
}
this.html.style.width=this.width+"px";
this.html.style.height=this.height+"px";
this.fireMoveEvent();
if(this.workflow!==null&&this.workflow.getCurrentSelection()==this){
this.workflow.showResizeHandles(this);
}
};
draw2d.Figure.prototype.setPosition=function(xPos,yPos){
this.x=xPos;
this.y=yPos;
if(this.html===null){
return;
}
this.html.style.left=this.x+"px";
this.html.style.top=this.y+"px";
this.fireMoveEvent();
if(this.workflow!==null&&this.workflow.getCurrentSelection()==this){
this.workflow.showResizeHandles(this);
}
};
draw2d.Figure.prototype.isResizeable=function(){
return this.resizeable;
};
draw2d.Figure.prototype.setResizeable=function(flag){
this.resizeable=flag;
};
draw2d.Figure.prototype.isSelectable=function(){
return this.selectable;
};
draw2d.Figure.prototype.setSelectable=function(flag){
this.selectable=flag;
};
draw2d.Figure.prototype.isStrechable=function(){
return true;
};
draw2d.Figure.prototype.isDeleteable=function(){
return this.deleteable;
};
draw2d.Figure.prototype.setDeleteable=function(flag){
this.deleteable=flag;
};
draw2d.Figure.prototype.setCanSnapToHelper=function(flag){
this.canSnapToHelper=flag;
};
draw2d.Figure.prototype.getCanSnapToHelper=function(){
return this.canSnapToHelper;
};
draw2d.Figure.prototype.getSnapToGridAnchor=function(){
return this.snapToGridAnchor;
};
draw2d.Figure.prototype.setSnapToGridAnchor=function(point){
this.snapToGridAnchor=point;
};
draw2d.Figure.prototype.getBounds=function(){
return new draw2d.Dimension(this.getX(),this.getY(),this.getWidth(),this.getHeight());
};
draw2d.Figure.prototype.getWidth=function(){
return this.width;
};
draw2d.Figure.prototype.getHeight=function(){
return this.height;
};
draw2d.Figure.prototype.getY=function(){
return this.y;
};
draw2d.Figure.prototype.getX=function(){
return this.x;
};
draw2d.Figure.prototype.getAbsoluteY=function(){
return this.y;
};
draw2d.Figure.prototype.getAbsoluteX=function(){
return this.x;
};
draw2d.Figure.prototype.onKeyDown=function(_5e06,ctrl){
if(_5e06==46){
this.workflow.getCommandStack().execute(this.createCommand(new draw2d.EditPolicy(draw2d.EditPolicy.DELETE)));
}
if(ctrl){
this.workflow.onKeyDown(_5e06,ctrl);
}
};
draw2d.Figure.prototype.getPosition=function(){
return new draw2d.Point(this.x,this.y);
};
draw2d.Figure.prototype.isOver=function(iX,iY){
var x=this.getAbsoluteX();
var y=this.getAbsoluteY();
var iX2=x+this.width;
var iY2=y+this.height;
return (iX>=x&&iX<=iX2&&iY>=y&&iY<=iY2);
};
draw2d.Figure.prototype.attachMoveListener=function(_5e0e){
if(_5e0e===null||this.moveListener===null){
return;
}
this.moveListener.add(_5e0e);
};
draw2d.Figure.prototype.detachMoveListener=function(_5e0f){
if(_5e0f===null||this.moveListener===null){
return;
}
this.moveListener.remove(_5e0f);
};
draw2d.Figure.prototype.fireMoveEvent=function(){
this.setDocumentDirty();
var size=this.moveListener.getSize();
for(var i=0;i<size;i++){
this.moveListener.get(i).onOtherFigureMoved(this);
}
};
draw2d.Figure.prototype.setModel=function(model){
if(this.model!==null){
this.model.removePropertyChangeListener(this);
}
this.model=model;
if(this.model!==null){
this.model.addPropertyChangeListener(this);
}
};
draw2d.Figure.prototype.getModel=function(){
return this.model;
};
draw2d.Figure.prototype.onOtherFigureMoved=function(_5e13){
};
draw2d.Figure.prototype.setDocumentDirty=function(){
if(this.workflow!==null){
this.workflow.setDocumentDirty();
}
};
draw2d.Figure.prototype.disableTextSelection=function(_5e14){
_5e14.onselectstart=function(){
return false;
};
_5e14.unselectable="on";
_5e14.style.MozUserSelect="none";
_5e14.onmousedown=function(){
return false;
};
};
draw2d.Figure.prototype.createCommand=function(_5e15){
if(_5e15.getPolicy()==draw2d.EditPolicy.MOVE){
if(!this.canDrag){
return null;
}
return new draw2d.CommandMove(this);
}
if(_5e15.getPolicy()==draw2d.EditPolicy.DELETE){
if(!this.isDeleteable()){
return null;
}
return new draw2d.CommandDelete(this);
}
if(_5e15.getPolicy()==draw2d.EditPolicy.RESIZE){
if(!this.isResizeable()){
return null;
}
return new draw2d.CommandResize(this);
}
return null;
};
draw2d.Node=function(){
this.bgColor=null;
this.lineColor=new draw2d.Color(128,128,255);
this.lineStroke=1;
this.ports=new draw2d.ArrayList();
draw2d.Figure.call(this);
};
draw2d.Node.prototype=new draw2d.Figure();
draw2d.Node.prototype.type="draw2d.Node";
draw2d.Node.prototype.dispose=function(){
for(var i=0;i<this.ports.getSize();i++){
this.ports.get(i).dispose();
}
this.ports=null;
draw2d.Figure.prototype.dispose.call(this);
};
draw2d.Node.prototype.createHTMLElement=function(){
var item=draw2d.Figure.prototype.createHTMLElement.call(this);
item.style.width="auto";
item.style.height="auto";
item.style.margin="0px";
item.style.padding="0px";
if(this.lineColor!==null){
item.style.border=this.lineStroke+"px solid "+this.lineColor.getHTMLStyle();
}
item.style.fontSize="1px";
if(this.bgColor!==null){
item.style.backgroundColor=this.bgColor.getHTMLStyle();
}
return item;
};
draw2d.Node.prototype.paint=function(){
draw2d.Figure.prototype.paint.call(this);
for(var i=0;i<this.ports.getSize();i++){
this.ports.get(i).paint();
}
};
draw2d.Node.prototype.getPorts=function(){
return this.ports;
};
draw2d.Node.prototype.getInputPorts=function(){
var _4db6=new draw2d.ArrayList();
for(var i=0;i<this.ports.getSize();i++){
var port=this.ports.get(i);
if(port instanceof draw2d.InputPort){
_4db6.add(port);
}
}
return _4db6;
};
draw2d.Node.prototype.getOutputPorts=function(){
var _4db9=new draw2d.ArrayList();
for(var i=0;i<this.ports.getSize();i++){
var port=this.ports.get(i);
if(port instanceof draw2d.OutputPort){
_4db9.add(port);
}
}
return _4db9;
};
draw2d.Node.prototype.getPort=function(_4dbc){
if(this.ports===null){
return null;
}
for(var i=0;i<this.ports.getSize();i++){
var port=this.ports.get(i);
if(port.getName()==_4dbc){
return port;
}
}
};
draw2d.Node.prototype.getInputPort=function(_4dbf){
if(this.ports===null){
return null;
}
for(var i=0;i<this.ports.getSize();i++){
var port=this.ports.get(i);
if(port.getName()==_4dbf&&port instanceof draw2d.InputPort){
return port;
}
}
};
draw2d.Node.prototype.getOutputPort=function(_4dc2){
if(this.ports===null){
return null;
}
for(var i=0;i<this.ports.getSize();i++){
var port=this.ports.get(i);
if(port.getName()==_4dc2&&port instanceof draw2d.OutputPort){
return port;
}
}
};
draw2d.Node.prototype.addPort=function(port,x,y){
this.ports.add(port);
port.setOrigin(x,y);
port.setPosition(x,y);
port.setParent(this);
port.setDeleteable(false);
this.html.appendChild(port.getHTMLElement());
if(this.workflow!==null){
this.workflow.registerPort(port);
}
};
draw2d.Node.prototype.removePort=function(port){
if(this.ports!==null){
this.ports.remove(port);
}
try{
this.html.removeChild(port.getHTMLElement());
}
catch(exc){
}
if(this.workflow!==null){
this.workflow.unregisterPort(port);
}
var _4dc9=port.getConnections();
for(var i=0;i<_4dc9.getSize();++i){
this.workflow.removeFigure(_4dc9.get(i));
}
};
draw2d.Node.prototype.setWorkflow=function(_4dcb){
var _4dcc=this.workflow;
draw2d.Figure.prototype.setWorkflow.call(this,_4dcb);
if(_4dcc!==null){
for(var i=0;i<this.ports.getSize();i++){
_4dcc.unregisterPort(this.ports.get(i));
}
}
if(this.workflow!==null){
for(var i=0;i<this.ports.getSize();i++){
this.workflow.registerPort(this.ports.get(i));
}
}
};
draw2d.Node.prototype.setBackgroundColor=function(color){
this.bgColor=color;
if(this.bgColor!==null){
this.html.style.backgroundColor=this.bgColor.getHTMLStyle();
}else{
this.html.style.backgroundColor="transparent";
}
};
draw2d.Node.prototype.getBackgroundColor=function(){
return this.bgColor;
};
draw2d.Node.prototype.setColor=function(color){
this.lineColor=color;
if(this.lineColor!==null){
this.html.style.border=this.lineStroke+"px solid "+this.lineColor.getHTMLStyle();
}else{
this.html.style.border="0px";
}
};
draw2d.Node.prototype.setLineWidth=function(w){
this.lineStroke=w;
if(this.lineColor!==null){
this.html.style.border=this.lineStroke+"px solid "+this.lineColor.getHTMLStyle();
}else{
this.html.style.border="0px";
}
};
draw2d.Node.prototype.getModelSourceConnections=function(){
throw "You must override the method [Node.prototype.getModelSourceConnections]";
};
draw2d.Node.prototype.refreshConnections=function(){
if(this.workflow!==null){
this.workflow.refreshConnections(this);
}
};
draw2d.VectorFigure=function(){
this.bgColor=null;
this.lineColor=new draw2d.Color(0,0,0);
this.stroke=1;
this.graphics=null;
draw2d.Node.call(this);
};
draw2d.VectorFigure.prototype=new draw2d.Node;
draw2d.VectorFigure.prototype.type="draw2d.VectorFigure";
draw2d.VectorFigure.prototype.dispose=function(){
draw2d.Node.prototype.dispose.call(this);
this.bgColor=null;
this.lineColor=null;
if(this.graphics!==null){
this.graphics.clear();
}
this.graphics=null;
};
draw2d.VectorFigure.prototype.createHTMLElement=function(){
var item=draw2d.Node.prototype.createHTMLElement.call(this);
item.style.border="0px";
item.style.backgroundColor="transparent";
return item;
};
draw2d.VectorFigure.prototype.setWorkflow=function(_509b){
draw2d.Node.prototype.setWorkflow.call(this,_509b);
if(this.workflow===null){
this.graphics.clear();
this.graphics=null;
}
};
draw2d.VectorFigure.prototype.paint=function(){
if(this.html===null){
return;
}
try{
if(this.graphics===null){
this.graphics=new jsGraphics(this.html);
}else{
this.graphics.clear();
}
draw2d.Node.prototype.paint.call(this);
for(var i=0;i<this.ports.getSize();i++){
this.getHTMLElement().appendChild(this.ports.get(i).getHTMLElement());
}
}
catch(e){
pushErrorStack(e,"draw2d.VectorFigure.prototype.paint=function()["+area+"]");
}
};
draw2d.VectorFigure.prototype.setDimension=function(w,h){
draw2d.Node.prototype.setDimension.call(this,w,h);
if(this.graphics!==null){
this.paint();
}
};
draw2d.VectorFigure.prototype.setBackgroundColor=function(color){
this.bgColor=color;
if(this.graphics!==null){
this.paint();
}
};
draw2d.VectorFigure.prototype.getBackgroundColor=function(){
return this.bgColor;
};
draw2d.VectorFigure.prototype.setLineWidth=function(w){
this.stroke=w;
if(this.graphics!==null){
this.paint();
}
};
draw2d.VectorFigure.prototype.setColor=function(color){
this.lineColor=color;
if(this.graphics!==null){
this.paint();
}
};
draw2d.VectorFigure.prototype.getColor=function(){
return this.lineColor;
};
draw2d.SVGFigure=function(width,_5181){
this.bgColor=null;
this.lineColor=new draw2d.Color(0,0,0);
this.stroke=1;
this.context=null;
draw2d.Node.call(this);
if(width&&_5181){
this.setDimension(width,_5181);
}
};
draw2d.SVGFigure.prototype=new draw2d.Node();
draw2d.SVGFigure.prototype.type="draw2d.SVGFigure";
draw2d.SVGFigure.prototype.createHTMLElement=function(){
var item=new MooCanvas(this.id,{width:100,height:100});
item.style.position="absolute";
item.style.left=this.x+"px";
item.style.top=this.y+"px";
item.style.zIndex=""+draw2d.Figure.ZOrderBaseIndex;
this.context=item.getContext("2d");
return item;
};
draw2d.SVGFigure.prototype.paint=function(){
this.context.clearRect(0,0,this.getWidth(),this.getHeight());
this.context.fillStyle="rgba(200,0,0,0.3)";
this.context.fillRect(0,0,this.getWidth(),this.getHeight());
};
draw2d.SVGFigure.prototype.setDimension=function(w,h){
draw2d.Node.prototype.setDimension.call(this,w,h);
this.html.width=w;
this.html.height=h;
this.html.style.width=w+"px";
this.html.style.height=h+"px";
if(this.context!==null){
if(this.context.element){
this.context.element.style.width=w+"px";
this.context.element.style.height=h+"px";
}
this.paint();
}
};
draw2d.SVGFigure.prototype.setBackgroundColor=function(color){
this.bgColor=color;
if(this.graphics!==null){
this.paint();
}
};
draw2d.SVGFigure.prototype.getBackgroundColor=function(){
return this.bgColor;
};
draw2d.SVGFigure.prototype.setLineWidth=function(w){
this.stroke=w;
if(this.context!==null){
this.paint();
}
};
draw2d.SVGFigure.prototype.setColor=function(color){
this.lineColor=color;
if(this.context!==null){
this.paint();
}
};
draw2d.SVGFigure.prototype.getColor=function(){
return this.lineColor;
};
draw2d.Label=function(msg){
this.msg=msg;
this.bgColor=null;
this.color=new draw2d.Color(0,0,0);
this.fontSize=10;
this.textNode=null;
this.align="center";
draw2d.Figure.call(this);
};
draw2d.Label.prototype=new draw2d.Figure();
draw2d.Label.prototype.type="draw2d.Label";
draw2d.Label.prototype.createHTMLElement=function(){
var item=draw2d.Figure.prototype.createHTMLElement.call(this);
this.textNode=document.createTextNode(this.msg);
item.appendChild(this.textNode);
item.style.color=this.color.getHTMLStyle();
item.style.fontSize=this.fontSize+"pt";
item.style.width="auto";
item.style.height="auto";
item.style.paddingLeft="3px";
item.style.paddingRight="3px";
item.style.textAlign=this.align;
item.style.MozUserSelect="none";
this.disableTextSelection(item);
if(this.bgColor!==null){
item.style.backgroundColor=this.bgColor.getHTMLStyle();
}
return item;
};
draw2d.Label.prototype.isResizeable=function(){
return false;
};
draw2d.Label.prototype.setWordwrap=function(flag){
this.html.style.whiteSpace=flag?"wrap":"nowrap";
};
draw2d.Label.prototype.setAlign=function(align){
this.align=align;
this.html.style.textAlign=align;
};
draw2d.Label.prototype.setBackgroundColor=function(color){
this.bgColor=color;
if(this.bgColor!==null){
this.html.style.backgroundColor=this.bgColor.getHTMLStyle();
}else{
this.html.style.backgroundColor="transparent";
}
};
draw2d.Label.prototype.setColor=function(color){
this.color=color;
this.html.style.color=this.color.getHTMLStyle();
};
draw2d.Label.prototype.setFontSize=function(size){
this.fontSize=size;
this.html.style.fontSize=this.fontSize+"pt";
};
draw2d.Label.prototype.setDimension=function(w,h){
};
draw2d.Label.prototype.getWidth=function(){
if(window.getComputedStyle){
return parseInt(getComputedStyle(this.html,"").getPropertyValue("width"));
}
return parseInt(this.html.clientWidth);
};
draw2d.Label.prototype.getHeight=function(){
if(window.getComputedStyle){
return parseInt(getComputedStyle(this.html,"").getPropertyValue("height"));
}
return parseInt(this.html.clientHeight);
};
draw2d.Label.prototype.getText=function(){
return this.msg;
};
draw2d.Label.prototype.setText=function(text){
this.msg=text;
this.html.removeChild(this.textNode);
this.textNode=document.createTextNode(this.msg);
this.html.appendChild(this.textNode);
};
draw2d.Label.prototype.setStyledText=function(text){
this.msg=text;
this.html.removeChild(this.textNode);
this.textNode=document.createElement("div");
this.textNode.style.whiteSpace="nowrap";
this.textNode.innerHTML=text;
this.html.appendChild(this.textNode);
};
draw2d.Oval=function(){
draw2d.VectorFigure.call(this);
};
draw2d.Oval.prototype=new draw2d.VectorFigure();
draw2d.Oval.prototype.type="draw2d.Oval";
draw2d.Oval.prototype.paint=function(){
if(this.html===null){
return;
}
try{
draw2d.VectorFigure.prototype.paint.call(this);
this.graphics.setStroke(this.stroke);
if(this.bgColor!==null){
this.graphics.setColor(this.bgColor.getHTMLStyle());
this.graphics.fillOval(0,0,this.getWidth()-1,this.getHeight()-1);
}
if(this.lineColor!==null){
this.graphics.setColor(this.lineColor.getHTMLStyle());
this.graphics.drawOval(0,0,this.getWidth()-1,this.getHeight()-1);
}
this.graphics.paint();
}
catch(e){
pushErrorStack(e,"draw2d.Oval.prototype.paint=function()");
}
};
draw2d.Circle=function(_51a8){
draw2d.Oval.call(this);
if(_51a8){
this.setDimension(_51a8,_51a8);
}
};
draw2d.Circle.prototype=new draw2d.Oval();
draw2d.Circle.prototype.type="draw2d.Circle";
draw2d.Circle.prototype.setDimension=function(w,h){
if(w>h){
draw2d.Oval.prototype.setDimension.call(this,w,w);
}else{
draw2d.Oval.prototype.setDimension.call(this,h,h);
}
};
draw2d.Circle.prototype.isStrechable=function(){
return false;
};
draw2d.Rectangle=function(width,_50a3){
this.bgColor=null;
this.lineColor=new draw2d.Color(0,0,0);
this.lineStroke=1;
draw2d.Figure.call(this);
if(width&&_50a3){
this.setDimension(width,_50a3);
}
};
draw2d.Rectangle.prototype=new draw2d.Figure();
draw2d.Rectangle.prototype.type="draw2d.Rectangle";
draw2d.Rectangle.prototype.dispose=function(){
draw2d.Figure.prototype.dispose.call(this);
this.bgColor=null;
this.lineColor=null;
};
draw2d.Rectangle.prototype.createHTMLElement=function(){
var item=draw2d.Figure.prototype.createHTMLElement.call(this);
item.style.width="auto";
item.style.height="auto";
item.style.margin="0px";
item.style.padding="0px";
item.style.border=this.lineStroke+"px solid "+this.lineColor.getHTMLStyle();
item.style.fontSize="1px";
item.style.lineHeight="1px";
item.innerHTML="&nbsp";
if(this.bgColor!==null){
item.style.backgroundColor=this.bgColor.getHTMLStyle();
}
return item;
};
draw2d.Rectangle.prototype.setBackgroundColor=function(color){
this.bgColor=color;
if(this.bgColor!==null){
this.html.style.backgroundColor=this.bgColor.getHTMLStyle();
}else{
this.html.style.backgroundColor="transparent";
}
};
draw2d.Rectangle.prototype.getBackgroundColor=function(){
return this.bgColor;
};
draw2d.Rectangle.prototype.setColor=function(color){
this.lineColor=color;
if(this.lineColor!==null){
this.html.style.border=this.lineStroke+"px solid "+this.lineColor.getHTMLStyle();
}else{
this.html.style.border=this.lineStroke+"0px";
}
};
draw2d.Rectangle.prototype.getColor=function(){
return this.lineColor;
};
draw2d.Rectangle.prototype.getWidth=function(){
return draw2d.Figure.prototype.getWidth.call(this)+2*this.lineStroke;
};
draw2d.Rectangle.prototype.getHeight=function(){
return draw2d.Figure.prototype.getHeight.call(this)+2*this.lineStroke;
};
draw2d.Rectangle.prototype.setDimension=function(w,h){
draw2d.Figure.prototype.setDimension.call(this,w-2*this.lineStroke,h-2*this.lineStroke);
};
draw2d.Rectangle.prototype.setLineWidth=function(w){
var diff=w-this.lineStroke;
this.setDimension(this.getWidth()-2*diff,this.getHeight()-2*diff);
this.lineStroke=w;
var c="transparent";
if(this.lineColor!==null){
c=this.lineColor.getHTMLStyle();
}
this.html.style.border=this.lineStroke+"px solid "+c;
};
draw2d.Rectangle.prototype.getLineWidth=function(){
return this.lineStroke;
};
draw2d.ImageFigure=function(url){
if(url===undefined){
url=null;
}
this.url=url;
draw2d.Node.call(this);
this.setDimension(40,40);
};
draw2d.ImageFigure.prototype=new draw2d.Node;
draw2d.ImageFigure.prototype.type="draw2d.Image";
draw2d.ImageFigure.prototype.createHTMLElement=function(){
var item=draw2d.Node.prototype.createHTMLElement.call(this);
item.style.width=this.width+"px";
item.style.height=this.height+"px";
item.style.margin="0px";
item.style.padding="0px";
item.style.border="0px";
if(this.url!==null){
item.style.backgroundImage="url("+this.url+")";
}else{
item.style.backgroundImage="";
}
return item;
};
draw2d.ImageFigure.prototype.setColor=function(color){
};
draw2d.ImageFigure.prototype.isResizeable=function(){
return false;
};
draw2d.ImageFigure.prototype.setImage=function(url){
if(url===undefined){
url=null;
}
this.url=url;
if(this.url!==null){
this.html.style.backgroundImage="url("+this.url+")";
}else{
this.html.style.backgroundImage="";
}
};
draw2d.Port=function(_6041,_6042){
Corona=function(){
};
Corona.prototype=new draw2d.Circle();
Corona.prototype.setAlpha=function(_6043){
draw2d.Circle.prototype.setAlpha.call(this,Math.min(0.3,_6043));
this.setDeleteable(false);
this.setCanDrag(false);
this.setResizeable(false);
this.setSelectable(false);
};
if(_6041===null||_6041===undefined){
this.currentUIRepresentation=new draw2d.Circle();
}else{
this.currentUIRepresentation=_6041;
}
if(_6042===null||_6042===undefined){
this.connectedUIRepresentation=new draw2d.Circle();
this.connectedUIRepresentation.setColor(null);
}else{
this.connectedUIRepresentation=_6042;
}
this.disconnectedUIRepresentation=this.currentUIRepresentation;
this.hideIfConnected=false;
this.uiRepresentationAdded=true;
this.parentNode=null;
this.originX=0;
this.originY=0;
this.coronaWidth=10;
this.corona=null;
draw2d.Rectangle.call(this);
this.setDimension(8,8);
this.setBackgroundColor(new draw2d.Color(100,180,100));
this.setColor(new draw2d.Color(90,150,90));
draw2d.Rectangle.prototype.setColor.call(this,null);
this.dropable=new draw2d.DropTarget(this.html);
this.dropable.node=this;
this.dropable.addEventListener("dragenter",function(_6044){
_6044.target.node.onDragEnter(_6044.relatedTarget.node);
});
this.dropable.addEventListener("dragleave",function(_6045){
_6045.target.node.onDragLeave(_6045.relatedTarget.node);
});
this.dropable.addEventListener("drop",function(_6046){
_6046.relatedTarget.node.onDrop(_6046.target.node);
});
};
draw2d.Port.prototype=new draw2d.Rectangle();
draw2d.Port.prototype.type="draw2d.Port";
draw2d.Port.ZOrderBaseIndex=5000;
draw2d.Port.setZOrderBaseIndex=function(index){
draw2d.Port.ZOrderBaseIndex=index;
};
draw2d.Port.prototype.setHideIfConnected=function(flag){
this.hideIfConnected=flag;
};
draw2d.Port.prototype.dispose=function(){
var size=this.moveListener.getSize();
for(var i=0;i<size;i++){
var _604b=this.moveListener.get(i);
this.parentNode.workflow.removeFigure(_604b);
_604b.dispose();
}
draw2d.Rectangle.prototype.dispose.call(this);
this.parentNode=null;
this.dropable.node=null;
this.dropable=null;
this.disconnectedUIRepresentation.dispose();
this.connectedUIRepresentation.dispose();
};
draw2d.Port.prototype.createHTMLElement=function(){
var item=draw2d.Rectangle.prototype.createHTMLElement.call(this);
item.style.zIndex=draw2d.Port.ZOrderBaseIndex;
this.currentUIRepresentation.html.zIndex=draw2d.Port.ZOrderBaseIndex;
item.appendChild(this.currentUIRepresentation.html);
this.uiRepresentationAdded=true;
return item;
};
draw2d.Port.prototype.setUiRepresentation=function(_604d){
if(_604d===null){
_604d=new draw2d.Figure();
}
if(this.uiRepresentationAdded){
this.html.removeChild(this.currentUIRepresentation.getHTMLElement());
}
this.html.appendChild(_604d.getHTMLElement());
_604d.paint();
this.currentUIRepresentation=_604d;
};
draw2d.Port.prototype.onMouseEnter=function(){
this.setLineWidth(2);
};
draw2d.Port.prototype.onMouseLeave=function(){
this.setLineWidth(0);
};
draw2d.Port.prototype.setDimension=function(width,_604f){
draw2d.Rectangle.prototype.setDimension.call(this,width,_604f);
this.connectedUIRepresentation.setDimension(width,_604f);
this.disconnectedUIRepresentation.setDimension(width,_604f);
this.setPosition(this.x,this.y);
};
draw2d.Port.prototype.setBackgroundColor=function(color){
this.currentUIRepresentation.setBackgroundColor(color);
};
draw2d.Port.prototype.getBackgroundColor=function(){
return this.currentUIRepresentation.getBackgroundColor();
};
draw2d.Port.prototype.getConnections=function(){
var _6051=new draw2d.ArrayList();
var size=this.moveListener.getSize();
for(var i=0;i<size;i++){
var _6054=this.moveListener.get(i);
if(_6054 instanceof draw2d.Connection){
_6051.add(_6054);
}
}
return _6051;
};
draw2d.Port.prototype.setColor=function(color){
this.currentUIRepresentation.setColor(color);
};
draw2d.Port.prototype.getColor=function(){
return this.currentUIRepresentation.getColor();
};
draw2d.Port.prototype.setLineWidth=function(width){
this.currentUIRepresentation.setLineWidth(width);
};
draw2d.Port.prototype.getLineWidth=function(){
return this.currentUIRepresentation.getLineWidth();
};
draw2d.Port.prototype.paint=function(){
try{
this.currentUIRepresentation.paint();
}
catch(e){
pushErrorStack(e,"draw2d.Port.prototype.paint=function()");
}
};
draw2d.Port.prototype.setPosition=function(xPos,yPos){
this.originX=xPos;
this.originY=yPos;
draw2d.Rectangle.prototype.setPosition.call(this,xPos,yPos);
if(this.html===null){
return;
}
this.html.style.left=(this.x-this.getWidth()/2)+"px";
this.html.style.top=(this.y-this.getHeight()/2)+"px";
};
draw2d.Port.prototype.setParent=function(_6059){
if(this.parentNode!==null){
this.parentNode.detachMoveListener(this);
}
this.parentNode=_6059;
if(this.parentNode!==null){
this.parentNode.attachMoveListener(this);
}
};
draw2d.Port.prototype.attachMoveListener=function(_605a){
draw2d.Rectangle.prototype.attachMoveListener.call(this,_605a);
if(this.hideIfConnected==true){
this.setUiRepresentation(this.connectedUIRepresentation);
}
};
draw2d.Port.prototype.detachMoveListener=function(_605b){
draw2d.Rectangle.prototype.detachMoveListener.call(this,_605b);
if(this.getConnections().getSize()==0){
this.setUiRepresentation(this.disconnectedUIRepresentation);
}
};
draw2d.Port.prototype.getParent=function(){
return this.parentNode;
};
draw2d.Port.prototype.onDrag=function(){
draw2d.Rectangle.prototype.onDrag.call(this);
this.parentNode.workflow.showConnectionLine(this.parentNode.x+this.x,this.parentNode.y+this.y,this.parentNode.x+this.originX,this.parentNode.y+this.originY);
};
draw2d.Port.prototype.getCoronaWidth=function(){
return this.coronaWidth;
};
draw2d.Port.prototype.setCoronaWidth=function(width){
this.coronaWidth=width;
};
draw2d.Port.prototype.setOrigin=function(x,y){
this.originX=x;
this.originY=y;
};
draw2d.Port.prototype.onDragend=function(){
this.setAlpha(1);
this.setPosition(this.originX,this.originY);
this.parentNode.workflow.hideConnectionLine();
document.body.focus();
};
draw2d.Port.prototype.onDragEnter=function(port){
var _6060=new draw2d.EditPolicy(draw2d.EditPolicy.CONNECT);
_6060.canvas=this.parentNode.workflow;
_6060.source=port;
_6060.target=this;
var _6061=this.createCommand(_6060);
if(_6061===null){
return;
}
this.parentNode.workflow.connectionLine.setColor(new draw2d.Color(0,150,0));
this.parentNode.workflow.connectionLine.setLineWidth(3);
this.showCorona(true);
};
draw2d.Port.prototype.onDragLeave=function(port){
this.parentNode.workflow.connectionLine.setColor(new draw2d.Color(0,0,0));
this.parentNode.workflow.connectionLine.setLineWidth(1);
this.showCorona(false);
};
draw2d.Port.prototype.onDrop=function(port){
var _6064=new draw2d.EditPolicy(draw2d.EditPolicy.CONNECT);
_6064.canvas=this.parentNode.workflow;
_6064.source=port;
_6064.target=this;
var _6065=this.createCommand(_6064);
if(_6065!==null){
this.parentNode.workflow.getCommandStack().execute(_6065);
}
};
draw2d.Port.prototype.getAbsolutePosition=function(){
return new draw2d.Point(this.getAbsoluteX(),this.getAbsoluteY());
};
draw2d.Port.prototype.getAbsoluteBounds=function(){
return new draw2d.Dimension(this.getAbsoluteX(),this.getAbsoluteY(),this.getWidth(),this.getHeight());
};
draw2d.Port.prototype.getAbsoluteY=function(){
return this.originY+this.parentNode.getY();
};
draw2d.Port.prototype.getAbsoluteX=function(){
return this.originX+this.parentNode.getX();
};
draw2d.Port.prototype.onOtherFigureMoved=function(_6066){
this.fireMoveEvent();
};
draw2d.Port.prototype.getName=function(){
return this.name;
};
draw2d.Port.prototype.setName=function(name){
this.name=name;
};
draw2d.Port.prototype.isOver=function(iX,iY){
var x=this.getAbsoluteX()-this.coronaWidth-this.getWidth()/2;
var y=this.getAbsoluteY()-this.coronaWidth-this.getHeight()/2;
var iX2=x+this.width+(this.coronaWidth*2)+this.getWidth()/2;
var iY2=y+this.height+(this.coronaWidth*2)+this.getHeight()/2;
return (iX>=x&&iX<=iX2&&iY>=y&&iY<=iY2);
};
draw2d.Port.prototype.showCorona=function(flag,_606f){
if(flag===true){
this.corona=new Corona();
this.corona.setAlpha(0.3);
this.corona.setBackgroundColor(new draw2d.Color(0,125,125));
this.corona.setColor(null);
this.corona.setDimension(this.getWidth()+(this.getCoronaWidth()*2),this.getWidth()+(this.getCoronaWidth()*2));
this.parentNode.getWorkflow().addFigure(this.corona,this.getAbsoluteX()-this.getCoronaWidth()-this.getWidth()/2,this.getAbsoluteY()-this.getCoronaWidth()-this.getHeight()/2);
}else{
if(flag===false&&this.corona!==null){
this.parentNode.getWorkflow().removeFigure(this.corona);
this.corona=null;
}
}
};
draw2d.Port.prototype.createCommand=function(_6070){
if(_6070.getPolicy()===draw2d.EditPolicy.MOVE){
if(!this.canDrag){
return null;
}
return new draw2d.CommandMovePort(this);
}
if(_6070.getPolicy()===draw2d.EditPolicy.CONNECT){
if(_6070.source.parentNode.id===_6070.target.parentNode.id){
return null;
}else{
return new draw2d.CommandConnect(_6070.canvas,_6070.source,_6070.target);
}
}
return null;
};
draw2d.InputPort=function(_5a0e){
draw2d.Port.call(this,_5a0e);
};
draw2d.InputPort.prototype=new draw2d.Port();
draw2d.InputPort.prototype.type="draw2d.InputPort";
draw2d.InputPort.prototype.onDragstart=function(x,y){
if(!this.canDrag){
return false;
}
return true;
};
draw2d.InputPort.prototype.onDragEnter=function(port){
if(port instanceof draw2d.OutputPort){
draw2d.Port.prototype.onDragEnter.call(this,port);
}else{
if(port instanceof draw2d.LineStartResizeHandle){
var line=this.workflow.currentSelection;
if(line instanceof draw2d.Connection&&line.getSource() instanceof draw2d.InputPort){
draw2d.Port.prototype.onDragEnter.call(this,line.getTarget());
}
}else{
if(port instanceof draw2d.LineEndResizeHandle){
var line=this.workflow.currentSelection;
if(line instanceof draw2d.Connection&&line.getTarget() instanceof draw2d.InputPort){
draw2d.Port.prototype.onDragEnter.call(this,line.getSource());
}
}
}
}
};
draw2d.InputPort.prototype.onDragLeave=function(port){
if(port instanceof draw2d.OutputPort){
draw2d.Port.prototype.onDragLeave.call(this,port);
}else{
if(port instanceof draw2d.LineStartResizeHandle){
var line=this.workflow.currentSelection;
if(line instanceof draw2d.Connection&&line.getSource() instanceof draw2d.InputPort){
draw2d.Port.prototype.onDragLeave.call(this,line.getTarget());
}
}else{
if(port instanceof draw2d.LineEndResizeHandle){
var line=this.workflow.currentSelection;
if(line instanceof draw2d.Connection&&line.getTarget() instanceof draw2d.InputPort){
draw2d.Port.prototype.onDragLeave.call(this,line.getSource());
}
}
}
}
};
draw2d.InputPort.prototype.createCommand=function(_5a15){
if(_5a15.getPolicy()==draw2d.EditPolicy.CONNECT){
if(_5a15.source.parentNode.id==_5a15.target.parentNode.id){
return null;
}
if(_5a15.source instanceof draw2d.OutputPort){
return new draw2d.CommandConnect(_5a15.canvas,_5a15.source,_5a15.target);
}
return null;
}
return draw2d.Port.prototype.createCommand.call(this,_5a15);
};
draw2d.OutputPort=function(_519a){
draw2d.Port.call(this,_519a);
this.maxFanOut=100;
};
draw2d.OutputPort.prototype=new draw2d.Port();
draw2d.OutputPort.prototype.type="draw2d.OutputPort";
draw2d.OutputPort.prototype.onDragEnter=function(port){
if(this.getMaxFanOut()<=this.getFanOut()){
return;
}
if(port instanceof draw2d.InputPort){
draw2d.Port.prototype.onDragEnter.call(this,port);
}else{
if(port instanceof draw2d.LineStartResizeHandle){
var line=this.workflow.currentSelection;
if(line instanceof draw2d.Connection&&line.getSource() instanceof draw2d.OutputPort){
draw2d.Port.prototype.onDragEnter.call(this,line.getTarget());
}
}else{
if(port instanceof draw2d.LineEndResizeHandle){
var line=this.workflow.currentSelection;
if(line instanceof draw2d.Connection&&line.getTarget() instanceof draw2d.OutputPort){
draw2d.Port.prototype.onDragEnter.call(this,line.getSource());
}
}
}
}
};
draw2d.OutputPort.prototype.onDragLeave=function(port){
if(port instanceof draw2d.InputPort){
draw2d.Port.prototype.onDragLeave.call(this,port);
}else{
if(port instanceof draw2d.LineStartResizeHandle){
var line=this.workflow.currentSelection;
if(line instanceof draw2d.Connection&&line.getSource() instanceof draw2d.OutputPort){
draw2d.Port.prototype.onDragLeave.call(this,line.getTarget());
}
}else{
if(port instanceof draw2d.LineEndResizeHandle){
var line=this.workflow.currentSelection;
if(line instanceof draw2d.Connection&&line.getTarget() instanceof draw2d.OutputPort){
draw2d.Port.prototype.onDragLeave.call(this,line.getSource());
}
}
}
}
};
draw2d.OutputPort.prototype.onDragstart=function(x,y){
if(!this.canDrag){
return false;
}
if(this.maxFanOut===-1){
return true;
}
if(this.getMaxFanOut()<=this.getFanOut()){
return false;
}
return true;
};
draw2d.OutputPort.prototype.setMaxFanOut=function(count){
this.maxFanOut=count;
};
draw2d.OutputPort.prototype.getMaxFanOut=function(){
return this.maxFanOut;
};
draw2d.OutputPort.prototype.getFanOut=function(){
if(this.getParent().workflow===null){
return 0;
}
var count=0;
var lines=this.getParent().workflow.getLines();
var size=lines.getSize();
for(var i=0;i<size;i++){
var line=lines.get(i);
if(line instanceof draw2d.Connection){
if(line.getSource()==this){
count++;
}else{
if(line.getTarget()==this){
count++;
}
}
}
}
return count;
};
draw2d.OutputPort.prototype.createCommand=function(_51a7){
if(_51a7.getPolicy()===draw2d.EditPolicy.CONNECT){
if(_51a7.source.parentNode.id===_51a7.target.parentNode.id){
return null;
}
if(_51a7.source instanceof draw2d.InputPort){
return new draw2d.CommandConnect(_51a7.canvas,_51a7.target,_51a7.source);
}
return null;
}
return draw2d.Port.prototype.createCommand.call(this,_51a7);
};
draw2d.Line=function(){
this.lineColor=new draw2d.Color(0,0,0);
this.stroke=1;
this.canvas=null;
this.parent=null;
this.workflow=null;
this.html=null;
this.graphics=null;
this.id=draw2d.UUID.create();
this.startX=30;
this.startY=30;
this.endX=100;
this.endY=100;
this.alpha=1;
this.isMoving=false;
this.model=null;
this.zOrder=draw2d.Line.ZOrderBaseIndex;
this.corona=draw2d.Line.CoronaWidth;
this.properties={};
this.moveListener=new draw2d.ArrayList();
this.setSelectable(true);
this.setDeleteable(true);
};
draw2d.Line.prototype.type="draw2d.Line";
draw2d.Line.ZOrderBaseIndex=200;
draw2d.Line.ZOrderBaseIndex=200;
draw2d.Line.CoronaWidth=5;
draw2d.Line.setZOrderBaseIndex=function(index){
draw2d.Line.ZOrderBaseIndex=index;
};
draw2d.Line.setDefaultCoronaWidth=function(width){
draw2d.Line.CoronaWidth=width;
};
draw2d.Line.prototype.dispose=function(){
this.canvas=null;
this.workflow=null;
if(this.graphics!==null){
this.graphics.clear();
}
this.graphics=null;
};
draw2d.Line.prototype.getZOrder=function(){
return this.zOrder;
};
draw2d.Line.prototype.setZOrder=function(index){
if(this.html!==null){
this.html.style.zIndex=index;
}
this.zOrder=index;
};
draw2d.Line.prototype.setCoronaWidth=function(width){
this.corona=width;
};
draw2d.Line.prototype.createHTMLElement=function(){
var item=document.createElement("div");
item.id=this.id;
item.style.position="absolute";
item.style.left="0px";
item.style.top="0px";
item.style.height="0px";
item.style.width="0px";
item.style.zIndex=this.zOrder;
return item;
};
draw2d.Line.prototype.setId=function(id){
this.id=id;
if(this.html!==null){
this.html.id=id;
}
};
draw2d.Line.prototype.getId=function(){
return this.id;
};
draw2d.Line.prototype.getProperties=function(){
return this.properties;
};
draw2d.Line.prototype.getProperty=function(key){
return this.properties[key];
};
draw2d.Line.prototype.setProperty=function(key,value){
this.properties[key]=value;
this.setDocumentDirty();
};
draw2d.Line.prototype.getHTMLElement=function(){
if(this.html===null){
this.html=this.createHTMLElement();
}
return this.html;
};
draw2d.Line.prototype.getWorkflow=function(){
return this.workflow;
};
draw2d.Line.prototype.isResizeable=function(){
return true;
};
draw2d.Line.prototype.setCanvas=function(_50d3){
this.canvas=_50d3;
if(this.graphics!==null){
this.graphics.clear();
}
this.graphics=null;
};
draw2d.Line.prototype.setWorkflow=function(_50d4){
this.workflow=_50d4;
if(this.graphics!==null){
this.graphics.clear();
}
this.graphics=null;
};
draw2d.Line.prototype.paint=function(){
if(this.html===null){
return;
}
try{
if(this.graphics===null){
this.graphics=new jsGraphics(this.html);
}else{
this.graphics.clear();
}
this.graphics.setStroke(this.stroke);
this.graphics.setColor(this.lineColor.getHTMLStyle());
this.graphics.drawLine(this.startX,this.startY,this.endX,this.endY);
this.graphics.paint();
}
catch(e){
pushErrorStack(e,"draw2d.Line.prototype.paint=function()");
}
};
draw2d.Line.prototype.attachMoveListener=function(_50d5){
this.moveListener.add(_50d5);
};
draw2d.Line.prototype.detachMoveListener=function(_50d6){
this.moveListener.remove(_50d6);
};
draw2d.Line.prototype.fireMoveEvent=function(){
var size=this.moveListener.getSize();
for(var i=0;i<size;i++){
this.moveListener.get(i).onOtherFigureMoved(this);
}
};
draw2d.Line.prototype.onOtherFigureMoved=function(_50d9){
};
draw2d.Line.prototype.setLineWidth=function(w){
this.stroke=w;
if(this.graphics!==null){
this.paint();
}
this.setDocumentDirty();
};
draw2d.Line.prototype.setColor=function(color){
this.lineColor=color;
if(this.graphics!==null){
this.paint();
}
this.setDocumentDirty();
};
draw2d.Line.prototype.getColor=function(){
return this.lineColor;
};
draw2d.Line.prototype.setAlpha=function(_50dc){
if(_50dc==this.alpha){
return;
}
try{
this.html.style.MozOpacity=_50dc;
}
catch(exc1){
}
try{
this.html.style.opacity=_50dc;
}
catch(exc2){
}
try{
var _50dd=Math.round(_50dc*100);
if(_50dd>=99){
this.html.style.filter="";
}else{
this.html.style.filter="alpha(opacity="+_50dd+")";
}
}
catch(exc3){
}
this.alpha=_50dc;
};
draw2d.Line.prototype.setStartPoint=function(x,y){
this.startX=x;
this.startY=y;
if(this.graphics!==null){
this.paint();
}
this.setDocumentDirty();
};
draw2d.Line.prototype.setEndPoint=function(x,y){
this.endX=x;
this.endY=y;
if(this.graphics!==null){
this.paint();
}
this.setDocumentDirty();
};
draw2d.Line.prototype.getStartX=function(){
return this.startX;
};
draw2d.Line.prototype.getStartY=function(){
return this.startY;
};
draw2d.Line.prototype.getStartPoint=function(){
return new draw2d.Point(this.startX,this.startY);
};
draw2d.Line.prototype.getEndX=function(){
return this.endX;
};
draw2d.Line.prototype.getEndY=function(){
return this.endY;
};
draw2d.Line.prototype.getEndPoint=function(){
return new draw2d.Point(this.endX,this.endY);
};
draw2d.Line.prototype.isSelectable=function(){
return this.selectable;
};
draw2d.Line.prototype.setSelectable=function(flag){
this.selectable=flag;
};
draw2d.Line.prototype.isDeleteable=function(){
return this.deleteable;
};
draw2d.Line.prototype.setDeleteable=function(flag){
this.deleteable=flag;
};
draw2d.Line.prototype.getLength=function(){
return Math.sqrt((this.startX-this.endX)*(this.startX-this.endX)+(this.startY-this.endY)*(this.startY-this.endY));
};
draw2d.Line.prototype.getAngle=function(){
var _50e4=this.getLength();
var angle=-(180/Math.PI)*Math.asin((this.startY-this.endY)/_50e4);
if(angle<0){
if(this.endX<this.startX){
angle=Math.abs(angle)+180;
}else{
angle=360-Math.abs(angle);
}
}else{
if(this.endX<this.startX){
angle=180-angle;
}
}
return angle;
};
draw2d.Line.prototype.createCommand=function(_50e6){
if(_50e6.getPolicy()==draw2d.EditPolicy.MOVE){
var x1=this.getStartX();
var y1=this.getStartY();
var x2=this.getEndX();
var y2=this.getEndY();
return new draw2d.CommandMoveLine(this,x1,y1,x2,y2);
}
if(_50e6.getPolicy()==draw2d.EditPolicy.DELETE){
if(this.isDeleteable()==false){
return null;
}
return new draw2d.CommandDelete(this);
}
return null;
};
draw2d.Line.prototype.setModel=function(model){
if(this.model!==null){
this.model.removePropertyChangeListener(this);
}
this.model=model;
if(this.model!==null){
this.model.addPropertyChangeListener(this);
}
};
draw2d.Line.prototype.getModel=function(){
return this.model;
};
draw2d.Line.prototype.onRemove=function(_50ec){
};
draw2d.Line.prototype.onContextMenu=function(x,y){
var menu=this.getContextMenu();
if(menu!==null){
this.workflow.showMenu(menu,x,y);
}
};
draw2d.Line.prototype.getContextMenu=function(){
return null;
};
draw2d.Line.prototype.onDoubleClick=function(){
};
draw2d.Line.prototype.setDocumentDirty=function(){
if(this.workflow!==null){
this.workflow.setDocumentDirty();
}
};
draw2d.Line.prototype.containsPoint=function(px,py){
return draw2d.Line.hit(this.corona,this.startX,this.startY,this.endX,this.endY,px,py);
};
draw2d.Line.hit=function(_50f2,X1,Y1,X2,Y2,px,py){
X2-=X1;
Y2-=Y1;
px-=X1;
py-=Y1;
var _50f9=px*X2+py*Y2;
var _50fa;
if(_50f9<=0){
_50fa=0;
}else{
px=X2-px;
py=Y2-py;
_50f9=px*X2+py*Y2;
if(_50f9<=0){
_50fa=0;
}else{
_50fa=_50f9*_50f9/(X2*X2+Y2*Y2);
}
}
var lenSq=px*px+py*py-_50fa;
if(lenSq<0){
lenSq=0;
}
return Math.sqrt(lenSq)<_50f2;
};
draw2d.ConnectionRouter=function(){
};
draw2d.ConnectionRouter.prototype.type="draw2d.ConnectionRouter";
draw2d.ConnectionRouter.prototype.getDirection=function(r,p){
var _631a=Math.abs(r.x-p.x);
var _631b=3;
var i=Math.abs(r.y-p.y);
if(i<=_631a){
_631a=i;
_631b=0;
}
i=Math.abs(r.getBottom()-p.y);
if(i<=_631a){
_631a=i;
_631b=2;
}
i=Math.abs(r.getRight()-p.x);
if(i<_631a){
_631a=i;
_631b=1;
}
return _631b;
};
draw2d.ConnectionRouter.prototype.getEndDirection=function(conn){
var p=conn.getEndPoint();
var rect=conn.getTarget().getParent().getBounds();
return this.getDirection(rect,p);
};
draw2d.ConnectionRouter.prototype.getStartDirection=function(conn){
var p=conn.getStartPoint();
var rect=conn.getSource().getParent().getBounds();
return this.getDirection(rect,p);
};
draw2d.ConnectionRouter.prototype.route=function(_6323){
};
draw2d.NullConnectionRouter=function(){
};
draw2d.NullConnectionRouter.prototype=new draw2d.ConnectionRouter();
draw2d.NullConnectionRouter.prototype.type="draw2d.NullConnectionRouter";
draw2d.NullConnectionRouter.prototype.invalidate=function(){
};
draw2d.NullConnectionRouter.prototype.route=function(_53f0){
_53f0.addPoint(_53f0.getStartPoint());
_53f0.addPoint(_53f0.getEndPoint());
};
draw2d.ManhattanConnectionRouter=function(){
this.MINDIST=20;
};
draw2d.ManhattanConnectionRouter.prototype=new draw2d.ConnectionRouter();
draw2d.ManhattanConnectionRouter.prototype.type="draw2d.ManhattanConnectionRouter";
draw2d.ManhattanConnectionRouter.prototype.route=function(conn){
var _4c73=conn.getStartPoint();
var _4c74=this.getStartDirection(conn);
var toPt=conn.getEndPoint();
var toDir=this.getEndDirection(conn);
this._route(conn,toPt,toDir,_4c73,_4c74);
};
draw2d.ManhattanConnectionRouter.prototype._route=function(conn,_4c78,_4c79,toPt,toDir){
var TOL=0.1;
var _4c7d=0.01;
var UP=0;
var RIGHT=1;
var DOWN=2;
var LEFT=3;
var xDiff=_4c78.x-toPt.x;
var yDiff=_4c78.y-toPt.y;
var point;
var dir;
if(((xDiff*xDiff)<(_4c7d))&&((yDiff*yDiff)<(_4c7d))){
conn.addPoint(new draw2d.Point(toPt.x,toPt.y));
return;
}
if(_4c79==LEFT){
if((xDiff>0)&&((yDiff*yDiff)<TOL)&&(toDir===RIGHT)){
point=toPt;
dir=toDir;
}else{
if(xDiff<0){
point=new draw2d.Point(_4c78.x-this.MINDIST,_4c78.y);
}else{
if(((yDiff>0)&&(toDir===DOWN))||((yDiff<0)&&(toDir==UP))){
point=new draw2d.Point(toPt.x,_4c78.y);
}else{
if(_4c79==toDir){
var pos=Math.min(_4c78.x,toPt.x)-this.MINDIST;
point=new draw2d.Point(pos,_4c78.y);
}else{
point=new draw2d.Point(_4c78.x-(xDiff/2),_4c78.y);
}
}
}
if(yDiff>0){
dir=UP;
}else{
dir=DOWN;
}
}
}else{
if(_4c79==RIGHT){
if((xDiff<0)&&((yDiff*yDiff)<TOL)&&(toDir===LEFT)){
point=toPt;
dir=toDir;
}else{
if(xDiff>0){
point=new draw2d.Point(_4c78.x+this.MINDIST,_4c78.y);
}else{
if(((yDiff>0)&&(toDir===DOWN))||((yDiff<0)&&(toDir===UP))){
point=new draw2d.Point(toPt.x,_4c78.y);
}else{
if(_4c79==toDir){
var pos=Math.max(_4c78.x,toPt.x)+this.MINDIST;
point=new draw2d.Point(pos,_4c78.y);
}else{
point=new draw2d.Point(_4c78.x-(xDiff/2),_4c78.y);
}
}
}
if(yDiff>0){
dir=UP;
}else{
dir=DOWN;
}
}
}else{
if(_4c79==DOWN){
if(((xDiff*xDiff)<TOL)&&(yDiff<0)&&(toDir==UP)){
point=toPt;
dir=toDir;
}else{
if(yDiff>0){
point=new draw2d.Point(_4c78.x,_4c78.y+this.MINDIST);
}else{
if(((xDiff>0)&&(toDir===RIGHT))||((xDiff<0)&&(toDir===LEFT))){
point=new draw2d.Point(_4c78.x,toPt.y);
}else{
if(_4c79===toDir){
var pos=Math.max(_4c78.y,toPt.y)+this.MINDIST;
point=new draw2d.Point(_4c78.x,pos);
}else{
point=new draw2d.Point(_4c78.x,_4c78.y-(yDiff/2));
}
}
}
if(xDiff>0){
dir=LEFT;
}else{
dir=RIGHT;
}
}
}else{
if(_4c79==UP){
if(((xDiff*xDiff)<TOL)&&(yDiff>0)&&(toDir===DOWN)){
point=toPt;
dir=toDir;
}else{
if(yDiff<0){
point=new draw2d.Point(_4c78.x,_4c78.y-this.MINDIST);
}else{
if(((xDiff>0)&&(toDir===RIGHT))||((xDiff<0)&&(toDir===LEFT))){
point=new draw2d.Point(_4c78.x,toPt.y);
}else{
if(_4c79===toDir){
var pos=Math.min(_4c78.y,toPt.y)-this.MINDIST;
point=new draw2d.Point(_4c78.x,pos);
}else{
point=new draw2d.Point(_4c78.x,_4c78.y-(yDiff/2));
}
}
}
if(xDiff>0){
dir=LEFT;
}else{
dir=RIGHT;
}
}
}
}
}
}
this._route(conn,point,dir,toPt,toDir);
conn.addPoint(_4c78);
};
draw2d.BezierConnectionRouter=function(_5b19){
if(!_5b19){
this.cheapRouter=new draw2d.ManhattanConnectionRouter();
}else{
this.cheapRouter=null;
}
this.iteration=5;
};
draw2d.BezierConnectionRouter.prototype=new draw2d.ConnectionRouter();
draw2d.BezierConnectionRouter.prototype.type="draw2d.BezierConnectionRouter";
draw2d.BezierConnectionRouter.prototype.drawBezier=function(_5b1a,_5b1b,t,iter){
var n=_5b1a.length-1;
var q=[];
var _5b20=n+1;
for(var i=0;i<_5b20;i++){
q[i]=[];
q[i][0]=_5b1a[i];
}
for(var j=1;j<=n;j++){
for(var i=0;i<=(n-j);i++){
q[i][j]=new draw2d.Point((1-t)*q[i][j-1].x+t*q[i+1][j-1].x,(1-t)*q[i][j-1].y+t*q[i+1][j-1].y);
}
}
var c1=[];
var c2=[];
for(var i=0;i<n+1;i++){
c1[i]=q[0][i];
c2[i]=q[i][n-i];
}
if(iter>=0){
this.drawBezier(c1,_5b1b,t,--iter);
this.drawBezier(c2,_5b1b,t,--iter);
}else{
for(var i=0;i<n;i++){
_5b1b.push(q[i][n-i]);
}
}
};
draw2d.BezierConnectionRouter.prototype.route=function(conn){
if(this.cheapRouter!==null&&(conn.getSource().getParent().isMoving===true||conn.getTarget().getParent().isMoving===true)){
this.cheapRouter.route(conn);
return;
}
var _5b26=[];
var _5b27=conn.getStartPoint();
var toPt=conn.getEndPoint();
this._route(_5b26,conn,toPt,this.getEndDirection(conn),_5b27,this.getStartDirection(conn));
var _5b29=[];
this.drawBezier(_5b26,_5b29,0.5,this.iteration);
for(var i=0;i<_5b29.length;i++){
conn.addPoint(_5b29[i]);
}
conn.addPoint(toPt);
};
draw2d.BezierConnectionRouter.prototype._route=function(_5b2b,conn,_5b2d,_5b2e,toPt,toDir){
var TOL=0.1;
var _5b32=0.01;
var _5b33=90;
var UP=0;
var RIGHT=1;
var DOWN=2;
var LEFT=3;
var xDiff=_5b2d.x-toPt.x;
var yDiff=_5b2d.y-toPt.y;
var point;
var dir;
if(((xDiff*xDiff)<(_5b32))&&((yDiff*yDiff)<(_5b32))){
_5b2b.push(new draw2d.Point(toPt.x,toPt.y));
return;
}
if(_5b2e===LEFT){
if((xDiff>0)&&((yDiff*yDiff)<TOL)&&(toDir===RIGHT)){
point=toPt;
dir=toDir;
}else{
if(xDiff<0){
point=new draw2d.Point(_5b2d.x-_5b33,_5b2d.y);
}else{
if(((yDiff>0)&&(toDir===DOWN))||((yDiff<0)&&(toDir===UP))){
point=new draw2d.Point(toPt.x,_5b2d.y);
}else{
if(_5b2e===toDir){
var pos=Math.min(_5b2d.x,toPt.x)-_5b33;
point=new draw2d.Point(pos,_5b2d.y);
}else{
point=new draw2d.Point(_5b2d.x-(xDiff/2),_5b2d.y);
}
}
}
if(yDiff>0){
dir=UP;
}else{
dir=DOWN;
}
}
}else{
if(_5b2e===RIGHT){
if((xDiff<0)&&((yDiff*yDiff)<TOL)&&(toDir==LEFT)){
point=toPt;
dir=toDir;
}else{
if(xDiff>0){
point=new draw2d.Point(_5b2d.x+_5b33,_5b2d.y);
}else{
if(((yDiff>0)&&(toDir===DOWN))||((yDiff<0)&&(toDir===UP))){
point=new draw2d.Point(toPt.x,_5b2d.y);
}else{
if(_5b2e===toDir){
var pos=Math.max(_5b2d.x,toPt.x)+_5b33;
point=new draw2d.Point(pos,_5b2d.y);
}else{
point=new draw2d.Point(_5b2d.x-(xDiff/2),_5b2d.y);
}
}
}
if(yDiff>0){
dir=UP;
}else{
dir=DOWN;
}
}
}else{
if(_5b2e===DOWN){
if(((xDiff*xDiff)<TOL)&&(yDiff<0)&&(toDir===UP)){
point=toPt;
dir=toDir;
}else{
if(yDiff>0){
point=new draw2d.Point(_5b2d.x,_5b2d.y+_5b33);
}else{
if(((xDiff>0)&&(toDir===RIGHT))||((xDiff<0)&&(toDir===LEFT))){
point=new draw2d.Point(_5b2d.x,toPt.y);
}else{
if(_5b2e===toDir){
var pos=Math.max(_5b2d.y,toPt.y)+_5b33;
point=new draw2d.Point(_5b2d.x,pos);
}else{
point=new draw2d.Point(_5b2d.x,_5b2d.y-(yDiff/2));
}
}
}
if(xDiff>0){
dir=LEFT;
}else{
dir=RIGHT;
}
}
}else{
if(_5b2e===UP){
if(((xDiff*xDiff)<TOL)&&(yDiff>0)&&(toDir===DOWN)){
point=toPt;
dir=toDir;
}else{
if(yDiff<0){
point=new draw2d.Point(_5b2d.x,_5b2d.y-_5b33);
}else{
if(((xDiff>0)&&(toDir===RIGHT))||((xDiff<0)&&(toDir===LEFT))){
point=new draw2d.Point(_5b2d.x,toPt.y);
}else{
if(_5b2e===toDir){
var pos=Math.min(_5b2d.y,toPt.y)-_5b33;
point=new draw2d.Point(_5b2d.x,pos);
}else{
point=new draw2d.Point(_5b2d.x,_5b2d.y-(yDiff/2));
}
}
}
if(xDiff>0){
dir=LEFT;
}else{
dir=RIGHT;
}
}
}
}
}
}
this._route(_5b2b,conn,point,dir,toPt,toDir);
_5b2b.push(_5b2d);
};
draw2d.FanConnectionRouter=function(){
};
draw2d.FanConnectionRouter.prototype=new draw2d.NullConnectionRouter();
draw2d.FanConnectionRouter.prototype.type="draw2d.FanConnectionRouter";
draw2d.FanConnectionRouter.prototype.route=function(conn){
var _60b1=conn.getStartPoint();
var toPt=conn.getEndPoint();
var lines=conn.getSource().getConnections();
var _60b4=new draw2d.ArrayList();
var index=0;
for(var i=0;i<lines.getSize();i++){
var _60b7=lines.get(i);
if(_60b7.getTarget()==conn.getTarget()||_60b7.getSource()==conn.getTarget()){
_60b4.add(_60b7);
if(conn==_60b7){
index=_60b4.getSize();
}
}
}
if(_60b4.getSize()>1){
this.routeCollision(conn,index);
}else{
draw2d.NullConnectionRouter.prototype.route.call(this,conn);
}
};
draw2d.FanConnectionRouter.prototype.routeNormal=function(conn){
conn.addPoint(conn.getStartPoint());
conn.addPoint(conn.getEndPoint());
};
draw2d.FanConnectionRouter.prototype.routeCollision=function(conn,index){
var start=conn.getStartPoint();
var end=conn.getEndPoint();
conn.addPoint(start);
var _60bd=10;
var _60be=new draw2d.Point((end.x+start.x)/2,(end.y+start.y)/2);
var _60bf=end.getPosition(start);
var ray;
if(_60bf==draw2d.PositionConstants.SOUTH||_60bf==draw2d.PositionConstants.EAST){
ray=new draw2d.Point(end.x-start.x,end.y-start.y);
}else{
ray=new draw2d.Point(start.x-end.x,start.y-end.y);
}
var _60c1=Math.sqrt(ray.x*ray.x+ray.y*ray.y);
var _60c2=_60bd*ray.x/_60c1;
var _60c3=_60bd*ray.y/_60c1;
var _60c4;
if(index%2===0){
_60c4=new draw2d.Point(_60be.x+(index/2)*(-1*_60c3),_60be.y+(index/2)*_60c2);
}else{
_60c4=new draw2d.Point(_60be.x+(index/2)*_60c3,_60be.y+(index/2)*(-1*_60c2));
}
conn.addPoint(_60c4);
conn.addPoint(end);
};
draw2d.Graphics=function(_5be2,_5be3,_5be4){
this.jsGraphics=_5be2;
this.xt=_5be4.x;
this.yt=_5be4.y;
this.radian=_5be3*Math.PI/180;
this.sinRadian=Math.sin(this.radian);
this.cosRadian=Math.cos(this.radian);
};
draw2d.Graphics.prototype.setStroke=function(x){
this.jsGraphics.setStroke(x);
};
draw2d.Graphics.prototype.drawLine=function(x1,y1,x2,y2){
var _x1=this.xt+x1*this.cosRadian-y1*this.sinRadian;
var _y1=this.yt+x1*this.sinRadian+y1*this.cosRadian;
var _x2=this.xt+x2*this.cosRadian-y2*this.sinRadian;
var _y2=this.yt+x2*this.sinRadian+y2*this.cosRadian;
this.jsGraphics.drawLine(_x1,_y1,_x2,_y2);
};
draw2d.Graphics.prototype.fillRect=function(x,y,w,h){
var x1=this.xt+x*this.cosRadian-y*this.sinRadian;
var y1=this.yt+x*this.sinRadian+y*this.cosRadian;
var x2=this.xt+(x+w)*this.cosRadian-y*this.sinRadian;
var y2=this.yt+(x+w)*this.sinRadian+y*this.cosRadian;
var x3=this.xt+(x+w)*this.cosRadian-(y+h)*this.sinRadian;
var y3=this.yt+(x+w)*this.sinRadian+(y+h)*this.cosRadian;
var x4=this.xt+x*this.cosRadian-(y+h)*this.sinRadian;
var y4=this.yt+x*this.sinRadian+(y+h)*this.cosRadian;
this.jsGraphics.fillPolygon([x1,x2,x3,x4],[y1,y2,y3,y4]);
};
draw2d.Graphics.prototype.fillPolygon=function(_5bfa,_5bfb){
var rotX=[];
var rotY=[];
for(var i=0;i<_5bfa.length;i++){
rotX[i]=this.xt+_5bfa[i]*this.cosRadian-_5bfb[i]*this.sinRadian;
rotY[i]=this.yt+_5bfa[i]*this.sinRadian+_5bfb[i]*this.cosRadian;
}
this.jsGraphics.fillPolygon(rotX,rotY);
};
draw2d.Graphics.prototype.setColor=function(color){
this.jsGraphics.setColor(color.getHTMLStyle());
};
draw2d.Graphics.prototype.drawPolygon=function(_5c00,_5c01){
var rotX=[];
var rotY=[];
for(var i=0;i<_5c00.length;i++){
rotX[i]=this.xt+_5c00[i]*this.cosRadian-_5c01[i]*this.sinRadian;
rotY[i]=this.yt+_5c00[i]*this.sinRadian+_5c01[i]*this.cosRadian;
}
this.jsGraphics.drawPolygon(rotX,rotY);
};
draw2d.Connection=function(){
draw2d.Line.call(this);
this.sourcePort=null;
this.targetPort=null;
this.canDrag=true;
this.sourceDecorator=null;
this.targetDecorator=null;
this.sourceAnchor=new draw2d.ConnectionAnchor();
this.targetAnchor=new draw2d.ConnectionAnchor();
this.router=draw2d.Connection.defaultRouter;
this.lineSegments=new draw2d.ArrayList();
this.children=new draw2d.ArrayList();
this.setColor(new draw2d.Color(0,0,115));
this.setLineWidth(1);
};
draw2d.Connection.prototype=new draw2d.Line();
draw2d.Connection.prototype.type="draw2d.Connection";
draw2d.Connection.defaultRouter=new draw2d.ManhattanConnectionRouter();
draw2d.Connection.setDefaultRouter=function(_53f2){
draw2d.Connection.defaultRouter=_53f2;
};
draw2d.Connection.prototype.disconnect=function(){
if(this.sourcePort!==null){
this.sourcePort.detachMoveListener(this);
this.fireSourcePortRouteEvent();
}
if(this.targetPort!==null){
this.targetPort.detachMoveListener(this);
this.fireTargetPortRouteEvent();
}
};
draw2d.Connection.prototype.reconnect=function(){
if(this.sourcePort!==null){
this.sourcePort.attachMoveListener(this);
this.fireSourcePortRouteEvent();
}
if(this.targetPort!==null){
this.targetPort.attachMoveListener(this);
this.fireTargetPortRouteEvent();
}
};
draw2d.Connection.prototype.isResizeable=function(){
return this.getCanDrag();
};
draw2d.Connection.prototype.setCanDrag=function(flag){
this.canDrag=flag;
};
draw2d.Connection.prototype.getCanDrag=function(){
return this.canDrag;
};
draw2d.Connection.prototype.addFigure=function(_53f4,_53f5){
var entry={};
entry.figure=_53f4;
entry.locator=_53f5;
this.children.add(entry);
if(this.graphics!==null){
this.paint();
}
var oThis=this;
var _53f8=function(){
var _53f9=arguments[0]||window.event;
_53f9.returnValue=false;
oThis.getWorkflow().setCurrentSelection(oThis);
oThis.getWorkflow().showLineResizeHandles(oThis);
};
if(_53f4.getHTMLElement().addEventListener){
_53f4.getHTMLElement().addEventListener("mousedown",_53f8,false);
}else{
if(_53f4.getHTMLElement().attachEvent){
_53f4.getHTMLElement().attachEvent("onmousedown",_53f8);
}
}
};
draw2d.Connection.prototype.setSourceDecorator=function(_53fa){
this.sourceDecorator=_53fa;
if(this.graphics!==null){
this.paint();
}
};
draw2d.Connection.prototype.getSourceDecorator=function(){
return this.sourceDecorator;
};
draw2d.Connection.prototype.setTargetDecorator=function(_53fb){
this.targetDecorator=_53fb;
if(this.graphics!==null){
this.paint();
}
};
draw2d.Connection.prototype.getTargetDecorator=function(){
return this.targetDecorator;
};
draw2d.Connection.prototype.setSourceAnchor=function(_53fc){
this.sourceAnchor=_53fc;
this.sourceAnchor.setOwner(this.sourcePort);
if(this.graphics!==null){
this.paint();
}
};
draw2d.Connection.prototype.setTargetAnchor=function(_53fd){
this.targetAnchor=_53fd;
this.targetAnchor.setOwner(this.targetPort);
if(this.graphics!==null){
this.paint();
}
};
draw2d.Connection.prototype.setRouter=function(_53fe){
if(_53fe!==null){
this.router=_53fe;
}else{
this.router=new draw2d.NullConnectionRouter();
}
if(this.graphics!==null){
this.paint();
}
};
draw2d.Connection.prototype.getRouter=function(){
return this.router;
};
draw2d.Connection.prototype.setWorkflow=function(_53ff){
draw2d.Line.prototype.setWorkflow.call(this,_53ff);
for(var i=0;i<this.children.getSize();i++){
this.children.get(i).isAppended=false;
}
};
draw2d.Connection.prototype.paint=function(){
if(this.html===null){
return;
}
try{
for(var i=0;i<this.children.getSize();i++){
var entry=this.children.get(i);
if(entry.isAppended==true){
this.html.removeChild(entry.figure.getHTMLElement());
}
entry.isAppended=false;
}
if(this.graphics===null){
this.graphics=new jsGraphics(this.html);
}else{
this.graphics.clear();
}
this.graphics.setStroke(this.stroke);
this.graphics.setColor(this.lineColor.getHTMLStyle());
this.startStroke();
this.router.route(this);
if(this.getSource().getParent().isMoving==false&&this.getTarget().getParent().isMoving==false){
if(this.targetDecorator!==null){
this.targetDecorator.paint(new draw2d.Graphics(this.graphics,this.getEndAngle(),this.getEndPoint()));
}
if(this.sourceDecorator!==null){
this.sourceDecorator.paint(new draw2d.Graphics(this.graphics,this.getStartAngle(),this.getStartPoint()));
}
}
this.finishStroke();
for(var i=0;i<this.children.getSize();i++){
var entry=this.children.get(i);
this.html.appendChild(entry.figure.getHTMLElement());
entry.isAppended=true;
entry.locator.relocate(entry.figure);
}
}
catch(e){
pushErrorStack(e,"draw2d.Connection.prototype.paint=function()");
}
};
draw2d.Connection.prototype.getStartPoint=function(){
if(this.isMoving==false){
return this.sourceAnchor.getLocation(this.targetAnchor.getReferencePoint());
}else{
return draw2d.Line.prototype.getStartPoint.call(this);
}
};
draw2d.Connection.prototype.getEndPoint=function(){
if(this.isMoving==false){
return this.targetAnchor.getLocation(this.sourceAnchor.getReferencePoint());
}else{
return draw2d.Line.prototype.getEndPoint.call(this);
}
};
draw2d.Connection.prototype.startStroke=function(){
this.oldPoint=null;
this.lineSegments=new draw2d.ArrayList();
};
draw2d.Connection.prototype.finishStroke=function(){
this.graphics.paint();
this.oldPoint=null;
};
draw2d.Connection.prototype.getPoints=function(){
var _5403=new draw2d.ArrayList();
var line=null;
for(var i=0;i<this.lineSegments.getSize();i++){
line=this.lineSegments.get(i);
_5403.add(line.start);
}
if(line!==null){
_5403.add(line.end);
}
return _5403;
};
draw2d.Connection.prototype.addPoint=function(p){
p=new draw2d.Point(parseInt(p.x),parseInt(p.y));
if(this.oldPoint!==null){
this.graphics.drawLine(this.oldPoint.x,this.oldPoint.y,p.x,p.y);
var line={};
line.start=this.oldPoint;
line.end=p;
this.lineSegments.add(line);
}
this.oldPoint={};
this.oldPoint.x=p.x;
this.oldPoint.y=p.y;
};
draw2d.Connection.prototype.refreshSourcePort=function(){
var model=this.getModel().getSourceModel();
var _5409=this.getModel().getSourcePortName();
var _540a=this.getWorkflow().getDocument().getFigures();
var count=_540a.getSize();
for(var i=0;i<count;i++){
var _540d=_540a.get(i);
if(_540d.getModel()==model){
var port=_540d.getOutputPort(_5409);
this.setSource(port);
}
}
this.setRouter(this.getRouter());
};
draw2d.Connection.prototype.refreshTargetPort=function(){
var model=this.getModel().getTargetModel();
var _5410=this.getModel().getTargetPortName();
var _5411=this.getWorkflow().getDocument().getFigures();
var count=_5411.getSize();
for(var i=0;i<count;i++){
var _5414=_5411.get(i);
if(_5414.getModel()==model){
var port=_5414.getInputPort(_5410);
this.setTarget(port);
}
}
this.setRouter(this.getRouter());
};
draw2d.Connection.prototype.setSource=function(port){
if(this.sourcePort!==null){
this.sourcePort.detachMoveListener(this);
}
this.sourcePort=port;
if(this.sourcePort===null){
return;
}
this.sourceAnchor.setOwner(this.sourcePort);
this.fireSourcePortRouteEvent();
this.sourcePort.attachMoveListener(this);
this.setStartPoint(port.getAbsoluteX(),port.getAbsoluteY());
};
draw2d.Connection.prototype.getSource=function(){
return this.sourcePort;
};
draw2d.Connection.prototype.setTarget=function(port){
if(this.targetPort!==null){
this.targetPort.detachMoveListener(this);
}
this.targetPort=port;
if(this.targetPort===null){
return;
}
this.targetAnchor.setOwner(this.targetPort);
this.fireTargetPortRouteEvent();
this.targetPort.attachMoveListener(this);
this.setEndPoint(port.getAbsoluteX(),port.getAbsoluteY());
};
draw2d.Connection.prototype.getTarget=function(){
return this.targetPort;
};
draw2d.Connection.prototype.onOtherFigureMoved=function(_5418){
if(_5418==this.sourcePort){
this.setStartPoint(this.sourcePort.getAbsoluteX(),this.sourcePort.getAbsoluteY());
}else{
this.setEndPoint(this.targetPort.getAbsoluteX(),this.targetPort.getAbsoluteY());
}
};
draw2d.Connection.prototype.containsPoint=function(px,py){
for(var i=0;i<this.lineSegments.getSize();i++){
var line=this.lineSegments.get(i);
if(draw2d.Line.hit(this.corona,line.start.x,line.start.y,line.end.x,line.end.y,px,py)){
return true;
}
}
return false;
};
draw2d.Connection.prototype.getStartAngle=function(){
var p1=this.lineSegments.get(0).start;
var p2=this.lineSegments.get(0).end;
if(this.router instanceof draw2d.BezierConnectionRouter){
p2=this.lineSegments.get(5).end;
}
var _541f=Math.sqrt((p1.x-p2.x)*(p1.x-p2.x)+(p1.y-p2.y)*(p1.y-p2.y));
var angle=-(180/Math.PI)*Math.asin((p1.y-p2.y)/_541f);
if(angle<0){
if(p2.x<p1.x){
angle=Math.abs(angle)+180;
}else{
angle=360-Math.abs(angle);
}
}else{
if(p2.x<p1.x){
angle=180-angle;
}
}
return angle;
};
draw2d.Connection.prototype.getEndAngle=function(){
if(this.lineSegments.getSize()===0){
return 90;
}
var p1=this.lineSegments.get(this.lineSegments.getSize()-1).end;
var p2=this.lineSegments.get(this.lineSegments.getSize()-1).start;
if(this.router instanceof draw2d.BezierConnectionRouter){
p2=this.lineSegments.get(this.lineSegments.getSize()-5).end;
}
var _5423=Math.sqrt((p1.x-p2.x)*(p1.x-p2.x)+(p1.y-p2.y)*(p1.y-p2.y));
var angle=-(180/Math.PI)*Math.asin((p1.y-p2.y)/_5423);
if(angle<0){
if(p2.x<p1.x){
angle=Math.abs(angle)+180;
}else{
angle=360-Math.abs(angle);
}
}else{
if(p2.x<p1.x){
angle=180-angle;
}
}
return angle;
};
draw2d.Connection.prototype.fireSourcePortRouteEvent=function(){
var _5425=this.sourcePort.getConnections();
for(var i=0;i<_5425.getSize();i++){
_5425.get(i).paint();
}
};
draw2d.Connection.prototype.fireTargetPortRouteEvent=function(){
var _5427=this.targetPort.getConnections();
for(var i=0;i<_5427.getSize();i++){
_5427.get(i).paint();
}
};
draw2d.Connection.prototype.createCommand=function(_5429){
if(_5429.getPolicy()==draw2d.EditPolicy.MOVE){
return new draw2d.CommandReconnect(this);
}
if(_5429.getPolicy()==draw2d.EditPolicy.DELETE){
if(this.isDeleteable()==true){
return new draw2d.CommandDelete(this);
}
return null;
}
return null;
};
draw2d.ConnectionAnchor=function(owner){
this.owner=owner;
};
draw2d.ConnectionAnchor.prototype.type="draw2d.ConnectionAnchor";
draw2d.ConnectionAnchor.prototype.getLocation=function(_5191){
return this.getReferencePoint();
};
draw2d.ConnectionAnchor.prototype.getOwner=function(){
return this.owner;
};
draw2d.ConnectionAnchor.prototype.setOwner=function(owner){
this.owner=owner;
};
draw2d.ConnectionAnchor.prototype.getBox=function(){
return this.getOwner().getAbsoluteBounds();
};
draw2d.ConnectionAnchor.prototype.getReferencePoint=function(){
if(this.getOwner()===null){
return null;
}else{
return this.getOwner().getAbsolutePosition();
}
};
draw2d.ChopboxConnectionAnchor=function(owner){
draw2d.ConnectionAnchor.call(this,owner);
};
draw2d.ChopboxConnectionAnchor.prototype=new draw2d.ConnectionAnchor();
draw2d.ChopboxConnectionAnchor.prototype.type="draw2d.ChopboxConnectionAnchor";
draw2d.ChopboxConnectionAnchor.prototype.getLocation=function(_5437){
var r=new draw2d.Dimension();
r.setBounds(this.getBox());
r.translate(-1,-1);
r.resize(1,1);
var _5439=r.x+r.w/2;
var _543a=r.y+r.h/2;
if(r.isEmpty()||(_5437.x==_5439&&_5437.y==_543a)){
return new Point(_5439,_543a);
}
var dx=_5437.x-_5439;
var dy=_5437.y-_543a;
var scale=0.5/Math.max(Math.abs(dx)/r.w,Math.abs(dy)/r.h);
dx*=scale;
dy*=scale;
_5439+=dx;
_543a+=dy;
return new draw2d.Point(Math.round(_5439),Math.round(_543a));
};
draw2d.ChopboxConnectionAnchor.prototype.getBox=function(){
return this.getOwner().getParent().getBounds();
};
draw2d.ChopboxConnectionAnchor.prototype.getReferencePoint=function(){
return this.getBox().getCenter();
};
draw2d.ConnectionDecorator=function(){
this.color=new draw2d.Color(0,0,0);
this.backgroundColor=new draw2d.Color(250,250,250);
};
draw2d.ConnectionDecorator.prototype.type="draw2d.ConnectionDecorator";
draw2d.ConnectionDecorator.prototype.paint=function(g){
};
draw2d.ConnectionDecorator.prototype.setColor=function(c){
this.color=c;
};
draw2d.ConnectionDecorator.prototype.setBackgroundColor=function(c){
this.backgroundColor=c;
};
draw2d.ArrowConnectionDecorator=function(_59f7,width){
draw2d.ConnectionDecorator.call(this);
if(_59f7===undefined||_59f7<1){
this.lenght=15;
}
if(width===undefined||width<1){
this.width=10;
}
};
draw2d.ArrowConnectionDecorator.prototype=new draw2d.ConnectionDecorator();
draw2d.ArrowConnectionDecorator.prototype.type="draw2d.ArrowConnectionDecorator";
draw2d.ArrowConnectionDecorator.prototype.paint=function(g){
if(this.backgroundColor!==null){
g.setColor(this.backgroundColor);
g.fillPolygon([3,this.lenght,this.lenght,3],[0,(this.width/2),-(this.width/2),0]);
}
g.setColor(this.color);
g.setStroke(1);
g.drawPolygon([3,this.lenght,this.lenght,3],[0,(this.width/2),-(this.width/2),0]);
};
draw2d.ArrowConnectionDecorator.prototype.setDimension=function(l,width){
this.width=w;
this.lenght=l;
};
draw2d.CompartmentFigure=function(){
draw2d.Node.call(this);
this.children=new draw2d.ArrayList();
this.setBorder(new draw2d.LineBorder(1));
this.dropable=new draw2d.DropTarget(this.html);
this.dropable.node=this;
this.dropable.addEventListener("figureenter",function(_5b67){
_5b67.target.node.onFigureEnter(_5b67.relatedTarget.node);
});
this.dropable.addEventListener("figureleave",function(_5b68){
_5b68.target.node.onFigureLeave(_5b68.relatedTarget.node);
});
this.dropable.addEventListener("figuredrop",function(_5b69){
_5b69.target.node.onFigureDrop(_5b69.relatedTarget.node);
});
};
draw2d.CompartmentFigure.prototype=new draw2d.Node();
draw2d.CompartmentFigure.prototype.type="draw2d.CompartmentFigure";
draw2d.CompartmentFigure.prototype.onFigureEnter=function(_5b6a){
};
draw2d.CompartmentFigure.prototype.onFigureLeave=function(_5b6b){
};
draw2d.CompartmentFigure.prototype.onFigureDrop=function(_5b6c){
};
draw2d.CompartmentFigure.prototype.getChildren=function(){
return this.children;
};
draw2d.CompartmentFigure.prototype.addChild=function(_5b6d){
_5b6d.setZOrder(this.getZOrder()+1);
_5b6d.setParent(this);
this.children.add(_5b6d);
};
draw2d.CompartmentFigure.prototype.removeChild=function(_5b6e){
_5b6e.setParent(null);
this.children.remove(_5b6e);
};
draw2d.CompartmentFigure.prototype.setZOrder=function(index){
draw2d.Node.prototype.setZOrder.call(this,index);
for(var i=0;i<this.children.getSize();i++){
this.children.get(i).setZOrder(index+1);
}
};
draw2d.CompartmentFigure.prototype.setPosition=function(xPos,yPos){
var oldX=this.getX();
var oldY=this.getY();
draw2d.Node.prototype.setPosition.call(this,xPos,yPos);
for(var i=0;i<this.children.getSize();i++){
var child=this.children.get(i);
child.setPosition(child.getX()+this.getX()-oldX,child.getY()+this.getY()-oldY);
}
};
draw2d.CompartmentFigure.prototype.onDrag=function(){
var oldX=this.getX();
var oldY=this.getY();
draw2d.Node.prototype.onDrag.call(this);
for(var i=0;i<this.children.getSize();i++){
var child=this.children.get(i);
child.setPosition(child.getX()+this.getX()-oldX,child.getY()+this.getY()-oldY);
}
};
draw2d.CanvasDocument=function(_51b0){
this.canvas=_51b0;
};
draw2d.CanvasDocument.prototype.type="draw2d.CanvasDocument";
draw2d.CanvasDocument.prototype.getFigures=function(){
var _51b1=new draw2d.ArrayList();
var _51b2=this.canvas.figures;
var _51b3=this.canvas.dialogs;
for(var i=0;i<_51b2.getSize();i++){
var _51b5=_51b2.get(i);
if(_51b3.indexOf(_51b5)==-1&&_51b5.getParent()===null&&!(_51b5 instanceof draw2d.WindowFigure)){
_51b1.add(_51b5);
}
}
return _51b1;
};
draw2d.CanvasDocument.prototype.getFigure=function(id){
return this.canvas.getFigure(id);
};
draw2d.CanvasDocument.prototype.getLines=function(){
return this.canvas.getLines();
};
draw2d.CanvasDocument.prototype.getLine=function(id){
return this.canvas.getLine(id);
};
draw2d.Annotation=function(msg){
this.msg=msg;
this.alpha=1;
this.color=new draw2d.Color(0,0,0);
this.bgColor=new draw2d.Color(241,241,121);
this.fontSize=10;
this.textNode=null;
draw2d.Figure.call(this);
};
draw2d.Annotation.prototype=new draw2d.Figure();
draw2d.Annotation.prototype.type="draw2d.Annotation";
draw2d.Annotation.prototype.createHTMLElement=function(){
var item=draw2d.Figure.prototype.createHTMLElement.call(this);
item.style.color=this.color.getHTMLStyle();
item.style.backgroundColor=this.bgColor.getHTMLStyle();
item.style.fontSize=this.fontSize+"pt";
item.style.width="auto";
item.style.height="auto";
item.style.margin="0px";
item.style.padding="0px";
item.onselectstart=function(){
return false;
};
item.unselectable="on";
item.style.cursor="default";
this.textNode=document.createTextNode(this.msg);
item.appendChild(this.textNode);
this.disableTextSelection(item);
return item;
};
draw2d.Annotation.prototype.onDoubleClick=function(){
var _59de=new draw2d.AnnotationDialog(this);
this.workflow.showDialog(_59de);
};
draw2d.Annotation.prototype.setBackgroundColor=function(color){
this.bgColor=color;
if(this.bgColor!==null){
this.html.style.backgroundColor=this.bgColor.getHTMLStyle();
}else{
this.html.style.backgroundColor="transparent";
}
};
draw2d.Annotation.prototype.getBackgroundColor=function(){
return this.bgColor;
};
draw2d.Annotation.prototype.setFontSize=function(size){
this.fontSize=size;
this.html.style.fontSize=this.fontSize+"pt";
};
draw2d.Annotation.prototype.getText=function(){
return this.msg;
};
draw2d.Annotation.prototype.setText=function(text){
this.msg=text;
this.html.removeChild(this.textNode);
this.textNode=document.createTextNode(this.msg);
this.html.appendChild(this.textNode);
};
draw2d.Annotation.prototype.setStyledText=function(text){
this.msg=text;
this.html.removeChild(this.textNode);
this.textNode=document.createElement("div");
this.textNode.innerHTML=text;
this.html.appendChild(this.textNode);
};
draw2d.ResizeHandle=function(_5b46,type){
draw2d.Rectangle.call(this,5,5);
this.type=type;
var _5b48=this.getWidth();
var _5b49=_5b48/2;
switch(this.type){
case 1:
this.setSnapToGridAnchor(new draw2d.Point(_5b48,_5b48));
break;
case 2:
this.setSnapToGridAnchor(new draw2d.Point(_5b49,_5b48));
break;
case 3:
this.setSnapToGridAnchor(new draw2d.Point(0,_5b48));
break;
case 4:
this.setSnapToGridAnchor(new draw2d.Point(0,_5b49));
break;
case 5:
this.setSnapToGridAnchor(new draw2d.Point(0,0));
break;
case 6:
this.setSnapToGridAnchor(new draw2d.Point(_5b49,0));
break;
case 7:
this.setSnapToGridAnchor(new draw2d.Point(_5b48,0));
break;
case 8:
this.setSnapToGridAnchor(new draw2d.Point(_5b48,_5b49));
case 9:
this.setSnapToGridAnchor(new draw2d.Point(_5b49,_5b49));
break;
}
this.setBackgroundColor(new draw2d.Color(0,255,0));
this.setWorkflow(_5b46);
this.setZOrder(10000);
};
draw2d.ResizeHandle.prototype=new draw2d.Rectangle();
draw2d.ResizeHandle.prototype.type="draw2d.ResizeHandle";
draw2d.ResizeHandle.prototype.getSnapToDirection=function(){
switch(this.type){
case 1:
return draw2d.SnapToHelper.NORTH_WEST;
case 2:
return draw2d.SnapToHelper.NORTH;
case 3:
return draw2d.SnapToHelper.NORTH_EAST;
case 4:
return draw2d.SnapToHelper.EAST;
case 5:
return draw2d.SnapToHelper.SOUTH_EAST;
case 6:
return draw2d.SnapToHelper.SOUTH;
case 7:
return draw2d.SnapToHelper.SOUTH_WEST;
case 8:
return draw2d.SnapToHelper.WEST;
case 9:
return draw2d.SnapToHelper.CENTER;
}
};
draw2d.ResizeHandle.prototype.onDragend=function(){
var _5b4a=this.workflow.currentSelection;
if(this.commandMove!==null){
this.commandMove.setPosition(_5b4a.getX(),_5b4a.getY());
this.workflow.getCommandStack().execute(this.commandMove);
this.commandMove=null;
}
if(this.commandResize!==null){
this.commandResize.setDimension(_5b4a.getWidth(),_5b4a.getHeight());
this.workflow.getCommandStack().execute(this.commandResize);
this.commandResize=null;
}
this.workflow.hideSnapToHelperLines();
};
draw2d.ResizeHandle.prototype.setPosition=function(xPos,yPos){
this.x=xPos;
this.y=yPos;
if(this.html===null){
return;
}
this.html.style.left=this.x+"px";
this.html.style.top=this.y+"px";
};
draw2d.ResizeHandle.prototype.onDragstart=function(x,y){
if(!this.canDrag){
return false;
}
var _5b4f=this.workflow.currentSelection;
this.commandMove=_5b4f.createCommand(new draw2d.EditPolicy(draw2d.EditPolicy.MOVE));
this.commandResize=_5b4f.createCommand(new draw2d.EditPolicy(draw2d.EditPolicy.RESIZE));
return true;
};
draw2d.ResizeHandle.prototype.onDrag=function(){
var oldX=this.getX();
var oldY=this.getY();
draw2d.Rectangle.prototype.onDrag.call(this);
var diffX=oldX-this.getX();
var diffY=oldY-this.getY();
var _5b54=this.workflow.currentSelection.getX();
var _5b55=this.workflow.currentSelection.getY();
var _5b56=this.workflow.currentSelection.getWidth();
var _5b57=this.workflow.currentSelection.getHeight();
switch(this.type){
case 1:
this.workflow.currentSelection.setPosition(_5b54-diffX,_5b55-diffY);
this.workflow.currentSelection.setDimension(_5b56+diffX,_5b57+diffY);
break;
case 2:
this.workflow.currentSelection.setPosition(_5b54,_5b55-diffY);
this.workflow.currentSelection.setDimension(_5b56,_5b57+diffY);
break;
case 3:
this.workflow.currentSelection.setPosition(_5b54,_5b55-diffY);
this.workflow.currentSelection.setDimension(_5b56-diffX,_5b57+diffY);
break;
case 4:
this.workflow.currentSelection.setPosition(_5b54,_5b55);
this.workflow.currentSelection.setDimension(_5b56-diffX,_5b57);
break;
case 5:
this.workflow.currentSelection.setPosition(_5b54,_5b55);
this.workflow.currentSelection.setDimension(_5b56-diffX,_5b57-diffY);
break;
case 6:
this.workflow.currentSelection.setPosition(_5b54,_5b55);
this.workflow.currentSelection.setDimension(_5b56,_5b57-diffY);
break;
case 7:
this.workflow.currentSelection.setPosition(_5b54-diffX,_5b55);
this.workflow.currentSelection.setDimension(_5b56+diffX,_5b57-diffY);
break;
case 8:
this.workflow.currentSelection.setPosition(_5b54-diffX,_5b55);
this.workflow.currentSelection.setDimension(_5b56+diffX,_5b57);
break;
}
this.workflow.moveResizeHandles(this.workflow.getCurrentSelection());
};
draw2d.ResizeHandle.prototype.setCanDrag=function(flag){
draw2d.Rectangle.prototype.setCanDrag.call(this,flag);
if(this.html===null){
return;
}
if(!flag){
this.html.style.cursor="";
return;
}
switch(this.type){
case 1:
this.html.style.cursor="nw-resize";
break;
case 2:
this.html.style.cursor="s-resize";
break;
case 3:
this.html.style.cursor="ne-resize";
break;
case 4:
this.html.style.cursor="w-resize";
break;
case 5:
this.html.style.cursor="se-resize";
break;
case 6:
this.html.style.cursor="n-resize";
break;
case 7:
this.html.style.cursor="sw-resize";
break;
case 8:
this.html.style.cursor="e-resize";
break;
case 9:
this.html.style.cursor="resize";
break;
}
};
draw2d.ResizeHandle.prototype.onKeyDown=function(_5b59,ctrl){
this.workflow.onKeyDown(_5b59,ctrl);
};
draw2d.ResizeHandle.prototype.fireMoveEvent=function(){
};
draw2d.LineStartResizeHandle=function(_4a79){
draw2d.ResizeHandle.call(this,_4a79,9);
this.setDimension(10,10);
this.setBackgroundColor(new draw2d.Color(100,255,0));
this.setZOrder(10000);
};
draw2d.LineStartResizeHandle.prototype=new draw2d.ResizeHandle();
draw2d.LineStartResizeHandle.prototype.type="draw2d.LineStartResizeHandle";
draw2d.LineStartResizeHandle.prototype.onDragend=function(){
if(this.workflow.currentSelection instanceof draw2d.Connection){
if(this.command!==null){
this.command.cancel();
}
}else{
if(this.command!==null){
this.getWorkflow().getCommandStack().execute(this.command);
}
}
this.command=null;
};
draw2d.LineStartResizeHandle.prototype.onDragstart=function(x,y){
if(!this.canDrag){
return false;
}
this.command=this.workflow.currentSelection.createCommand(new draw2d.EditPolicy(draw2d.EditPolicy.MOVE));
return this.command!==null;
};
draw2d.LineStartResizeHandle.prototype.onDrag=function(){
var oldX=this.getX();
var oldY=this.getY();
draw2d.Rectangle.prototype.onDrag.call(this);
var diffX=oldX-this.getX();
var diffY=oldY-this.getY();
var _4a80=this.workflow.currentSelection.getStartPoint();
var line=this.workflow.currentSelection;
line.setStartPoint(_4a80.x-diffX,_4a80.y-diffY);
line.isMoving=true;
};
draw2d.LineStartResizeHandle.prototype.onDrop=function(_4a82){
var line=this.workflow.currentSelection;
line.isMoving=false;
if(line instanceof draw2d.Connection){
this.command.setNewPorts(_4a82,line.getTarget());
this.getWorkflow().getCommandStack().execute(this.command);
}
this.command=null;
};
draw2d.LineEndResizeHandle=function(_50b7){
draw2d.ResizeHandle.call(this,_50b7,9);
this.setDimension(10,10);
this.setBackgroundColor(new draw2d.Color(0,255,0));
this.setZOrder(10000);
};
draw2d.LineEndResizeHandle.prototype=new draw2d.ResizeHandle();
draw2d.LineEndResizeHandle.prototype.type="draw2d.LineEndResizeHandle";
draw2d.LineEndResizeHandle.prototype.onDragend=function(){
if(this.workflow.currentSelection instanceof draw2d.Connection){
if(this.command!==null){
this.command.cancel();
}
}else{
if(this.command!==null){
this.workflow.getCommandStack().execute(this.command);
}
}
this.command=null;
};
draw2d.LineEndResizeHandle.prototype.onDragstart=function(x,y){
if(!this.canDrag){
return false;
}
this.command=this.workflow.currentSelection.createCommand(new draw2d.EditPolicy(draw2d.EditPolicy.MOVE));
return this.command!==null;
};
draw2d.LineEndResizeHandle.prototype.onDrag=function(){
var oldX=this.getX();
var oldY=this.getY();
draw2d.Rectangle.prototype.onDrag.call(this);
var diffX=oldX-this.getX();
var diffY=oldY-this.getY();
var _50be=this.workflow.currentSelection.getEndPoint();
var line=this.workflow.currentSelection;
line.setEndPoint(_50be.x-diffX,_50be.y-diffY);
line.isMoving=true;
};
draw2d.LineEndResizeHandle.prototype.onDrop=function(_50c0){
var line=this.workflow.currentSelection;
line.isMoving=false;
if(line instanceof draw2d.Connection){
this.command.setNewPorts(line.getSource(),_50c0);
this.getWorkflow().getCommandStack().execute(this.command);
}
this.command=null;
};
draw2d.Canvas=function(_4e4a){
try{
if(_4e4a){
this.construct(_4e4a);
}
this.enableSmoothFigureHandling=false;
this.canvasLines=new draw2d.ArrayList();
}
catch(e){
pushErrorStack(e,"draw2d.Canvas=function(/*:String*/id)");
}
};
draw2d.Canvas.IMAGE_BASE_URL="";
draw2d.Canvas.prototype.type="draw2d.Canvas";
draw2d.Canvas.prototype.construct=function(_4e4b){
this.canvasId=_4e4b;
this.html=document.getElementById(this.canvasId);
this.scrollArea=document.body.parentNode;
};
draw2d.Canvas.prototype.setViewPort=function(divId){
this.scrollArea=document.getElementById(divId);
};
draw2d.Canvas.prototype.addFigure=function(_4e4d,xPos,yPos,_4e50){
try{
if(this.enableSmoothFigureHandling===true){
if(_4e4d.timer<=0){
_4e4d.setAlpha(0.001);
}
var _4e51=_4e4d;
var _4e52=function(){
if(_4e51.alpha<1){
_4e51.setAlpha(Math.min(1,_4e51.alpha+0.05));
}else{
window.clearInterval(_4e51.timer);
_4e51.timer=-1;
}
};
if(_4e51.timer>0){
window.clearInterval(_4e51.timer);
}
_4e51.timer=window.setInterval(_4e52,30);
}
_4e4d.setCanvas(this);
if(xPos&&yPos){
_4e4d.setPosition(xPos,yPos);
}
if(_4e4d instanceof draw2d.Line){
this.canvasLines.add(_4e4d);
this.html.appendChild(_4e4d.getHTMLElement());
}else{
var obj=this.canvasLines.getFirstElement();
if(obj===null){
this.html.appendChild(_4e4d.getHTMLElement());
}else{
this.html.insertBefore(_4e4d.getHTMLElement(),obj.getHTMLElement());
}
}
if(!_4e50){
_4e4d.paint();
}
}
catch(e){
pushErrorStack(e,"draw2d.Canvas.prototype.addFigure= function( /*:draw2d.Figure*/figure,/*:int*/ xPos,/*:int*/ yPos, /*:boolean*/ avoidPaint)");
}
};
draw2d.Canvas.prototype.removeFigure=function(_4e54){
if(this.enableSmoothFigureHandling===true){
var oThis=this;
var _4e56=_4e54;
var _4e57=function(){
if(_4e56.alpha>0){
_4e56.setAlpha(Math.max(0,_4e56.alpha-0.05));
}else{
window.clearInterval(_4e56.timer);
_4e56.timer=-1;
oThis.html.removeChild(_4e56.html);
_4e56.setCanvas(null);
}
};
if(_4e56.timer>0){
window.clearInterval(_4e56.timer);
}
_4e56.timer=window.setInterval(_4e57,20);
}else{
this.html.removeChild(_4e54.html);
_4e54.setCanvas(null);
}
if(_4e54 instanceof draw2d.Line){
this.canvasLines.remove(_4e54);
}
};
draw2d.Canvas.prototype.getEnableSmoothFigureHandling=function(){
return this.enableSmoothFigureHandling;
};
draw2d.Canvas.prototype.setEnableSmoothFigureHandling=function(flag){
this.enableSmoothFigureHandling=flag;
};
draw2d.Canvas.prototype.getWidth=function(){
return parseInt(this.html.style.width);
};
draw2d.Canvas.prototype.setWidth=function(width){
if(this.scrollArea!==null){
this.scrollArea.style.width=width+"px";
}else{
this.html.style.width=width+"px";
}
};
draw2d.Canvas.prototype.getHeight=function(){
return parseInt(this.html.style.height);
};
draw2d.Canvas.prototype.setHeight=function(_4e5a){
if(this.scrollArea!==null){
this.scrollArea.style.height=_4e5a+"px";
}else{
this.html.style.height=_4e5a+"px";
}
};
draw2d.Canvas.prototype.setBackgroundImage=function(_4e5b,_4e5c){
if(_4e5b!==null){
if(_4e5c){
this.html.style.background="transparent url("+_4e5b+") ";
}else{
this.html.style.background="transparent url("+_4e5b+") no-repeat";
}
}else{
this.html.style.background="transparent";
}
};
draw2d.Canvas.prototype.getY=function(){
return this.y;
};
draw2d.Canvas.prototype.getX=function(){
return this.x;
};
draw2d.Canvas.prototype.getAbsoluteY=function(){
var el=this.html;
var ot=el.offsetTop;
while((el=el.offsetParent)!==null){
ot+=el.offsetTop;
}
return ot;
};
draw2d.Canvas.prototype.getAbsoluteX=function(){
var el=this.html;
var ol=el.offsetLeft;
while((el=el.offsetParent)!==null){
ol+=el.offsetLeft;
}
return ol;
};
draw2d.Canvas.prototype.getScrollLeft=function(){
return this.scrollArea.scrollLeft;
};
draw2d.Canvas.prototype.getScrollTop=function(){
return this.scrollArea.scrollTop;
};
draw2d.Workflow=function(id){
try{
if(!id){
return;
}
this.isIPad=navigator.userAgent.match(/iPad/i)!=null;
this.menu=null;
this.gridWidthX=10;
this.gridWidthY=10;
this.snapToGridHelper=null;
this.verticalSnapToHelperLine=null;
this.horizontalSnapToHelperLine=null;
this.snapToGeometryHelper=null;
this.figures=new draw2d.ArrayList();
this.lines=new draw2d.ArrayList();
this.commonPorts=new draw2d.ArrayList();
this.dropTargets=new draw2d.ArrayList();
this.compartments=new draw2d.ArrayList();
this.selectionListeners=new draw2d.ArrayList();
this.dialogs=new draw2d.ArrayList();
this.toolPalette=null;
this.dragging=false;
this.tooltip=null;
this.draggingLine=null;
this.draggingLineCommand=null;
this.commandStack=new draw2d.CommandStack();
this.oldScrollPosLeft=0;
this.oldScrollPosTop=0;
this.currentSelection=null;
this.currentMenu=null;
this.connectionLine=new draw2d.Line();
this.resizeHandleStart=new draw2d.LineStartResizeHandle(this);
this.resizeHandleEnd=new draw2d.LineEndResizeHandle(this);
this.resizeHandle1=new draw2d.ResizeHandle(this,1);
this.resizeHandle2=new draw2d.ResizeHandle(this,2);
this.resizeHandle3=new draw2d.ResizeHandle(this,3);
this.resizeHandle4=new draw2d.ResizeHandle(this,4);
this.resizeHandle5=new draw2d.ResizeHandle(this,5);
this.resizeHandle6=new draw2d.ResizeHandle(this,6);
this.resizeHandle7=new draw2d.ResizeHandle(this,7);
this.resizeHandle8=new draw2d.ResizeHandle(this,8);
this.resizeHandleHalfWidth=parseInt(this.resizeHandle2.getWidth()/2);
draw2d.Canvas.call(this,id);
this.setPanning(this.isIPad);
if(this.html!==null){
this.html.style.backgroundImage="url(grid_10.png)";
this.html.className="Workflow";
oThis=this;
this.html.tabIndex="0";
var _5e1a=function(){
var _5e1b=arguments[0]||window.event;
_5e1b.cancelBubble=true;
_5e1b.returnValue=false;
_5e1b.stopped=true;
var diffX=_5e1b.clientX;
var diffY=_5e1b.clientY;
var _5e1e=oThis.getScrollLeft();
var _5e1f=oThis.getScrollTop();
var _5e20=oThis.getAbsoluteX();
var _5e21=oThis.getAbsoluteY();
var line=oThis.getBestLine(diffX+_5e1e-_5e20,diffY+_5e1f-_5e21,null);
if(line!==null){
line.onContextMenu(diffX+_5e1e-_5e20,diffY+_5e1f-_5e21);
}else{
oThis.onContextMenu(diffX+_5e1e-_5e20,diffY+_5e1f-_5e21);
}
};
this.html.oncontextmenu=function(){
return false;
};
var oThis=this;
var _5e24=function(event){
var ctrl=event.ctrlKey;
oThis.onKeyDown(event.keyCode,ctrl);
};
var _5e27=function(){
var _5e28=arguments[0]||window.event;
if(_5e28.returnValue==false){
return;
}
var diffX=_5e28.clientX;
var diffY=_5e28.clientY;
var _5e2b=oThis.getScrollLeft();
var _5e2c=oThis.getScrollTop();
var _5e2d=oThis.getAbsoluteX();
var _5e2e=oThis.getAbsoluteY();
oThis.onMouseDown(diffX+_5e2b-_5e2d,diffY+_5e2c-_5e2e);
};
var _5e2f=function(e){
var _5e31=e.touches.item(0);
var diffX=_5e31.clientX;
var diffY=_5e31.clientY;
var _5e34=oThis.getScrollLeft();
var _5e35=oThis.getScrollTop();
var _5e36=oThis.getAbsoluteX();
var _5e37=oThis.getAbsoluteY();
oThis.onMouseDown(diffX+_5e34-_5e36,diffY+_5e35-_5e37);
e.preventDefault();
};
var _5e38=function(){
var _5e39=arguments[0]||window.event;
if(oThis.currentMenu!==null){
oThis.removeFigure(oThis.currentMenu);
oThis.currentMenu=null;
}
if(_5e39.button==2){
return;
}
var diffX=_5e39.clientX;
var diffY=_5e39.clientY;
var _5e3c=oThis.getScrollLeft();
var _5e3d=oThis.getScrollTop();
var _5e3e=oThis.getAbsoluteX();
var _5e3f=oThis.getAbsoluteY();
oThis.onMouseUp(diffX+_5e3c-_5e3e,diffY+_5e3d-_5e3f);
};
var _5e40=function(e){
var _5e42=e.touches.item(0);
var diffX=_5e42.clientX;
var diffY=_5e42.clientY;
var _5e45=oThis.getScrollLeft();
var _5e46=oThis.getScrollTop();
var _5e47=oThis.getAbsoluteX();
var _5e48=oThis.getAbsoluteY();
oThis.currentMouseX=diffX+_5e45-_5e47;
oThis.currentMouseY=diffY+_5e46-_5e48;
var obj=oThis.getBestFigure(oThis.currentMouseX,oThis.currentMouseY);
if(draw2d.Drag.currentHover!==null&&obj===null){
var _5e4a=new draw2d.DragDropEvent();
_5e4a.initDragDropEvent("mouseleave",false,oThis);
draw2d.Drag.currentHover.dispatchEvent(_5e4a);
}else{
var diffX=_5e42.clientX;
var diffY=_5e42.clientY;
var _5e45=oThis.getScrollLeft();
var _5e46=oThis.getScrollTop();
var _5e47=oThis.getAbsoluteX();
var _5e48=oThis.getAbsoluteY();
oThis.onMouseMove(diffX+_5e45-_5e47,diffY+_5e46-_5e48);
}
if(obj===null){
draw2d.Drag.currentHover=null;
}
e.preventDefault();
};
var _5e4b=function(){
var _5e4c=arguments[0]||window.event;
var diffX=_5e4c.clientX;
var diffY=_5e4c.clientY;
var _5e4f=oThis.getScrollLeft();
var _5e50=oThis.getScrollTop();
var _5e51=oThis.getAbsoluteX();
var _5e52=oThis.getAbsoluteY();
oThis.currentMouseX=diffX+_5e4f-_5e51;
oThis.currentMouseY=diffY+_5e50-_5e52;
var obj=oThis.getBestFigure(oThis.currentMouseX,oThis.currentMouseY);
if(draw2d.Drag.currentHover!==null&&obj===null){
var _5e54=new draw2d.DragDropEvent();
_5e54.initDragDropEvent("mouseleave",false,oThis);
draw2d.Drag.currentHover.dispatchEvent(_5e54);
}else{
var diffX=_5e4c.clientX;
var diffY=_5e4c.clientY;
var _5e4f=oThis.getScrollLeft();
var _5e50=oThis.getScrollTop();
var _5e51=oThis.getAbsoluteX();
var _5e52=oThis.getAbsoluteY();
oThis.onMouseMove(diffX+_5e4f-_5e51,diffY+_5e50-_5e52);
}
if(obj===null){
draw2d.Drag.currentHover=null;
}
if(oThis.tooltip!==null){
if(Math.abs(oThis.currentTooltipX-oThis.currentMouseX)>10||Math.abs(oThis.currentTooltipY-oThis.currentMouseY)>10){
oThis.showTooltip(null);
}
}
};
var _5e55=function(_5e56){
var _5e56=arguments[0]||window.event;
var diffX=_5e56.clientX;
var diffY=_5e56.clientY;
var _5e59=oThis.getScrollLeft();
var _5e5a=oThis.getScrollTop();
var _5e5b=oThis.getAbsoluteX();
var _5e5c=oThis.getAbsoluteY();
var line=oThis.getBestLine(diffX+_5e59-_5e5b,diffY+_5e5a-_5e5c,null);
if(line!==null){
line.onDoubleClick();
}
};
if(this.html.addEventListener){
this.html.addEventListener("contextmenu",_5e1a,false);
this.html.addEventListener("touchstart",_5e2f,false);
this.html.addEventListener("touchmove",_5e40,false);
this.html.addEventListener("mousemove",_5e4b,false);
this.html.addEventListener("mouseup",_5e38,false);
this.html.addEventListener("mousedown",_5e27,false);
this.html.addEventListener("keydown",_5e24,false);
this.html.addEventListener("dblclick",_5e55,false);
}else{
if(this.html.attachEvent){
this.html.attachEvent("oncontextmenu",_5e1a);
this.html.attachEvent("onmousemove",_5e4b);
this.html.attachEvent("onmousedown",_5e27);
this.html.attachEvent("onmouseup",_5e38);
this.html.attachEvent("onkeydown",_5e24);
this.html.attachEvent("ondblclick",_5e55);
}else{
throw "Open-jACOB Draw2D not supported in this browser.";
}
}
}
}
catch(e){
pushErrorStack(e,"draw2d.Workflow=function(/*:String*/id)");
}
};
draw2d.Workflow.prototype=new draw2d.Canvas();
draw2d.Workflow.prototype.type="draw2d.Workflow";
draw2d.Workflow.COLOR_GREEN=new draw2d.Color(0,255,0);
draw2d.Workflow.prototype.clear=function(){
this.scrollTo(0,0,true);
this.gridWidthX=10;
this.gridWidthY=10;
this.snapToGridHelper=null;
this.verticalSnapToHelperLine=null;
this.horizontalSnapToHelperLine=null;
var _5e5e=this.getDocument();
var _5e5f=_5e5e.getLines().clone();
for(var i=0;i<_5e5f.getSize();i++){
(new draw2d.CommandDelete(_5e5f.get(i))).execute();
}
var _5e61=_5e5e.getFigures().clone();
for(var i=0;i<_5e61.getSize();i++){
(new draw2d.CommandDelete(_5e61.get(i))).execute();
}
this.commonPorts.removeAllElements();
this.dropTargets.removeAllElements();
this.compartments.removeAllElements();
this.selectionListeners.removeAllElements();
this.dialogs.removeAllElements();
this.commandStack=new draw2d.CommandStack();
this.currentSelection=null;
this.currentMenu=null;
draw2d.Drag.clearCurrent();
};
draw2d.Workflow.prototype.onScroll=function(){
var _5e62=this.getScrollLeft();
var _5e63=this.getScrollTop();
var _5e64=_5e62-this.oldScrollPosLeft;
var _5e65=_5e63-this.oldScrollPosTop;
for(var i=0;i<this.figures.getSize();i++){
var _5e67=this.figures.get(i);
if(_5e67.hasFixedPosition&&_5e67.hasFixedPosition()==true){
_5e67.setPosition(_5e67.getX()+_5e64,_5e67.getY()+_5e65);
}
}
this.oldScrollPosLeft=_5e62;
this.oldScrollPosTop=_5e63;
};
draw2d.Workflow.prototype.setPanning=function(flag){
this.panning=flag;
if(flag){
this.html.style.cursor="move";
}else{
this.html.style.cursor="default";
}
};
draw2d.Workflow.prototype.scrollTo=function(x,y,fast){
if(fast){
this.scrollArea.scrollLeft=x;
this.scrollArea.scrollTop=y;
}else{
var steps=40;
var xStep=(x-this.getScrollLeft())/steps;
var yStep=(y-this.getScrollTop())/steps;
var oldX=this.getScrollLeft();
var oldY=this.getScrollTop();
for(var i=0;i<steps;i++){
this.scrollArea.scrollLeft=oldX+(xStep*i);
this.scrollArea.scrollTop=oldY+(yStep*i);
}
}
};
draw2d.Workflow.prototype.showTooltip=function(_5e72,_5e73){
if(this.tooltip!==null){
this.removeFigure(this.tooltip);
this.tooltip=null;
if(this.tooltipTimer>=0){
window.clearTimeout(this.tooltipTimer);
this.tooltipTimer=-1;
}
}
this.tooltip=_5e72;
if(this.tooltip!==null){
this.currentTooltipX=this.currentMouseX;
this.currentTooltipY=this.currentMouseY;
this.addFigure(this.tooltip,this.currentTooltipX+10,this.currentTooltipY+10);
var oThis=this;
var _5e75=function(){
oThis.tooltipTimer=-1;
oThis.showTooltip(null);
};
if(_5e73==true){
this.tooltipTimer=window.setTimeout(_5e75,5000);
}
}
};
draw2d.Workflow.prototype.showDialog=function(_5e76,xPos,yPos){
if(xPos){
this.addFigure(_5e76,xPos,yPos);
}else{
this.addFigure(_5e76,200,100);
}
this.dialogs.add(_5e76);
};
draw2d.Workflow.prototype.showMenu=function(menu,xPos,yPos){
if(this.menu!==null){
this.html.removeChild(this.menu.getHTMLElement());
this.menu.setWorkflow();
}
this.menu=menu;
if(this.menu!==null){
this.menu.setWorkflow(this);
this.menu.setPosition(xPos,yPos);
this.html.appendChild(this.menu.getHTMLElement());
this.menu.paint();
}
};
draw2d.Workflow.prototype.onContextMenu=function(x,y){
var menu=this.getContextMenu();
if(menu!==null){
this.showMenu(menu,x,y);
}
};
draw2d.Workflow.prototype.getContextMenu=function(){
return null;
};
draw2d.Workflow.prototype.setToolWindow=function(_5e7f,x,y){
this.toolPalette=_5e7f;
if(y){
this.addFigure(_5e7f,x,y);
}else{
this.addFigure(_5e7f,20,20);
}
this.dialogs.add(_5e7f);
};
draw2d.Workflow.prototype.setSnapToGrid=function(flag){
if(flag){
this.snapToGridHelper=new draw2d.SnapToGrid(this);
}else{
this.snapToGridHelper=null;
}
};
draw2d.Workflow.prototype.setSnapToGeometry=function(flag){
if(flag){
this.snapToGeometryHelper=new draw2d.SnapToGeometry(this);
}else{
this.snapToGeometryHelper=null;
}
};
draw2d.Workflow.prototype.setGridWidth=function(dx,dy){
this.gridWidthX=dx;
this.gridWidthY=dy;
};
draw2d.Workflow.prototype.addFigure=function(_5e86,xPos,yPos){
try{
draw2d.Canvas.prototype.addFigure.call(this,_5e86,xPos,yPos,true);
_5e86.setWorkflow(this);
var _5e89=this;
if(_5e86 instanceof draw2d.CompartmentFigure){
this.compartments.add(_5e86);
}
if(_5e86 instanceof draw2d.Line){
this.lines.add(_5e86);
}else{
this.figures.add(_5e86);
_5e86.draggable.addEventListener("drag",function(_5e8a){
var _5e8b=_5e89.getFigure(_5e8a.target.element.id);
if(_5e8b===null){
return;
}
if(_5e8b.isSelectable()==false){
return;
}
_5e89.moveResizeHandles(_5e8b);
});
}
_5e86.paint();
this.setDocumentDirty();
}
catch(e){
pushErrorStack(e,"draw2d.Workflow.prototype.addFigure=function(/*:draw2d.Figure*/ figure ,/*:int*/ xPos, /*:int*/ yPos)");
}
};
draw2d.Workflow.prototype.removeFigure=function(_5e8c){
draw2d.Canvas.prototype.removeFigure.call(this,_5e8c);
this.figures.remove(_5e8c);
this.lines.remove(_5e8c);
this.dialogs.remove(_5e8c);
_5e8c.setWorkflow(null);
if(_5e8c instanceof draw2d.CompartmentFigure){
this.compartments.remove(_5e8c);
}
if(_5e8c instanceof draw2d.Connection){
_5e8c.disconnect();
}
if(this.currentSelection==_5e8c){
this.setCurrentSelection(null);
}
this.setDocumentDirty();
_5e8c.onRemove(this);
};
draw2d.Workflow.prototype.moveFront=function(_5e8d){
this.html.removeChild(_5e8d.getHTMLElement());
this.html.appendChild(_5e8d.getHTMLElement());
};
draw2d.Workflow.prototype.moveBack=function(_5e8e){
this.html.removeChild(_5e8e.getHTMLElement());
this.html.insertBefore(_5e8e.getHTMLElement(),this.html.firstChild);
};
draw2d.Workflow.prototype.getBestCompartmentFigure=function(x,y,_5e91){
var _5e92=null;
for(var i=0;i<this.figures.getSize();i++){
var _5e94=this.figures.get(i);
if((_5e94 instanceof draw2d.CompartmentFigure)&&_5e94.isOver(x,y)==true&&_5e94!=_5e91){
if(_5e92===null){
_5e92=_5e94;
}else{
if(_5e92.getZOrder()<_5e94.getZOrder()){
_5e92=_5e94;
}
}
}
}
return _5e92;
};
draw2d.Workflow.prototype.getBestFigure=function(x,y,_5e97){
var _5e98=null;
for(var i=0;i<this.figures.getSize();i++){
var _5e9a=this.figures.get(i);
if(_5e9a.isOver(x,y)==true&&_5e9a!=_5e97){
if(_5e98===null){
_5e98=_5e9a;
}else{
if(_5e98.getZOrder()<_5e9a.getZOrder()){
_5e98=_5e9a;
}
}
}
}
return _5e98;
};
draw2d.Workflow.prototype.getBestLine=function(x,y,_5e9d){
var _5e9e=null;
var count=this.lines.getSize();
for(var i=0;i<count;i++){
var line=this.lines.get(i);
if(line.containsPoint(x,y)==true&&line!=_5e9d){
if(_5e9e===null){
_5e9e=line;
}else{
if(_5e9e.getZOrder()<line.getZOrder()){
_5e9e=line;
}
}
}
}
return _5e9e;
};
draw2d.Workflow.prototype.getFigure=function(id){
for(var i=0;i<this.figures.getSize();i++){
var _5ea4=this.figures.get(i);
if(_5ea4.id==id){
return _5ea4;
}
}
return null;
};
draw2d.Workflow.prototype.getFigures=function(){
return this.figures;
};
draw2d.Workflow.prototype.getDocument=function(){
return new draw2d.CanvasDocument(this);
};
draw2d.Workflow.prototype.addSelectionListener=function(w){
if(w!==null){
if(w.onSelectionChanged){
this.selectionListeners.add(w);
}else{
throw "Object doesn't implement required callback method [onSelectionChanged]";
}
}
};
draw2d.Workflow.prototype.removeSelectionListener=function(w){
this.selectionListeners.remove(w);
};
draw2d.Workflow.prototype.setCurrentSelection=function(_5ea7){
if(_5ea7===null||this.currentSelection!=_5ea7){
this.hideResizeHandles();
this.hideLineResizeHandles();
}
this.currentSelection=_5ea7;
for(var i=0;i<this.selectionListeners.getSize();i++){
var w=this.selectionListeners.get(i);
if(w.onSelectionChanged){
w.onSelectionChanged(this.currentSelection,this.currentSelection?this.currentSelection.getModel():null);
}
}
if(_5ea7 instanceof draw2d.Line){
this.showLineResizeHandles(_5ea7);
if(!(_5ea7 instanceof draw2d.Connection)){
this.draggingLineCommand=line.createCommand(new draw2d.EditPolicy(draw2d.EditPolicy.MOVE));
if(this.draggingLineCommand!==null){
this.draggingLine=_5ea7;
}
}
}
};
draw2d.Workflow.prototype.getCurrentSelection=function(){
return this.currentSelection;
};
draw2d.Workflow.prototype.getLine=function(id){
var count=this.lines.getSize();
for(var i=0;i<count;i++){
var line=this.lines.get(i);
if(line.getId()==id){
return line;
}
}
return null;
};
draw2d.Workflow.prototype.getLines=function(){
return this.lines;
};
draw2d.Workflow.prototype.registerPort=function(port){
port.draggable.targets=this.dropTargets;
this.commonPorts.add(port);
this.dropTargets.add(port.dropable);
};
draw2d.Workflow.prototype.unregisterPort=function(port){
port.draggable.targets=null;
this.commonPorts.remove(port);
this.dropTargets.remove(port.dropable);
};
draw2d.Workflow.prototype.getCommandStack=function(){
return this.commandStack;
};
draw2d.Workflow.prototype.showConnectionLine=function(x1,y1,x2,y2){
this.connectionLine.setStartPoint(x1,y1);
this.connectionLine.setEndPoint(x2,y2);
if(this.connectionLine.canvas===null){
draw2d.Canvas.prototype.addFigure.call(this,this.connectionLine);
}
};
draw2d.Workflow.prototype.hideConnectionLine=function(){
if(this.connectionLine.canvas!==null){
draw2d.Canvas.prototype.removeFigure.call(this,this.connectionLine);
}
};
draw2d.Workflow.prototype.showLineResizeHandles=function(_5eb4){
var _5eb5=this.resizeHandleStart.getWidth()/2;
var _5eb6=this.resizeHandleStart.getHeight()/2;
var _5eb7=_5eb4.getStartPoint();
var _5eb8=_5eb4.getEndPoint();
draw2d.Canvas.prototype.addFigure.call(this,this.resizeHandleStart,_5eb7.x-_5eb5,_5eb7.y-_5eb5);
draw2d.Canvas.prototype.addFigure.call(this,this.resizeHandleEnd,_5eb8.x-_5eb5,_5eb8.y-_5eb5);
this.resizeHandleStart.setCanDrag(_5eb4.isResizeable());
this.resizeHandleEnd.setCanDrag(_5eb4.isResizeable());
if(_5eb4.isResizeable()){
this.resizeHandleStart.setBackgroundColor(draw2d.Workflow.COLOR_GREEN);
this.resizeHandleEnd.setBackgroundColor(draw2d.Workflow.COLOR_GREEN);
this.resizeHandleStart.draggable.targets=this.dropTargets;
this.resizeHandleEnd.draggable.targets=this.dropTargets;
}else{
this.resizeHandleStart.setBackgroundColor(null);
this.resizeHandleEnd.setBackgroundColor(null);
}
};
draw2d.Workflow.prototype.hideLineResizeHandles=function(){
if(this.resizeHandleStart.canvas!==null){
draw2d.Canvas.prototype.removeFigure.call(this,this.resizeHandleStart);
}
if(this.resizeHandleEnd.canvas!==null){
draw2d.Canvas.prototype.removeFigure.call(this,this.resizeHandleEnd);
}
};
draw2d.Workflow.prototype.showResizeHandles=function(_5eb9){
this.hideLineResizeHandles();
this.hideResizeHandles();
if(this.getEnableSmoothFigureHandling()==true&&this.getCurrentSelection()!=_5eb9){
this.resizeHandle1.setAlpha(0.01);
this.resizeHandle2.setAlpha(0.01);
this.resizeHandle3.setAlpha(0.01);
this.resizeHandle4.setAlpha(0.01);
this.resizeHandle5.setAlpha(0.01);
this.resizeHandle6.setAlpha(0.01);
this.resizeHandle7.setAlpha(0.01);
this.resizeHandle8.setAlpha(0.01);
}
var _5eba=this.resizeHandle1.getWidth();
var _5ebb=this.resizeHandle1.getHeight();
var _5ebc=_5eb9.getHeight();
var _5ebd=_5eb9.getWidth();
var xPos=_5eb9.getX();
var yPos=_5eb9.getY();
draw2d.Canvas.prototype.addFigure.call(this,this.resizeHandle1,xPos-_5eba,yPos-_5ebb);
draw2d.Canvas.prototype.addFigure.call(this,this.resizeHandle3,xPos+_5ebd,yPos-_5ebb);
draw2d.Canvas.prototype.addFigure.call(this,this.resizeHandle5,xPos+_5ebd,yPos+_5ebc);
draw2d.Canvas.prototype.addFigure.call(this,this.resizeHandle7,xPos-_5eba,yPos+_5ebc);
this.moveFront(this.resizeHandle1);
this.moveFront(this.resizeHandle3);
this.moveFront(this.resizeHandle5);
this.moveFront(this.resizeHandle7);
this.resizeHandle1.setCanDrag(_5eb9.isResizeable());
this.resizeHandle3.setCanDrag(_5eb9.isResizeable());
this.resizeHandle5.setCanDrag(_5eb9.isResizeable());
this.resizeHandle7.setCanDrag(_5eb9.isResizeable());
if(_5eb9.isResizeable()){
var green=new draw2d.Color(0,255,0);
this.resizeHandle1.setBackgroundColor(green);
this.resizeHandle3.setBackgroundColor(green);
this.resizeHandle5.setBackgroundColor(green);
this.resizeHandle7.setBackgroundColor(green);
}else{
this.resizeHandle1.setBackgroundColor(null);
this.resizeHandle3.setBackgroundColor(null);
this.resizeHandle5.setBackgroundColor(null);
this.resizeHandle7.setBackgroundColor(null);
}
if(_5eb9.isStrechable()&&_5eb9.isResizeable()){
this.resizeHandle2.setCanDrag(_5eb9.isResizeable());
this.resizeHandle4.setCanDrag(_5eb9.isResizeable());
this.resizeHandle6.setCanDrag(_5eb9.isResizeable());
this.resizeHandle8.setCanDrag(_5eb9.isResizeable());
draw2d.Canvas.prototype.addFigure.call(this,this.resizeHandle2,xPos+(_5ebd/2)-this.resizeHandleHalfWidth,yPos-_5ebb);
draw2d.Canvas.prototype.addFigure.call(this,this.resizeHandle4,xPos+_5ebd,yPos+(_5ebc/2)-(_5ebb/2));
draw2d.Canvas.prototype.addFigure.call(this,this.resizeHandle6,xPos+(_5ebd/2)-this.resizeHandleHalfWidth,yPos+_5ebc);
draw2d.Canvas.prototype.addFigure.call(this,this.resizeHandle8,xPos-_5eba,yPos+(_5ebc/2)-(_5ebb/2));
this.moveFront(this.resizeHandle2);
this.moveFront(this.resizeHandle4);
this.moveFront(this.resizeHandle6);
this.moveFront(this.resizeHandle8);
}
};
draw2d.Workflow.prototype.hideResizeHandles=function(){
if(this.resizeHandle1.canvas!==null){
draw2d.Canvas.prototype.removeFigure.call(this,this.resizeHandle1);
}
if(this.resizeHandle2.canvas!==null){
draw2d.Canvas.prototype.removeFigure.call(this,this.resizeHandle2);
}
if(this.resizeHandle3.canvas!==null){
draw2d.Canvas.prototype.removeFigure.call(this,this.resizeHandle3);
}
if(this.resizeHandle4.canvas!==null){
draw2d.Canvas.prototype.removeFigure.call(this,this.resizeHandle4);
}
if(this.resizeHandle5.canvas!==null){
draw2d.Canvas.prototype.removeFigure.call(this,this.resizeHandle5);
}
if(this.resizeHandle6.canvas!==null){
draw2d.Canvas.prototype.removeFigure.call(this,this.resizeHandle6);
}
if(this.resizeHandle7.canvas!==null){
draw2d.Canvas.prototype.removeFigure.call(this,this.resizeHandle7);
}
if(this.resizeHandle8.canvas!==null){
draw2d.Canvas.prototype.removeFigure.call(this,this.resizeHandle8);
}
};
draw2d.Workflow.prototype.moveResizeHandles=function(_5ec1){
var _5ec2=this.resizeHandle1.getWidth();
var _5ec3=this.resizeHandle1.getHeight();
var _5ec4=_5ec1.getHeight();
var _5ec5=_5ec1.getWidth();
var xPos=_5ec1.getX();
var yPos=_5ec1.getY();
this.resizeHandle1.setPosition(xPos-_5ec2,yPos-_5ec3);
this.resizeHandle3.setPosition(xPos+_5ec5,yPos-_5ec3);
this.resizeHandle5.setPosition(xPos+_5ec5,yPos+_5ec4);
this.resizeHandle7.setPosition(xPos-_5ec2,yPos+_5ec4);
if(_5ec1.isStrechable()){
this.resizeHandle2.setPosition(xPos+(_5ec5/2)-this.resizeHandleHalfWidth,yPos-_5ec3);
this.resizeHandle4.setPosition(xPos+_5ec5,yPos+(_5ec4/2)-(_5ec3/2));
this.resizeHandle6.setPosition(xPos+(_5ec5/2)-this.resizeHandleHalfWidth,yPos+_5ec4);
this.resizeHandle8.setPosition(xPos-_5ec2,yPos+(_5ec4/2)-(_5ec3/2));
}
};
draw2d.Workflow.prototype.onMouseDown=function(x,y){
this.dragging=true;
this.mouseDownPosX=x;
this.mouseDownPosY=y;
if(this.toolPalette!==null&&this.toolPalette.getActiveTool()!==null){
this.toolPalette.getActiveTool().execute(x,y);
}
this.showMenu(null);
var line=this.getBestLine(x,y);
if(line!==null&&line.isSelectable()){
this.setCurrentSelection(line);
}else{
this.setCurrentSelection(null);
}
};
draw2d.Workflow.prototype.onMouseUp=function(x,y){
this.dragging=false;
if(this.draggingLineCommand!==null){
this.getCommandStack().execute(this.draggingLineCommand);
this.draggingLine=null;
this.draggingLineCommand=null;
}
};
draw2d.Workflow.prototype.onMouseMove=function(x,y){
if(this.dragging===true&&this.draggingLine!==null){
var diffX=x-this.mouseDownPosX;
var diffY=y-this.mouseDownPosY;
this.draggingLine.startX=this.draggingLine.getStartX()+diffX;
this.draggingLine.startY=this.draggingLine.getStartY()+diffY;
this.draggingLine.setEndPoint(this.draggingLine.getEndX()+diffX,this.draggingLine.getEndY()+diffY);
this.mouseDownPosX=x;
this.mouseDownPosY=y;
this.showLineResizeHandles(this.currentSelection);
}else{
if(this.dragging===true&&this.panning===true){
var diffX=x-this.mouseDownPosX;
var diffY=y-this.mouseDownPosY;
this.scrollTo(this.getScrollLeft()-diffX,this.getScrollTop()-diffY,true);
this.onScroll();
}
}
};
draw2d.Workflow.prototype.onKeyDown=function(_5ed1,ctrl){
if(_5ed1==46&&this.currentSelection!==null){
this.commandStack.execute(this.currentSelection.createCommand(new draw2d.EditPolicy(draw2d.EditPolicy.DELETE)));
}else{
if(_5ed1==90&&ctrl){
this.commandStack.undo();
}else{
if(_5ed1==89&&ctrl){
this.commandStack.redo();
}
}
}
};
draw2d.Workflow.prototype.setDocumentDirty=function(){
try{
for(var i=0;i<this.dialogs.getSize();i++){
var d=this.dialogs.get(i);
if(d!==null&&d.onSetDocumentDirty){
d.onSetDocumentDirty();
}
}
if(this.snapToGeometryHelper!==null){
this.snapToGeometryHelper.onSetDocumentDirty();
}
if(this.snapToGridHelper!==null){
this.snapToGridHelper.onSetDocumentDirty();
}
}
catch(e){
pushErrorStack(e,"draw2d.Workflow.prototype.setDocumentDirty=function()");
}
};
draw2d.Workflow.prototype.snapToHelper=function(_5ed5,pos){
if(this.snapToGeometryHelper!==null){
if(_5ed5 instanceof draw2d.ResizeHandle){
var _5ed7=_5ed5.getSnapToGridAnchor();
pos.x+=_5ed7.x;
pos.y+=_5ed7.y;
var _5ed8=new draw2d.Point(pos.x,pos.y);
var _5ed9=_5ed5.getSnapToDirection();
var _5eda=this.snapToGeometryHelper.snapPoint(_5ed9,pos,_5ed8);
if((_5ed9&draw2d.SnapToHelper.EAST_WEST)&&!(_5eda&draw2d.SnapToHelper.EAST_WEST)){
this.showSnapToHelperLineVertical(_5ed8.x);
}else{
this.hideSnapToHelperLineVertical();
}
if((_5ed9&draw2d.SnapToHelper.NORTH_SOUTH)&&!(_5eda&draw2d.SnapToHelper.NORTH_SOUTH)){
this.showSnapToHelperLineHorizontal(_5ed8.y);
}else{
this.hideSnapToHelperLineHorizontal();
}
_5ed8.x-=_5ed7.x;
_5ed8.y-=_5ed7.y;
return _5ed8;
}else{
var _5edb=new draw2d.Dimension(pos.x,pos.y,_5ed5.getWidth(),_5ed5.getHeight());
var _5ed8=new draw2d.Dimension(pos.x,pos.y,_5ed5.getWidth(),_5ed5.getHeight());
var _5ed9=draw2d.SnapToHelper.NSEW;
var _5eda=this.snapToGeometryHelper.snapRectangle(_5edb,_5ed8);
if((_5ed9&draw2d.SnapToHelper.WEST)&&!(_5eda&draw2d.SnapToHelper.WEST)){
this.showSnapToHelperLineVertical(_5ed8.x);
}else{
if((_5ed9&draw2d.SnapToHelper.EAST)&&!(_5eda&draw2d.SnapToHelper.EAST)){
this.showSnapToHelperLineVertical(_5ed8.getX()+_5ed8.getWidth());
}else{
this.hideSnapToHelperLineVertical();
}
}
if((_5ed9&draw2d.SnapToHelper.NORTH)&&!(_5eda&draw2d.SnapToHelper.NORTH)){
this.showSnapToHelperLineHorizontal(_5ed8.y);
}else{
if((_5ed9&draw2d.SnapToHelper.SOUTH)&&!(_5eda&draw2d.SnapToHelper.SOUTH)){
this.showSnapToHelperLineHorizontal(_5ed8.getY()+_5ed8.getHeight());
}else{
this.hideSnapToHelperLineHorizontal();
}
}
return _5ed8.getTopLeft();
}
}else{
if(this.snapToGridHelper!==null){
var _5ed7=_5ed5.getSnapToGridAnchor();
pos.x=pos.x+_5ed7.x;
pos.y=pos.y+_5ed7.y;
var _5ed8=new draw2d.Point(pos.x,pos.y);
this.snapToGridHelper.snapPoint(0,pos,_5ed8);
_5ed8.x=_5ed8.x-_5ed7.x;
_5ed8.y=_5ed8.y-_5ed7.y;
return _5ed8;
}
}
return pos;
};
draw2d.Workflow.prototype.showSnapToHelperLineHorizontal=function(_5edc){
if(this.horizontalSnapToHelperLine===null){
this.horizontalSnapToHelperLine=new draw2d.Line();
this.horizontalSnapToHelperLine.setColor(new draw2d.Color(175,175,255));
this.addFigure(this.horizontalSnapToHelperLine);
}
this.horizontalSnapToHelperLine.setStartPoint(0,_5edc);
this.horizontalSnapToHelperLine.setEndPoint(this.getWidth(),_5edc);
};
draw2d.Workflow.prototype.showSnapToHelperLineVertical=function(_5edd){
if(this.verticalSnapToHelperLine===null){
this.verticalSnapToHelperLine=new draw2d.Line();
this.verticalSnapToHelperLine.setColor(new draw2d.Color(175,175,255));
this.addFigure(this.verticalSnapToHelperLine);
}
this.verticalSnapToHelperLine.setStartPoint(_5edd,0);
this.verticalSnapToHelperLine.setEndPoint(_5edd,this.getHeight());
};
draw2d.Workflow.prototype.hideSnapToHelperLines=function(){
this.hideSnapToHelperLineHorizontal();
this.hideSnapToHelperLineVertical();
};
draw2d.Workflow.prototype.hideSnapToHelperLineHorizontal=function(){
if(this.horizontalSnapToHelperLine!==null){
this.removeFigure(this.horizontalSnapToHelperLine);
this.horizontalSnapToHelperLine=null;
}
};
draw2d.Workflow.prototype.hideSnapToHelperLineVertical=function(){
if(this.verticalSnapToHelperLine!==null){
this.removeFigure(this.verticalSnapToHelperLine);
this.verticalSnapToHelperLine=null;
}
};
draw2d.WindowFigure=function(title){
this.title=title;
this.titlebar=null;
draw2d.Figure.call(this);
this.setDeleteable(false);
this.setCanSnapToHelper(false);
this.setZOrder(draw2d.WindowFigure.ZOrderIndex);
};
draw2d.WindowFigure.prototype=new draw2d.Figure();
draw2d.WindowFigure.prototype.type=":draw2d.WindowFigure";
draw2d.WindowFigure.ZOrderIndex=50000;
draw2d.WindowFigure.setZOrderBaseIndex=function(index){
draw2d.WindowFigure.ZOrderBaseIndex=index;
};
draw2d.WindowFigure.prototype.hasFixedPosition=function(){
return true;
};
draw2d.WindowFigure.prototype.hasTitleBar=function(){
return true;
};
draw2d.WindowFigure.prototype.createHTMLElement=function(){
var item=draw2d.Figure.prototype.createHTMLElement.call(this);
item.style.margin="0px";
item.style.padding="0px";
item.style.border="1px solid black";
item.style.backgroundImage="url(window_bg.png)";
item.style.zIndex=draw2d.WindowFigure.ZOrderIndex;
item.style.cursor=null;
item.className="WindowFigure";
if(this.hasTitleBar()){
this.titlebar=document.createElement("div");
this.titlebar.style.position="absolute";
this.titlebar.style.left="0px";
this.titlebar.style.top="0px";
this.titlebar.style.width=this.getWidth()+"px";
this.titlebar.style.height="15px";
this.titlebar.style.margin="0px";
this.titlebar.style.padding="0px";
this.titlebar.style.font="normal 10px verdana";
this.titlebar.style.backgroundColor="blue";
this.titlebar.style.borderBottom="2px solid gray";
this.titlebar.style.whiteSpace="nowrap";
this.titlebar.style.textAlign="center";
this.titlebar.style.backgroundImage="url(window_toolbar.png)";
this.titlebar.className="WindowFigure_titlebar";
this.textNode=document.createTextNode(this.title);
this.titlebar.appendChild(this.textNode);
this.disableTextSelection(this.titlebar);
item.appendChild(this.titlebar);
}
return item;
};
draw2d.WindowFigure.prototype.setDocumentDirty=function(_5556){
};
draw2d.WindowFigure.prototype.onDragend=function(){
};
draw2d.WindowFigure.prototype.onDragstart=function(x,y){
if(this.titlebar===null){
return false;
}
if(this.canDrag===true&&x<parseInt(this.titlebar.style.width)&&y<parseInt(this.titlebar.style.height)){
return true;
}
return false;
};
draw2d.WindowFigure.prototype.isSelectable=function(){
return false;
};
draw2d.WindowFigure.prototype.setCanDrag=function(flag){
draw2d.Figure.prototype.setCanDrag.call(this,flag);
this.html.style.cursor="";
if(this.titlebar===null){
return;
}
if(flag){
this.titlebar.style.cursor="move";
}else{
this.titlebar.style.cursor="";
}
};
draw2d.WindowFigure.prototype.setWorkflow=function(_555a){
var _555b=this.workflow;
draw2d.Figure.prototype.setWorkflow.call(this,_555a);
if(_555b!==null){
_555b.removeSelectionListener(this);
}
if(this.workflow!==null){
this.workflow.addSelectionListener(this);
}
};
draw2d.WindowFigure.prototype.setDimension=function(w,h){
draw2d.Figure.prototype.setDimension.call(this,w,h);
if(this.titlebar!==null){
this.titlebar.style.width=this.getWidth()+"px";
}
};
draw2d.WindowFigure.prototype.setTitle=function(title){
this.title=title;
};
draw2d.WindowFigure.prototype.getMinWidth=function(){
return 50;
};
draw2d.WindowFigure.prototype.getMinHeight=function(){
return 50;
};
draw2d.WindowFigure.prototype.isResizeable=function(){
return false;
};
draw2d.WindowFigure.prototype.setAlpha=function(_555f){
};
draw2d.WindowFigure.prototype.setBackgroundColor=function(color){
this.bgColor=color;
if(this.bgColor!==null){
this.html.style.backgroundColor=this.bgColor.getHTMLStyle();
}else{
this.html.style.backgroundColor="transparent";
this.html.style.backgroundImage="";
}
};
draw2d.WindowFigure.prototype.setColor=function(color){
this.lineColor=color;
if(this.lineColor!==null){
this.html.style.border=this.lineStroke+"px solid "+this.lineColor.getHTMLStyle();
}else{
this.html.style.border="0px";
}
};
draw2d.WindowFigure.prototype.setLineWidth=function(w){
this.lineStroke=w;
this.html.style.border=this.lineStroke+"px solid black";
};
draw2d.WindowFigure.prototype.onSelectionChanged=function(_5563,model){
};
draw2d.Button=function(_59e9,width,_59eb){
this.x=0;
this.y=0;
this.width=24;
this.height=24;
this.id=draw2d.UUID.create();
this.enabled=true;
this.active=false;
this.palette=_59e9;
this.html=this.createHTMLElement();
if(width!==undefined&&_59eb!==undefined){
this.setDimension(width,_59eb);
}else{
this.setDimension(24,24);
}
};
draw2d.Button.prototype.type="draw2d.Button";
draw2d.Button.prototype.dispose=function(){
};
draw2d.Button.prototype.getImageUrl=function(){
return this.type+".png";
};
draw2d.Button.prototype.createHTMLElement=function(){
var item=document.createElement("div");
item.id=this.id;
item.style.position="absolute";
item.style.left=this.x+"px";
item.style.top=this.y+"px";
item.style.height=this.width+"px";
item.style.width=this.height+"px";
item.style.margin="0px";
item.style.padding="0px";
item.style.outline="none";
if(this.getImageUrl()!==null){
item.style.backgroundImage="url("+this.getImageUrl()+")";
}else{
item.style.backgroundImage="";
}
var oThis=this;
this.omousedown=function(event){
if(oThis.enabled){
oThis.setActive(true);
}
event.cancelBubble=true;
event.returnValue=false;
};
this.omouseup=function(event){
if(oThis.enabled){
oThis.setActive(false);
oThis.execute();
oThis.palette.setActiveTool(null);
}
event.cancelBubble=true;
event.returnValue=false;
};
if(item.addEventListener){
item.addEventListener("mousedown",this.omousedown,false);
item.addEventListener("mouseup",this.omouseup,false);
}else{
if(item.attachEvent){
item.attachEvent("onmousedown",this.omousedown);
item.attachEvent("onmouseup",this.omouseup);
}
}
return item;
};
draw2d.Button.prototype.getHTMLElement=function(){
if(this.html===null){
this.html=this.createHTMLElement();
}
return this.html;
};
draw2d.Button.prototype.execute=function(){
};
draw2d.Button.prototype.setTooltip=function(_59f0){
this.tooltip=_59f0;
if(this.tooltip!==null){
this.html.title=this.tooltip;
}else{
this.html.title="";
}
};
draw2d.Button.prototype.getWorkflow=function(){
return this.getToolPalette().getWorkflow();
};
draw2d.Button.prototype.getToolPalette=function(){
return this.palette;
};
draw2d.Button.prototype.setActive=function(flag){
if(!this.enabled){
return;
}
this.active=flag;
if(flag===true){
this.html.style.border="1px inset";
}else{
this.html.style.border="0px";
}
};
draw2d.Button.prototype.isActive=function(){
return this.active;
};
draw2d.Button.prototype.setEnabled=function(flag){
this.enabled=flag;
if(flag){
this.html.style.filter="alpha(opacity=100)";
this.html.style.opacity="1.0";
}else{
this.html.style.filter="alpha(opacity=30)";
this.html.style.opacity="0.3";
}
};
draw2d.Button.prototype.setDimension=function(w,h){
this.width=w;
this.height=h;
if(this.html===null){
return;
}
this.html.style.width=this.width+"px";
this.html.style.height=this.height+"px";
};
draw2d.Button.prototype.setPosition=function(xPos,yPos){
this.x=Math.max(0,xPos);
this.y=Math.max(0,yPos);
if(this.html===null){
return;
}
this.html.style.left=this.x+"px";
this.html.style.top=this.y+"px";
};
draw2d.Button.prototype.getWidth=function(){
return this.width;
};
draw2d.Button.prototype.getHeight=function(){
return this.height;
};
draw2d.Button.prototype.getY=function(){
return this.y;
};
draw2d.Button.prototype.getX=function(){
return this.x;
};
draw2d.Button.prototype.getPosition=function(){
return new draw2d.Point(this.x,this.y);
};
draw2d.ToggleButton=function(_5c09){
draw2d.Button.call(this,_5c09);
this.isDownFlag=false;
};
draw2d.ToggleButton.prototype=new draw2d.Button();
draw2d.ToggleButton.prototype.type="draw2d.ToggleButton";
draw2d.ToggleButton.prototype.createHTMLElement=function(){
var item=document.createElement("div");
item.id=this.id;
item.style.position="absolute";
item.style.left=this.x+"px";
item.style.top=this.y+"px";
item.style.height="24px";
item.style.width="24px";
item.style.margin="0px";
item.style.padding="0px";
if(this.getImageUrl()!==null){
item.style.backgroundImage="url("+this.getImageUrl()+")";
}else{
item.style.backgroundImage="";
}
var oThis=this;
this.omousedown=function(event){
if(oThis.enabled){
if(!oThis.isDown()){
draw2d.Button.prototype.setActive.call(oThis,true);
}
}
event.cancelBubble=true;
event.returnValue=false;
};
this.omouseup=function(event){
if(oThis.enabled){
if(oThis.isDown()){
draw2d.Button.prototype.setActive.call(oThis,false);
}
oThis.isDownFlag=!oThis.isDownFlag;
oThis.execute();
}
event.cancelBubble=true;
event.returnValue=false;
};
if(item.addEventListener){
item.addEventListener("mousedown",this.omousedown,false);
item.addEventListener("mouseup",this.omouseup,false);
}else{
if(item.attachEvent){
item.attachEvent("onmousedown",this.omousedown);
item.attachEvent("onmouseup",this.omouseup);
}
}
return item;
};
draw2d.ToggleButton.prototype.isDown=function(){
return this.isDownFlag;
};
draw2d.ToggleButton.prototype.setActive=function(flag){
draw2d.Button.prototype.setActive.call(this,flag);
this.isDownFlag=flag;
};
draw2d.ToggleButton.prototype.execute=function(){
};
draw2d.ToolGeneric=function(_6005){
this.x=0;
this.y=0;
this.enabled=true;
this.tooltip=null;
this.palette=_6005;
this.html=this.createHTMLElement();
this.setDimension(10,10);
};
draw2d.ToolGeneric.prototype.type="draw2d.ToolGeneric";
draw2d.ToolGeneric.prototype.dispose=function(){
};
draw2d.ToolGeneric.prototype.getImageUrl=function(){
return this.type+".png";
};
draw2d.ToolGeneric.prototype.getWorkflow=function(){
return this.getToolPalette().getWorkflow();
};
draw2d.ToolGeneric.prototype.getToolPalette=function(){
return this.palette;
};
draw2d.ToolGeneric.prototype.createHTMLElement=function(){
var item=document.createElement("div");
item.id=this.id;
item.style.position="absolute";
item.style.left=this.x+"px";
item.style.top=this.y+"px";
item.style.height="24px";
item.style.width="24px";
item.style.margin="0px";
item.style.padding="0px";
if(this.getImageUrl()!==null){
item.style.backgroundImage="url("+this.getImageUrl()+")";
}else{
item.style.backgroundImage="";
}
var oThis=this;
this.click=function(event){
if(oThis.enabled){
oThis.palette.setActiveTool(oThis);
}
event.cancelBubble=true;
event.returnValue=false;
};
if(item.addEventListener){
item.addEventListener("click",this.click,false);
}else{
if(item.attachEvent){
item.attachEvent("onclick",this.click);
}
}
if(this.tooltip!==null){
item.title=this.tooltip;
}else{
item.title="";
}
return item;
};
draw2d.ToolGeneric.prototype.getHTMLElement=function(){
if(this.html===null){
this.html=this.createHTMLElement();
}
return this.html;
};
draw2d.ToolGeneric.prototype.execute=function(x,y){
if(this.enabled){
this.palette.setActiveTool(null);
}
};
draw2d.ToolGeneric.prototype.setTooltip=function(_600b){
this.tooltip=_600b;
if(this.tooltip!==null){
this.html.title=this.tooltip;
}else{
this.html.title="";
}
};
draw2d.ToolGeneric.prototype.setActive=function(flag){
if(!this.enabled){
return;
}
if(flag===true){
this.html.style.border="1px inset";
}else{
this.html.style.border="0px";
}
};
draw2d.ToolGeneric.prototype.setEnabled=function(flag){
this.enabled=flag;
if(flag){
this.html.style.filter="alpha(opacity=100)";
this.html.style.opacity="1.0";
}else{
this.html.style.filter="alpha(opacity=30)";
this.html.style.opacity="0.3";
}
};
draw2d.ToolGeneric.prototype.setDimension=function(w,h){
this.width=w;
this.height=h;
if(this.html===null){
return;
}
this.html.style.width=this.width+"px";
this.html.style.height=this.height+"px";
};
draw2d.ToolGeneric.prototype.setPosition=function(xPos,yPos){
this.x=Math.max(0,xPos);
this.y=Math.max(0,yPos);
if(this.html===null){
return;
}
this.html.style.left=this.x+"px";
this.html.style.top=this.y+"px";
};
draw2d.ToolGeneric.prototype.getWidth=function(){
return this.width;
};
draw2d.ToolGeneric.prototype.getHeight=function(){
return this.height;
};
draw2d.ToolGeneric.prototype.getY=function(){
return this.y;
};
draw2d.ToolGeneric.prototype.getX=function(){
return this.x;
};
draw2d.ToolGeneric.prototype.getPosition=function(){
return new draw2d.Point(this.x,this.y);
};
draw2d.ToolPalette=function(title){
draw2d.WindowFigure.call(this,title);
this.setDimension(75,400);
this.activeTool=null;
this.children={};
};
draw2d.ToolPalette.prototype=new draw2d.WindowFigure();
draw2d.ToolPalette.prototype.type="draw2d.ToolPalette";
draw2d.ToolPalette.prototype.dispose=function(){
draw2d.WindowFigure.prototype.dispose.call(this);
};
draw2d.ToolPalette.prototype.createHTMLElement=function(){
var item=draw2d.WindowFigure.prototype.createHTMLElement.call(this);
this.scrollarea=document.createElement("div");
this.scrollarea.style.position="absolute";
this.scrollarea.style.left="0px";
if(this.hasTitleBar()){
this.scrollarea.style.top="15px";
}else{
this.scrollarea.style.top="0px";
}
this.scrollarea.style.width=this.getWidth()+"px";
this.scrollarea.style.height="15px";
this.scrollarea.style.margin="0px";
this.scrollarea.style.padding="0px";
this.scrollarea.style.font="normal 10px verdana";
this.scrollarea.style.borderBottom="2px solid gray";
this.scrollarea.style.whiteSpace="nowrap";
this.scrollarea.style.textAlign="center";
this.scrollarea.style.overflowX="auto";
this.scrollarea.style.overflowY="auto";
this.scrollarea.style.overflow="auto";
item.appendChild(this.scrollarea);
return item;
};
draw2d.ToolPalette.prototype.setDimension=function(w,h){
draw2d.WindowFigure.prototype.setDimension.call(this,w,h);
if(this.scrollarea!==null){
this.scrollarea.style.width=this.getWidth()+"px";
if(this.hasTitleBar()){
this.scrollarea.style.height=(this.getHeight()-15)+"px";
}else{
this.scrollarea.style.height=this.getHeight()+"px";
}
}
};
draw2d.ToolPalette.prototype.addChild=function(item){
this.children[item.id]=item;
this.scrollarea.appendChild(item.getHTMLElement());
};
draw2d.ToolPalette.prototype.getChild=function(id){
return this.children[id];
};
draw2d.ToolPalette.prototype.getActiveTool=function(){
return this.activeTool;
};
draw2d.ToolPalette.prototype.setActiveTool=function(tool){
if(this.activeTool!=tool&&this.activeTool!==null){
this.activeTool.setActive(false);
}
if(tool!==null){
tool.setActive(true);
}
this.activeTool=tool;
};
draw2d.Dialog=function(title){
this.buttonbar=null;
if(title){
draw2d.WindowFigure.call(this,title);
}else{
draw2d.WindowFigure.call(this,"Dialog");
}
this.setDimension(400,300);
};
draw2d.Dialog.prototype=new draw2d.WindowFigure();
draw2d.Dialog.prototype.type="draw2d.Dialog";
draw2d.Dialog.prototype.createHTMLElement=function(){
var item=draw2d.WindowFigure.prototype.createHTMLElement.call(this);
var oThis=this;
this.buttonbar=document.createElement("div");
this.buttonbar.style.position="absolute";
this.buttonbar.style.left="0px";
this.buttonbar.style.bottom="0px";
this.buttonbar.style.width=this.getWidth()+"px";
this.buttonbar.style.height="30px";
this.buttonbar.style.margin="0px";
this.buttonbar.style.padding="0px";
this.buttonbar.style.font="normal 10px verdana";
this.buttonbar.style.backgroundColor="#c0c0c0";
this.buttonbar.style.borderBottom="2px solid gray";
this.buttonbar.style.whiteSpace="nowrap";
this.buttonbar.style.textAlign="center";
this.buttonbar.className="Dialog_buttonbar";
this.okbutton=document.createElement("button");
this.okbutton.style.border="1px solid gray";
this.okbutton.style.font="normal 10px verdana";
this.okbutton.style.width="80px";
this.okbutton.style.margin="5px";
this.okbutton.className="Dialog_okbutton";
this.okbutton.innerHTML="Ok";
this.okbutton.onclick=function(){
var error=null;
try{
oThis.onOk();
}
catch(e){
error=e;
}
oThis.workflow.removeFigure(oThis);
if(error!==null){
throw error;
}
};
this.buttonbar.appendChild(this.okbutton);
this.cancelbutton=document.createElement("button");
this.cancelbutton.innerHTML="Cancel";
this.cancelbutton.style.font="normal 10px verdana";
this.cancelbutton.style.border="1px solid gray";
this.cancelbutton.style.width="80px";
this.cancelbutton.style.margin="5px";
this.cancelbutton.className="Dialog_cancelbutton";
this.cancelbutton.onclick=function(){
var error=null;
try{
oThis.onCancel();
}
catch(e){
error=e;
}
oThis.workflow.removeFigure(oThis);
if(error!==null){
throw error;
}
};
this.buttonbar.appendChild(this.cancelbutton);
item.appendChild(this.buttonbar);
return item;
};
draw2d.Dialog.prototype.onOk=function(){
};
draw2d.Dialog.prototype.onCancel=function(){
};
draw2d.Dialog.prototype.setDimension=function(w,h){
draw2d.WindowFigure.prototype.setDimension.call(this,w,h);
if(this.buttonbar!==null){
this.buttonbar.style.width=this.getWidth()+"px";
}
};
draw2d.Dialog.prototype.setWorkflow=function(_573d){
draw2d.WindowFigure.prototype.setWorkflow.call(this,_573d);
this.setFocus();
};
draw2d.Dialog.prototype.setFocus=function(){
};
draw2d.Dialog.prototype.onSetDocumentDirty=function(){
};
draw2d.InputDialog=function(){
draw2d.Dialog.call(this);
this.setDimension(400,100);
};
draw2d.InputDialog.prototype=new draw2d.Dialog();
draw2d.InputDialog.prototype.type="draw2d.InputDialog";
draw2d.InputDialog.prototype.createHTMLElement=function(){
var item=draw2d.Dialog.prototype.createHTMLElement.call(this);
return item;
};
draw2d.InputDialog.prototype.onOk=function(){
this.workflow.removeFigure(this);
};
draw2d.InputDialog.prototype.onCancel=function(){
this.workflow.removeFigure(this);
};
draw2d.PropertyDialog=function(_5dd5,_5dd6,label){
this.figure=_5dd5;
this.propertyName=_5dd6;
this.label=label;
draw2d.Dialog.call(this);
this.setDimension(400,120);
};
draw2d.PropertyDialog.prototype=new draw2d.Dialog();
draw2d.PropertyDialog.prototype.type="draw2d.PropertyDialog";
draw2d.PropertyDialog.prototype.createHTMLElement=function(){
var item=draw2d.Dialog.prototype.createHTMLElement.call(this);
var _5dd9=document.createElement("form");
_5dd9.style.position="absolute";
_5dd9.style.left="10px";
_5dd9.style.top="30px";
_5dd9.style.width="375px";
_5dd9.style.font="normal 10px verdana";
item.appendChild(_5dd9);
this.labelDiv=document.createElement("div");
this.labelDiv.innerHTML=this.label;
this.disableTextSelection(this.labelDiv);
_5dd9.appendChild(this.labelDiv);
this.input=document.createElement("input");
this.input.style.border="1px solid gray";
this.input.style.font="normal 10px verdana";
this.input.type="text";
var value=this.figure.getProperty(this.propertyName);
if(value){
this.input.value=value;
}else{
this.input.value="";
}
this.input.style.width="100%";
_5dd9.appendChild(this.input);
this.input.focus();
return item;
};
draw2d.PropertyDialog.prototype.onOk=function(){
draw2d.Dialog.prototype.onOk.call(this);
this.figure.setProperty(this.propertyName,this.input.value);
};
draw2d.AnnotationDialog=function(_5b5b){
this.figure=_5b5b;
draw2d.Dialog.call(this);
this.setDimension(400,100);
};
draw2d.AnnotationDialog.prototype=new draw2d.Dialog();
draw2d.AnnotationDialog.prototype.type="draw2d.AnnotationDialog";
draw2d.AnnotationDialog.prototype.createHTMLElement=function(){
var item=draw2d.Dialog.prototype.createHTMLElement.call(this);
var _5b5d=document.createElement("form");
_5b5d.style.position="absolute";
_5b5d.style.left="10px";
_5b5d.style.top="30px";
_5b5d.style.width="375px";
_5b5d.style.font="normal 10px verdana";
item.appendChild(_5b5d);
this.label=document.createTextNode("Text");
_5b5d.appendChild(this.label);
this.input=document.createElement("input");
this.input.style.border="1px solid gray";
this.input.style.font="normal 10px verdana";
this.input.type="text";
var value=this.figure.getText();
if(value){
this.input.value=value;
}else{
this.input.value="";
}
this.input.style.width="100%";
_5b5d.appendChild(this.input);
this.input.focus();
return item;
};
draw2d.AnnotationDialog.prototype.onOk=function(){
this.workflow.getCommandStack().execute(new draw2d.CommandSetText(this.figure,this.input.value));
this.workflow.removeFigure(this);
};
draw2d.PropertyWindow=function(){
this.currentSelection=null;
draw2d.WindowFigure.call(this,"Property Window");
this.setDimension(200,100);
};
draw2d.PropertyWindow.prototype=new draw2d.WindowFigure();
draw2d.PropertyWindow.prototype.type="draw2d.PropertyWindow";
draw2d.PropertyWindow.prototype.dispose=function(){
draw2d.WindowFigure.prototype.dispose.call(this);
};
draw2d.PropertyWindow.prototype.createHTMLElement=function(){
var item=draw2d.WindowFigure.prototype.createHTMLElement.call(this);
item.appendChild(this.createLabel("Type:",15,25));
item.appendChild(this.createLabel("X :",15,50));
item.appendChild(this.createLabel("Y :",15,70));
item.appendChild(this.createLabel("Width :",85,50));
item.appendChild(this.createLabel("Height :",85,70));
this.labelType=this.createLabel("",50,25);
this.labelX=this.createLabel("",40,50);
this.labelY=this.createLabel("",40,70);
this.labelWidth=this.createLabel("",135,50);
this.labelHeight=this.createLabel("",135,70);
this.labelType.style.fontWeight="normal";
this.labelX.style.fontWeight="normal";
this.labelY.style.fontWeight="normal";
this.labelWidth.style.fontWeight="normal";
this.labelHeight.style.fontWeight="normal";
item.appendChild(this.labelType);
item.appendChild(this.labelX);
item.appendChild(this.labelY);
item.appendChild(this.labelWidth);
item.appendChild(this.labelHeight);
return item;
};
draw2d.PropertyWindow.prototype.onSelectionChanged=function(_4e0c){
draw2d.WindowFigure.prototype.onSelectionChanged.call(this,_4e0c);
if(this.currentSelection!==null){
this.currentSelection.detachMoveListener(this);
}
this.currentSelection=_4e0c;
if(_4e0c!==null&&_4e0c!=this){
this.labelType.innerHTML=_4e0c.type;
if(_4e0c.getX){
this.labelX.innerHTML=_4e0c.getX();
this.labelY.innerHTML=_4e0c.getY();
this.labelWidth.innerHTML=_4e0c.getWidth();
this.labelHeight.innerHTML=_4e0c.getHeight();
this.currentSelection=_4e0c;
this.currentSelection.attachMoveListener(this);
}else{
this.labelX.innerHTML="";
this.labelY.innerHTML="";
this.labelWidth.innerHTML="";
this.labelHeight.innerHTML="";
}
}else{
this.labelType.innerHTML="&lt;none&gt;";
this.labelX.innerHTML="";
this.labelY.innerHTML="";
this.labelWidth.innerHTML="";
this.labelHeight.innerHTML="";
}
};
draw2d.PropertyWindow.prototype.getCurrentSelection=function(){
return this.currentSelection;
};
draw2d.PropertyWindow.prototype.onOtherFigureMoved=function(_4e0d){
if(_4e0d==this.currentSelection){
this.onSelectionChanged(_4e0d);
}
};
draw2d.PropertyWindow.prototype.createLabel=function(text,x,y){
var l=document.createElement("div");
l.style.position="absolute";
l.style.left=x+"px";
l.style.top=y+"px";
l.style.font="normal 10px verdana";
l.style.whiteSpace="nowrap";
l.style.fontWeight="bold";
l.innerHTML=text;
return l;
};
draw2d.ColorDialog=function(){
this.maxValue={"h":"359","s":"100","v":"100"};
this.HSV={0:359,1:100,2:100};
this.slideHSV={0:359,1:100,2:100};
this.SVHeight=165;
this.wSV=162;
this.wH=162;
draw2d.Dialog.call(this,"Color Chooser");
this.loadSV();
this.setColor(new draw2d.Color(255,0,0));
this.setDimension(219,244);
};
draw2d.ColorDialog.prototype=new draw2d.Dialog();
draw2d.ColorDialog.prototype.type="draw2d.ColorDialog";
draw2d.ColorDialog.prototype.createHTMLElement=function(){
var oThis=this;
var item=draw2d.Dialog.prototype.createHTMLElement.call(this);
this.outerDiv=document.createElement("div");
this.outerDiv.id="plugin";
this.outerDiv.style.top="15px";
this.outerDiv.style.left="0px";
this.outerDiv.style.width="201px";
this.outerDiv.style.position="absolute";
this.outerDiv.style.padding="9px";
this.outerDiv.display="block";
this.outerDiv.style.background="#0d0d0d";
this.plugHEX=document.createElement("div");
this.plugHEX.id="plugHEX";
this.plugHEX.innerHTML="F1FFCC";
this.plugHEX.style.color="white";
this.plugHEX.style.font="normal 10px verdana";
this.outerDiv.appendChild(this.plugHEX);
this.SV=document.createElement("div");
this.SV.onmousedown=function(event){
oThis.mouseDownSV(oThis.SVslide,event);
};
this.SV.id="SV";
this.SV.style.cursor="crosshair";
this.SV.style.background="#FF0000 url(SatVal.png)";
this.SV.style.position="absolute";
this.SV.style.height="166px";
this.SV.style.width="167px";
this.SV.style.marginRight="10px";
this.SV.style.filter="progid:DXImageTransform.Microsoft.AlphaImageLoader(src='SatVal.png', sizingMethod='scale')";
this.SV.style["float"]="left";
this.outerDiv.appendChild(this.SV);
this.SVslide=document.createElement("div");
this.SVslide.onmousedown=function(event){
oThis.mouseDownSV(event);
};
this.SVslide.style.top="40px";
this.SVslide.style.left="40px";
this.SVslide.style.position="absolute";
this.SVslide.style.cursor="crosshair";
this.SVslide.style.background="url(slide.gif)";
this.SVslide.style.height="9px";
this.SVslide.style.width="9px";
this.SVslide.style.lineHeight="1px";
this.outerDiv.appendChild(this.SVslide);
this.H=document.createElement("form");
this.H.id="H";
this.H.onmousedown=function(event){
oThis.mouseDownH(event);
};
this.H.style.border="1px solid #000000";
this.H.style.cursor="crosshair";
this.H.style.position="absolute";
this.H.style.width="19px";
this.H.style.top="28px";
this.H.style.left="191px";
this.outerDiv.appendChild(this.H);
this.Hslide=document.createElement("div");
this.Hslide.style.top="-7px";
this.Hslide.style.left="-8px";
this.Hslide.style.background="url(slideHue.gif)";
this.Hslide.style.height="5px";
this.Hslide.style.width="33px";
this.Hslide.style.position="absolute";
this.Hslide.style.lineHeight="1px";
this.H.appendChild(this.Hslide);
this.Hmodel=document.createElement("div");
this.Hmodel.style.height="1px";
this.Hmodel.style.width="19px";
this.Hmodel.style.lineHeight="1px";
this.Hmodel.style.margin="0px";
this.Hmodel.style.padding="0px";
this.Hmodel.style.fontSize="1px";
this.H.appendChild(this.Hmodel);
item.appendChild(this.outerDiv);
return item;
};
draw2d.ColorDialog.prototype.onOk=function(){
draw2d.Dialog.prototype.onOk.call(this);
};
draw2d.browser=function(v){
return (Math.max(navigator.userAgent.toLowerCase().indexOf(v),0));
};
draw2d.ColorDialog.prototype.showColor=function(c){
this.plugHEX.style.background="#"+c;
this.plugHEX.innerHTML=c;
};
draw2d.ColorDialog.prototype.getSelectedColor=function(){
var rgb=this.hex2rgb(this.plugHEX.innerHTML);
return new draw2d.Color(rgb[0],rgb[1],rgb[2]);
};
draw2d.ColorDialog.prototype.setColor=function(color){
if(color===null){
color=new draw2d.Color(100,100,100);
}
var hex=this.rgb2hex(Array(color.getRed(),color.getGreen(),color.getBlue()));
this.updateH(hex);
};
draw2d.ColorDialog.prototype.XY=function(e,v){
var z=draw2d.browser("msie")?Array(event.clientX+document.body.scrollLeft,event.clientY+document.body.scrollTop):Array(e.pageX,e.pageY);
return z[v];
};
draw2d.ColorDialog.prototype.mkHSV=function(a,b,c){
return (Math.min(a,Math.max(0,Math.ceil((parseInt(c)/b)*a))));
};
draw2d.ColorDialog.prototype.ckHSV=function(a,b){
if(a>=0&&a<=b){
return (a);
}else{
if(a>b){
return (b);
}else{
if(a<0){
return ("-"+oo);
}
}
}
};
draw2d.ColorDialog.prototype.mouseDownH=function(e){
this.slideHSV[0]=this.HSV[0];
var oThis=this;
this.H.onmousemove=function(e){
oThis.dragH(e);
};
this.H.onmouseup=function(e){
oThis.H.onmousemove="";
oThis.H.onmouseup="";
};
this.dragH(e);
};
draw2d.ColorDialog.prototype.dragH=function(e){
var y=this.XY(e,1)-this.getY()-40;
this.Hslide.style.top=(this.ckHSV(y,this.wH)-5)+"px";
this.slideHSV[0]=this.mkHSV(359,this.wH,this.Hslide.style.top);
this.updateSV();
this.showColor(this.commit());
this.SV.style.backgroundColor="#"+this.hsv2hex(Array(this.HSV[0],100,100));
};
draw2d.ColorDialog.prototype.mouseDownSV=function(o,e){
this.slideHSV[0]=this.HSV[0];
var oThis=this;
function reset(){
oThis.SV.onmousemove="";
oThis.SV.onmouseup="";
oThis.SVslide.onmousemove="";
oThis.SVslide.onmouseup="";
}
this.SV.onmousemove=function(e){
oThis.dragSV(e);
};
this.SV.onmouseup=reset;
this.SVslide.onmousemove=function(e){
oThis.dragSV(e);
};
this.SVslide.onmouseup=reset;
this.dragSV(e);
};
draw2d.ColorDialog.prototype.dragSV=function(e){
var x=this.XY(e,0)-this.getX()-1;
var y=this.XY(e,1)-this.getY()-20;
this.SVslide.style.left=this.ckHSV(x,this.wSV)+"px";
this.SVslide.style.top=this.ckHSV(y,this.wSV)+"px";
this.slideHSV[1]=this.mkHSV(100,this.wSV,this.SVslide.style.left);
this.slideHSV[2]=100-this.mkHSV(100,this.wSV,this.SVslide.style.top);
this.updateSV();
};
draw2d.ColorDialog.prototype.commit=function(){
var r="hsv";
var z={};
var j="";
for(var i=0;i<=r.length-1;i++){
j=r.substr(i,1);
z[i]=(j=="h")?this.maxValue[j]-this.mkHSV(this.maxValue[j],this.wH,this.Hslide.style.top):this.HSV[i];
}
return (this.updateSV(this.hsv2hex(z)));
};
draw2d.ColorDialog.prototype.updateSV=function(v){
this.HSV=v?this.hex2hsv(v):Array(this.slideHSV[0],this.slideHSV[1],this.slideHSV[2]);
if(!v){
v=this.hsv2hex(Array(this.slideHSV[0],this.slideHSV[1],this.slideHSV[2]));
}
this.showColor(v);
return v;
};
draw2d.ColorDialog.prototype.loadSV=function(){
var z="";
for(var i=this.SVHeight;i>=0;i--){
z+="<div style=\"background:#"+this.hsv2hex(Array(Math.round((359/this.SVHeight)*i),100,100))+";\"><br/></div>";
}
this.Hmodel.innerHTML=z;
};
draw2d.ColorDialog.prototype.updateH=function(v){
this.plugHEX.innerHTML=v;
this.HSV=this.hex2hsv(v);
this.SV.style.backgroundColor="#"+this.hsv2hex(Array(this.HSV[0],100,100));
this.SVslide.style.top=(parseInt(this.wSV-this.wSV*(this.HSV[1]/100))+20)+"px";
this.SVslide.style.left=(parseInt(this.wSV*(this.HSV[1]/100))+5)+"px";
this.Hslide.style.top=(parseInt(this.wH*((this.maxValue["h"]-this.HSV[0])/this.maxValue["h"]))-7)+"px";
};
draw2d.ColorDialog.prototype.toHex=function(v){
v=Math.round(Math.min(Math.max(0,v),255));
return ("0123456789ABCDEF".charAt((v-v%16)/16)+"0123456789ABCDEF".charAt(v%16));
};
draw2d.ColorDialog.prototype.hex2rgb=function(r){
return ({0:parseInt(r.substr(0,2),16),1:parseInt(r.substr(2,2),16),2:parseInt(r.substr(4,2),16)});
};
draw2d.ColorDialog.prototype.rgb2hex=function(r){
return (this.toHex(r[0])+this.toHex(r[1])+this.toHex(r[2]));
};
draw2d.ColorDialog.prototype.hsv2hex=function(h){
return (this.rgb2hex(this.hsv2rgb(h)));
};
draw2d.ColorDialog.prototype.hex2hsv=function(v){
return (this.rgb2hsv(this.hex2rgb(v)));
};
draw2d.ColorDialog.prototype.rgb2hsv=function(r){
var max=Math.max(r[0],r[1],r[2]);
var delta=max-Math.min(r[0],r[1],r[2]);
var H;
var S;
var V;
if(max!=0){
S=Math.round(delta/max*100);
if(r[0]==max){
H=(r[1]-r[2])/delta;
}else{
if(r[1]==max){
H=2+(r[2]-r[0])/delta;
}else{
if(r[2]==max){
H=4+(r[0]-r[1])/delta;
}
}
}
var H=Math.min(Math.round(H*60),360);
if(H<0){
H+=360;
}
}
return ({0:H?H:0,1:S?S:0,2:Math.round((max/255)*100)});
};
draw2d.ColorDialog.prototype.hsv2rgb=function(r){
var R;
var B;
var G;
var S=r[1]/100;
var V=r[2]/100;
var H=r[0]/360;
if(S>0){
if(H>=1){
H=0;
}
H=6*H;
F=H-Math.floor(H);
A=Math.round(255*V*(1-S));
B=Math.round(255*V*(1-(S*F)));
C=Math.round(255*V*(1-(S*(1-F))));
V=Math.round(255*V);
switch(Math.floor(H)){
case 0:
R=V;
G=C;
B=A;
break;
case 1:
R=B;
G=V;
B=A;
break;
case 2:
R=A;
G=V;
B=C;
break;
case 3:
R=A;
G=B;
B=V;
break;
case 4:
R=C;
G=A;
B=V;
break;
case 5:
R=V;
G=A;
B=B;
break;
}
return ({0:R?R:0,1:G?G:0,2:B?B:0});
}else{
return ({0:(V=Math.round(V*255)),1:V,2:V});
}
};
draw2d.LineColorDialog=function(_4cab){
draw2d.ColorDialog.call(this);
this.figure=_4cab;
var color=_4cab.getColor();
this.updateH(this.rgb2hex(color.getRed(),color.getGreen(),color.getBlue()));
};
draw2d.LineColorDialog.prototype=new draw2d.ColorDialog();
draw2d.LineColorDialog.prototype.type="draw2d.LineColorDialog";
draw2d.LineColorDialog.prototype.onOk=function(){
var _4cad=this.workflow;
draw2d.ColorDialog.prototype.onOk.call(this);
if(typeof this.figure.setColor=="function"){
_4cad.getCommandStack().execute(new draw2d.CommandSetColor(this.figure,this.getSelectedColor()));
if(_4cad.getCurrentSelection()==this.figure){
_4cad.setCurrentSelection(this.figure);
}
}
};
draw2d.BackgroundColorDialog=function(_4e47){
draw2d.ColorDialog.call(this);
this.figure=_4e47;
var color=_4e47.getBackgroundColor();
if(color!==null){
this.updateH(this.rgb2hex(color.getRed(),color.getGreen(),color.getBlue()));
}
};
draw2d.BackgroundColorDialog.prototype=new draw2d.ColorDialog();
draw2d.BackgroundColorDialog.prototype.type="draw2d.BackgroundColorDialog";
draw2d.BackgroundColorDialog.prototype.onOk=function(){
var _4e49=this.workflow;
draw2d.ColorDialog.prototype.onOk.call(this);
if(typeof this.figure.setBackgroundColor=="function"){
_4e49.getCommandStack().execute(new draw2d.CommandSetBackgroundColor(this.figure,this.getSelectedColor()));
if(_4e49.getCurrentSelection()==this.figure){
_4e49.setCurrentSelection(this.figure);
}
}
};
draw2d.AnnotationDialog=function(_5b5b){
this.figure=_5b5b;
draw2d.Dialog.call(this);
this.setDimension(400,100);
};
draw2d.AnnotationDialog.prototype=new draw2d.Dialog();
draw2d.AnnotationDialog.prototype.type="draw2d.AnnotationDialog";
draw2d.AnnotationDialog.prototype.createHTMLElement=function(){
var item=draw2d.Dialog.prototype.createHTMLElement.call(this);
var _5b5d=document.createElement("form");
_5b5d.style.position="absolute";
_5b5d.style.left="10px";
_5b5d.style.top="30px";
_5b5d.style.width="375px";
_5b5d.style.font="normal 10px verdana";
item.appendChild(_5b5d);
this.label=document.createTextNode("Text");
_5b5d.appendChild(this.label);
this.input=document.createElement("input");
this.input.style.border="1px solid gray";
this.input.style.font="normal 10px verdana";
this.input.type="text";
var value=this.figure.getText();
if(value){
this.input.value=value;
}else{
this.input.value="";
}
this.input.style.width="100%";
_5b5d.appendChild(this.input);
this.input.focus();
return item;
};
draw2d.AnnotationDialog.prototype.onOk=function(){
this.workflow.getCommandStack().execute(new draw2d.CommandSetText(this.figure,this.input.value));
this.workflow.removeFigure(this);
};
draw2d.Command=function(label){
this.label=label;
};
draw2d.Command.prototype.type="draw2d.Command";
draw2d.Command.prototype.getLabel=function(){
return this.label;
};
draw2d.Command.prototype.canExecute=function(){
return true;
};
draw2d.Command.prototype.execute=function(){
};
draw2d.Command.prototype.cancel=function(){
};
draw2d.Command.prototype.undo=function(){
};
draw2d.Command.prototype.redo=function(){
};
draw2d.CommandStack=function(){
this.undostack=[];
this.redostack=[];
this.maxundo=50;
this.eventListeners=new draw2d.ArrayList();
};
draw2d.CommandStack.PRE_EXECUTE=1;
draw2d.CommandStack.PRE_REDO=2;
draw2d.CommandStack.PRE_UNDO=4;
draw2d.CommandStack.POST_EXECUTE=8;
draw2d.CommandStack.POST_REDO=16;
draw2d.CommandStack.POST_UNDO=32;
draw2d.CommandStack.POST_MASK=draw2d.CommandStack.POST_EXECUTE|draw2d.CommandStack.POST_UNDO|draw2d.CommandStack.POST_REDO;
draw2d.CommandStack.PRE_MASK=draw2d.CommandStack.PRE_EXECUTE|draw2d.CommandStack.PRE_UNDO|draw2d.CommandStack.PRE_REDO;
draw2d.CommandStack.prototype.type="draw2d.CommandStack";
draw2d.CommandStack.prototype.setUndoLimit=function(count){
this.maxundo=count;
};
draw2d.CommandStack.prototype.markSaveLocation=function(){
this.undostack=[];
this.redostack=[];
};
draw2d.CommandStack.prototype.execute=function(_5c10){
if(_5c10===null){
return;
}
if(_5c10===undefined){
throw "Missing parameter [command] for method call CommandStack.prototype.execute";
}
if(_5c10.canExecute()==false){
return;
}
this.notifyListeners(_5c10,draw2d.CommandStack.PRE_EXECUTE);
this.undostack.push(_5c10);
_5c10.execute();
this.redostack=[];
if(this.undostack.length>this.maxundo){
this.undostack=this.undostack.slice(this.undostack.length-this.maxundo);
}
this.notifyListeners(_5c10,draw2d.CommandStack.POST_EXECUTE);
};
draw2d.CommandStack.prototype.undo=function(){
var _5c11=this.undostack.pop();
if(_5c11){
this.notifyListeners(_5c11,draw2d.CommandStack.PRE_UNDO);
this.redostack.push(_5c11);
_5c11.undo();
this.notifyListeners(_5c11,draw2d.CommandStack.POST_UNDO);
}
};
draw2d.CommandStack.prototype.redo=function(){
var _5c12=this.redostack.pop();
if(_5c12){
this.notifyListeners(_5c12,draw2d.CommandStack.PRE_REDO);
this.undostack.push(_5c12);
_5c12.redo();
this.notifyListeners(_5c12,draw2d.CommandStack.POST_REDO);
}
};
draw2d.CommandStack.prototype.getRedoLabel=function(){
if(this.redostack.lenght===0){
return "";
}
var _5c13=this.redostack[this.redostack.length-1];
if(_5c13){
return _5c13.getLabel();
}
return "";
};
draw2d.CommandStack.prototype.getUndoLabel=function(){
if(this.undostack.lenght===0){
return "";
}
var _5c14=this.undostack[this.undostack.length-1];
if(_5c14){
return _5c14.getLabel();
}
return "";
};
draw2d.CommandStack.prototype.canRedo=function(){
return this.redostack.length>0;
};
draw2d.CommandStack.prototype.canUndo=function(){
return this.undostack.length>0;
};
draw2d.CommandStack.prototype.addCommandStackEventListener=function(_5c15){
if(_5c15 instanceof draw2d.CommandStackEventListener){
this.eventListeners.add(_5c15);
}else{
throw "Object doesn't implement required callback interface [draw2d.CommandStackListener]";
}
};
draw2d.CommandStack.prototype.removeCommandStackEventListener=function(_5c16){
this.eventListeners.remove(_5c16);
};
draw2d.CommandStack.prototype.notifyListeners=function(_5c17,state){
var event=new draw2d.CommandStackEvent(_5c17,state);
var size=this.eventListeners.getSize();
for(var i=0;i<size;i++){
this.eventListeners.get(i).stackChanged(event);
}
};
draw2d.CommandStackEvent=function(_5ddb,_5ddc){
this.command=_5ddb;
this.details=_5ddc;
};
draw2d.CommandStackEvent.prototype.type="draw2d.CommandStackEvent";
draw2d.CommandStackEvent.prototype.getCommand=function(){
return this.command;
};
draw2d.CommandStackEvent.prototype.getDetails=function(){
return this.details;
};
draw2d.CommandStackEvent.prototype.isPostChangeEvent=function(){
return 0!=(this.getDetails()&draw2d.CommandStack.POST_MASK);
};
draw2d.CommandStackEvent.prototype.isPreChangeEvent=function(){
return 0!=(this.getDetails()&draw2d.CommandStack.PRE_MASK);
};
draw2d.CommandStackEventListener=function(){
};
draw2d.CommandStackEventListener.prototype.type="draw2d.CommandStackEventListener";
draw2d.CommandStackEventListener.prototype.stackChanged=function(event){
};
draw2d.CommandAdd=function(_5ef7,_5ef8,x,y,_5efb){
draw2d.Command.call(this,"add figure");
if(_5efb===undefined){
_5efb=null;
}
this.parent=_5efb;
this.figure=_5ef8;
this.x=x;
this.y=y;
this.workflow=_5ef7;
};
draw2d.CommandAdd.prototype=new draw2d.Command();
draw2d.CommandAdd.prototype.type="draw2d.CommandAdd";
draw2d.CommandAdd.prototype.execute=function(){
this.redo();
};
draw2d.CommandAdd.prototype.redo=function(){
if(this.x&&this.y){
this.workflow.addFigure(this.figure,this.x,this.y);
}else{
this.workflow.addFigure(this.figure);
}
this.workflow.setCurrentSelection(this.figure);
if(this.parent!==null){
this.parent.addChild(this.figure);
}
};
draw2d.CommandAdd.prototype.undo=function(){
this.workflow.removeFigure(this.figure);
this.workflow.setCurrentSelection(null);
if(this.parent!==null){
this.parent.removeChild(this.figure);
}
};
draw2d.CommandDelete=function(_571e){
draw2d.Command.call(this,"delete figure");
this.parent=_571e.parent;
this.figure=_571e;
this.workflow=_571e.workflow;
this.connections=null;
this.compartmentDeleteCommands=null;
};
draw2d.CommandDelete.prototype=new draw2d.Command();
draw2d.CommandDelete.prototype.type="draw2d.CommandDelete";
draw2d.CommandDelete.prototype.execute=function(){
this.redo();
};
draw2d.CommandDelete.prototype.undo=function(){
if(this.figure instanceof draw2d.CompartmentFigure){
for(var i=0;i<this.compartmentDeleteCommands.getSize();i++){
var _5720=this.compartmentDeleteCommands.get(i);
this.figure.addChild(_5720.figure);
this.workflow.getCommandStack().undo();
}
}
this.workflow.addFigure(this.figure);
if(this.figure instanceof draw2d.Connection){
this.figure.reconnect();
}
this.workflow.setCurrentSelection(this.figure);
if(this.parent!==null){
this.parent.addChild(this.figure);
}
for(var i=0;i<this.connections.getSize();++i){
this.workflow.addFigure(this.connections.get(i));
this.connections.get(i).reconnect();
}
};
draw2d.CommandDelete.prototype.redo=function(){
if(this.figure instanceof draw2d.CompartmentFigure){
if(this.compartmentDeleteCommands===null){
this.compartmentDeleteCommands=new draw2d.ArrayList();
var _5721=this.figure.getChildren().clone();
for(var i=0;i<_5721.getSize();i++){
var child=_5721.get(i);
this.figure.removeChild(child);
var _5724=new draw2d.CommandDelete(child);
this.compartmentDeleteCommands.add(_5724);
this.workflow.getCommandStack().execute(_5724);
}
}else{
for(var i=0;i<this.compartmentDeleteCommands.getSize();i++){
this.workflow.redo();
}
}
}
this.workflow.removeFigure(this.figure);
this.workflow.setCurrentSelection(null);
if(this.figure instanceof draw2d.Node&&this.connections===null){
this.connections=new draw2d.ArrayList();
var ports=this.figure.getPorts();
for(var i=0;i<ports.getSize();i++){
var port=ports.get(i);
for(var c=0,c_size=port.getConnections().getSize();c<c_size;c++){
if(!this.connections.contains(port.getConnections().get(c))){
this.connections.add(port.getConnections().get(c));
}
}
}
}
if(this.connections===null){
this.connections=new draw2d.ArrayList();
}
if(this.parent!==null){
this.parent.removeChild(this.figure);
}
for(var i=0;i<this.connections.getSize();++i){
this.workflow.removeFigure(this.connections.get(i));
}
};
draw2d.CommandMove=function(_545e,x,y){
draw2d.Command.call(this,"move figure");
this.figure=_545e;
if(x==undefined){
this.oldX=_545e.getX();
this.oldY=_545e.getY();
}else{
this.oldX=x;
this.oldY=y;
}
this.oldCompartment=_545e.getParent();
};
draw2d.CommandMove.prototype=new draw2d.Command();
draw2d.CommandMove.prototype.type="draw2d.CommandMove";
draw2d.CommandMove.prototype.setStartPosition=function(x,y){
this.oldX=x;
this.oldY=y;
};
draw2d.CommandMove.prototype.setPosition=function(x,y){
this.newX=x;
this.newY=y;
this.newCompartment=this.figure.workflow.getBestCompartmentFigure(x,y,this.figure);
};
draw2d.CommandMove.prototype.canExecute=function(){
return this.newX!=this.oldX||this.newY!=this.oldY;
};
draw2d.CommandMove.prototype.execute=function(){
this.redo();
};
draw2d.CommandMove.prototype.undo=function(){
this.figure.setPosition(this.oldX,this.oldY);
if(this.newCompartment!==null){
this.newCompartment.removeChild(this.figure);
}
if(this.oldCompartment!==null){
this.oldCompartment.addChild(this.figure);
}
this.figure.workflow.moveResizeHandles(this.figure);
};
draw2d.CommandMove.prototype.redo=function(){
this.figure.setPosition(this.newX,this.newY);
if(this.oldCompartment!==null){
this.oldCompartment.removeChild(this.figure);
}
if(this.newCompartment!==null){
this.newCompartment.addChild(this.figure);
}
this.figure.workflow.moveResizeHandles(this.figure);
};
draw2d.CommandResize=function(_60fc,width,_60fe){
draw2d.Command.call(this,"resize figure");
this.figure=_60fc;
if(width===undefined){
this.oldWidth=_60fc.getWidth();
this.oldHeight=_60fc.getHeight();
}else{
this.oldWidth=width;
this.oldHeight=_60fe;
}
};
draw2d.CommandResize.prototype=new draw2d.Command();
draw2d.CommandResize.prototype.type="draw2d.CommandResize";
draw2d.CommandResize.prototype.setDimension=function(width,_6100){
this.newWidth=width;
this.newHeight=_6100;
};
draw2d.CommandResize.prototype.canExecute=function(){
return this.newWidth!=this.oldWidth||this.newHeight!=this.oldHeight;
};
draw2d.CommandResize.prototype.execute=function(){
this.redo();
};
draw2d.CommandResize.prototype.undo=function(){
this.figure.setDimension(this.oldWidth,this.oldHeight);
this.figure.workflow.moveResizeHandles(this.figure);
};
draw2d.CommandResize.prototype.redo=function(){
this.figure.setDimension(this.newWidth,this.newHeight);
this.figure.workflow.moveResizeHandles(this.figure);
};
draw2d.CommandSetText=function(_510a,text){
draw2d.Command.call(this,"set text");
this.figure=_510a;
this.newText=text;
this.oldText=_510a.getText();
};
draw2d.CommandSetText.prototype=new draw2d.Command();
draw2d.CommandSetText.prototype.type="draw2d.CommandSetText";
draw2d.CommandSetText.prototype.execute=function(){
this.redo();
};
draw2d.CommandSetText.prototype.redo=function(){
this.figure.setText(this.newText);
};
draw2d.CommandSetText.prototype.undo=function(){
this.figure.setText(this.oldText);
};
draw2d.CommandSetColor=function(_610d,color){
draw2d.Command.call(this,"set color");
this.figure=_610d;
this.newColor=color;
this.oldColor=_610d.getColor();
};
draw2d.CommandSetColor.prototype=new draw2d.Command();
draw2d.CommandSetColor.prototype.type="draw2d.CommandSetColor";
draw2d.CommandSetColor.prototype.execute=function(){
this.redo();
};
draw2d.CommandSetColor.prototype.undo=function(){
this.figure.setColor(this.oldColor);
};
draw2d.CommandSetColor.prototype.redo=function(){
this.figure.setColor(this.newColor);
};
draw2d.CommandSetBackgroundColor=function(_4e61,color){
draw2d.Command.call(this,"set background color");
this.figure=_4e61;
this.newColor=color;
this.oldColor=_4e61.getBackgroundColor();
};
draw2d.CommandSetBackgroundColor.prototype=new draw2d.Command();
draw2d.CommandSetBackgroundColor.prototype.type="draw2d.CommandSetBackgroundColor";
draw2d.CommandSetBackgroundColor.prototype.execute=function(){
this.redo();
};
draw2d.CommandSetBackgroundColor.prototype.undo=function(){
this.figure.setBackgroundColor(this.oldColor);
};
draw2d.CommandSetBackgroundColor.prototype.redo=function(){
this.figure.setBackgroundColor(this.newColor);
};
draw2d.CommandConnect=function(_4cae,_4caf,_4cb0){
draw2d.Command.call(this,"create connection");
this.workflow=_4cae;
this.source=_4caf;
this.target=_4cb0;
this.connection=null;
};
draw2d.CommandConnect.prototype=new draw2d.Command();
draw2d.CommandConnect.prototype.type="draw2d.CommandConnect";
draw2d.CommandConnect.prototype.setConnection=function(_4cb1){
this.connection=_4cb1;
};
draw2d.CommandConnect.prototype.execute=function(){
if(this.connection===null){
this.connection=new draw2d.Connection();
}
this.connection.setSource(this.source);
this.connection.setTarget(this.target);
this.workflow.addFigure(this.connection);
};
draw2d.CommandConnect.prototype.redo=function(){
this.workflow.addFigure(this.connection);
this.connection.reconnect();
};
draw2d.CommandConnect.prototype.undo=function(){
this.workflow.removeFigure(this.connection);
};
draw2d.CommandReconnect=function(con){
draw2d.Command.call(this,"reconnect connection");
this.con=con;
this.oldSourcePort=con.getSource();
this.oldTargetPort=con.getTarget();
this.oldRouter=con.getRouter();
this.con.setRouter(new draw2d.NullConnectionRouter());
};
draw2d.CommandReconnect.prototype=new draw2d.Command();
draw2d.CommandReconnect.prototype.type="draw2d.CommandReconnect";
draw2d.CommandReconnect.prototype.canExecute=function(){
return true;
};
draw2d.CommandReconnect.prototype.setNewPorts=function(_5b99,_5b9a){
this.newSourcePort=_5b99;
this.newTargetPort=_5b9a;
};
draw2d.CommandReconnect.prototype.execute=function(){
this.redo();
};
draw2d.CommandReconnect.prototype.cancel=function(){
var start=this.con.sourceAnchor.getLocation(this.con.targetAnchor.getReferencePoint());
var end=this.con.targetAnchor.getLocation(this.con.sourceAnchor.getReferencePoint());
this.con.setStartPoint(start.x,start.y);
this.con.setEndPoint(end.x,end.y);
this.con.getWorkflow().showLineResizeHandles(this.con);
this.con.setRouter(this.oldRouter);
};
draw2d.CommandReconnect.prototype.undo=function(){
this.con.setSource(this.oldSourcePort);
this.con.setTarget(this.oldTargetPort);
this.con.setRouter(this.oldRouter);
if(this.con.getWorkflow().getCurrentSelection()==this.con){
this.con.getWorkflow().showLineResizeHandles(this.con);
}
};
draw2d.CommandReconnect.prototype.redo=function(){
this.con.setSource(this.newSourcePort);
this.con.setTarget(this.newTargetPort);
this.con.setRouter(this.oldRouter);
if(this.con.getWorkflow().getCurrentSelection()==this.con){
this.con.getWorkflow().showLineResizeHandles(this.con);
}
};
draw2d.CommandMoveLine=function(line,_5a17,_5a18,endX,endY){
draw2d.Command.call(this,"move line");
this.line=line;
this.startX1=_5a17;
this.startY1=_5a18;
this.endX1=endX;
this.endY1=endY;
};
draw2d.CommandMoveLine.prototype=new draw2d.Command();
draw2d.CommandMoveLine.prototype.type="draw2d.CommandMoveLine";
draw2d.CommandMoveLine.prototype.canExecute=function(){
return this.startX1!=this.startX2||this.startY1!=this.startY2||this.endX1!=this.endX2||this.endY1!=this.endY2;
};
draw2d.CommandMoveLine.prototype.execute=function(){
this.startX2=this.line.getStartX();
this.startY2=this.line.getStartY();
this.endX2=this.line.getEndX();
this.endY2=this.line.getEndY();
this.redo();
};
draw2d.CommandMoveLine.prototype.undo=function(){
this.line.setStartPoint(this.startX1,this.startY1);
this.line.setEndPoint(this.endX1,this.endY1);
if(this.line.workflow.getCurrentSelection()==this.line){
this.line.workflow.showLineResizeHandles(this.line);
}
};
draw2d.CommandMoveLine.prototype.redo=function(){
this.line.setStartPoint(this.startX2,this.startY2);
this.line.setEndPoint(this.endX2,this.endY2);
if(this.line.workflow.getCurrentSelection()==this.line){
this.line.workflow.showLineResizeHandles(this.line);
}
};
draw2d.CommandMovePort=function(port){
draw2d.Command.call(this,"move port");
this.port=port;
};
draw2d.CommandMovePort.prototype=new draw2d.Command();
draw2d.CommandMovePort.prototype.type="draw2d.CommandMovePort";
draw2d.CommandMovePort.prototype.execute=function(){
this.port.setAlpha(1);
this.port.setPosition(this.port.originX,this.port.originY);
this.port.parentNode.workflow.hideConnectionLine();
};
draw2d.CommandMovePort.prototype.undo=function(){
};
draw2d.CommandMovePort.prototype.redo=function(){
};
draw2d.CommandMovePort.prototype.setPosition=function(x,y){
};
draw2d.Menu=function(){
this.menuItems=new draw2d.ArrayList();
draw2d.Figure.call(this);
this.setSelectable(false);
this.setDeleteable(false);
this.setCanDrag(false);
this.setResizeable(false);
this.setSelectable(false);
this.setZOrder(10000);
this.dirty=false;
};
draw2d.Menu.prototype=new draw2d.Figure();
draw2d.Menu.prototype.type="draw2d.Menu";
draw2d.Menu.prototype.createHTMLElement=function(){
var item=document.createElement("div");
item.style.position="absolute";
item.style.left=this.x+"px";
item.style.top=this.y+"px";
item.style.margin="0px";
item.style.padding="0px";
item.style.zIndex=""+draw2d.Figure.ZOrderBaseIndex;
item.style.border="1px solid gray";
item.style.background="lavender";
item.style.cursor="pointer";
item.style.width="auto";
item.style.height="auto";
item.className="Menu";
return item;
};
draw2d.Menu.prototype.setWorkflow=function(_4dd2){
this.workflow=_4dd2;
};
draw2d.Menu.prototype.setDimension=function(w,h){
};
draw2d.Menu.prototype.appendMenuItem=function(item){
this.menuItems.add(item);
item.parentMenu=this;
this.dirty=true;
};
draw2d.Menu.prototype.getHTMLElement=function(){
var html=draw2d.Figure.prototype.getHTMLElement.call(this);
if(this.dirty){
this.createList();
}
return html;
};
draw2d.Menu.prototype.createList=function(){
this.dirty=false;
this.html.innerHTML="";
var oThis=this;
for(var i=0;i<this.menuItems.getSize();i++){
var item=this.menuItems.get(i);
var li=document.createElement("a");
li.innerHTML=item.getLabel();
li.style.display="block";
li.style.fontFamily="Verdana, Arial, Helvetica, sans-serif";
li.style.fontSize="9pt";
li.style.color="dimgray";
li.style.borderBottom="1px solid silver";
li.style.paddingLeft="5px";
li.style.paddingRight="5px";
li.style.whiteSpace="nowrap";
li.style.cursor="pointer";
li.className="MenuItem";
this.html.appendChild(li);
li.menuItem=item;
if(li.addEventListener){
li.addEventListener("click",function(event){
var _4ddc=arguments[0]||window.event;
_4ddc.cancelBubble=true;
_4ddc.returnValue=false;
var diffX=_4ddc.clientX;
var diffY=_4ddc.clientY;
var _4ddf=document.body.parentNode.scrollLeft;
var _4de0=document.body.parentNode.scrollTop;
this.menuItem.execute(diffX+_4ddf,diffY+_4de0);
},false);
li.addEventListener("mouseup",function(event){
event.cancelBubble=true;
event.returnValue=false;
},false);
li.addEventListener("mousedown",function(event){
event.cancelBubble=true;
event.returnValue=false;
},false);
li.addEventListener("mouseover",function(event){
this.style.backgroundColor="silver";
},false);
li.addEventListener("mouseout",function(event){
this.style.backgroundColor="transparent";
},false);
}else{
if(li.attachEvent){
li.attachEvent("onclick",function(event){
var _4de6=arguments[0]||window.event;
_4de6.cancelBubble=true;
_4de6.returnValue=false;
var diffX=_4de6.clientX;
var diffY=_4de6.clientY;
var _4de9=document.body.parentNode.scrollLeft;
var _4dea=document.body.parentNode.scrollTop;
event.srcElement.menuItem.execute(diffX+_4de9,diffY+_4dea);
});
li.attachEvent("onmousedown",function(event){
event.cancelBubble=true;
event.returnValue=false;
});
li.attachEvent("onmouseup",function(event){
event.cancelBubble=true;
event.returnValue=false;
});
li.attachEvent("onmouseover",function(event){
event.srcElement.style.backgroundColor="silver";
});
li.attachEvent("onmouseout",function(event){
event.srcElement.style.backgroundColor="transparent";
});
}
}
}
};
draw2d.MenuItem=function(label,_544d,_544e){
this.label=label;
this.iconUrl=_544d;
this.parentMenu=null;
this.action=_544e;
};
draw2d.MenuItem.prototype.type="draw2d.MenuItem";
draw2d.MenuItem.prototype.isEnabled=function(){
return true;
};
draw2d.MenuItem.prototype.getLabel=function(){
return this.label;
};
draw2d.MenuItem.prototype.execute=function(x,y){
this.parentMenu.workflow.showMenu(null);
this.action(x,y);
};
draw2d.Locator=function(){
};
draw2d.Locator.prototype.type="draw2d.Locator";
draw2d.Locator.prototype.relocate=function(_4cff){
};
draw2d.ConnectionLocator=function(_517f){
draw2d.Locator.call(this);
this.connection=_517f;
};
draw2d.ConnectionLocator.prototype=new draw2d.Locator;
draw2d.ConnectionLocator.prototype.type="draw2d.ConnectionLocator";
draw2d.ConnectionLocator.prototype.getConnection=function(){
return this.connection;
};
draw2d.ManhattanMidpointLocator=function(_5b9f){
draw2d.ConnectionLocator.call(this,_5b9f);
};
draw2d.ManhattanMidpointLocator.prototype=new draw2d.ConnectionLocator;
draw2d.ManhattanMidpointLocator.prototype.type="draw2d.ManhattanMidpointLocator";
draw2d.ManhattanMidpointLocator.prototype.relocate=function(_5ba0){
var conn=this.getConnection();
var p=new draw2d.Point();
var _5ba3=conn.getPoints();
var index=Math.floor((_5ba3.getSize()-2)/2);
if(_5ba3.getSize()<=index+1){
return;
}
var p1=_5ba3.get(index);
var p2=_5ba3.get(index+1);
p.x=(p2.x-p1.x)/2+p1.x+5;
p.y=(p2.y-p1.y)/2+p1.y+5;
_5ba0.setPosition(p.x,p.y);
};
draw2d.BezierMidpointLocator=function(_4c6a){
draw2d.ConnectionLocator.call(this,_4c6a);
};
draw2d.BezierMidpointLocator.prototype=new draw2d.ConnectionLocator;
draw2d.BezierMidpointLocator.prototype.type="draw2d.BezierMidpointLocator";
draw2d.BezierMidpointLocator.prototype.relocate=function(_4c6b){
var conn=this.getConnection();
var p=new draw2d.Point();
var _4c6e=conn.getPoints();
var index=Math.floor((_4c6e.getSize()-2)/2);
if(_4c6e.getSize()<=index+1){
return;
}
var p1=_4c6e.get(index);
var p2=_4c6e.get(index+1);
p.x=(p2.x-p1.x)/2+p1.x+5;
p.y=(p2.y-p1.y)/2+p1.y+5;
_4c6b.setPosition(p.x,p.y);
};
draw2d.EditPartFactory=function(){
};
draw2d.EditPartFactory.prototype.type="draw2d.EditPartFactory";
draw2d.EditPartFactory.prototype.createEditPart=function(model){
};
draw2d.AbstractObjectModel=function(){
this.listeners=new draw2d.ArrayList();
this.id=draw2d.UUID.create();
};
draw2d.AbstractObjectModel.EVENT_ELEMENT_ADDED="element added";
draw2d.AbstractObjectModel.EVENT_ELEMENT_REMOVED="element removed";
draw2d.AbstractObjectModel.EVENT_CONNECTION_ADDED="connection addedx";
draw2d.AbstractObjectModel.EVENT_CONNECTION_REMOVED="connection removed";
draw2d.AbstractObjectModel.prototype.type="draw2d.AbstractObjectModel";
draw2d.AbstractObjectModel.prototype.getModelChildren=function(){
return new draw2d.ArrayList();
};
draw2d.AbstractObjectModel.prototype.getModelParent=function(){
return this.modelParent;
};
draw2d.AbstractObjectModel.prototype.setModelParent=function(_5728){
this.modelParent=_5728;
};
draw2d.AbstractObjectModel.prototype.getId=function(){
return this.id;
};
draw2d.AbstractObjectModel.prototype.firePropertyChange=function(_5729,_572a,_572b){
var count=this.listeners.getSize();
if(count===0){
return;
}
var event=new draw2d.PropertyChangeEvent(this,_5729,_572a,_572b);
for(var i=0;i<count;i++){
try{
this.listeners.get(i).propertyChange(event);
}
catch(e){
alert("Method: draw2d.AbstractObjectModel.prototype.firePropertyChange\n"+e+"\nProperty: "+_5729+"\nListener Class:"+this.listeners.get(i).type);
}
}
};
draw2d.AbstractObjectModel.prototype.addPropertyChangeListener=function(_572f){
if(_572f!==null){
this.listeners.add(_572f);
}
};
draw2d.AbstractObjectModel.prototype.removePropertyChangeListener=function(_5730){
if(_5730!==null){
this.listeners.remove(_5730);
}
};
draw2d.AbstractObjectModel.prototype.getPersistentAttributes=function(){
return {id:this.id};
};
draw2d.AbstractConnectionModel=function(){
draw2d.AbstractObjectModel.call(this);
};
draw2d.AbstractConnectionModel.prototype=new draw2d.AbstractObjectModel();
draw2d.AbstractConnectionModel.prototype.type="draw2d.AbstractConnectionModel";
draw2d.AbstractConnectionModel.prototype.getSourceModel=function(){
throw "you must override the method [AbstractConnectionModel.prototype.getSourceModel]";
};
draw2d.AbstractConnectionModel.prototype.getTargetModel=function(){
throw "you must override the method [AbstractConnectionModel.prototype.getTargetModel]";
};
draw2d.AbstractConnectionModel.prototype.getSourcePortName=function(){
throw "you must override the method [AbstractConnectionModel.prototype.getSourcePortName]";
};
draw2d.AbstractConnectionModel.prototype.getTargetPortName=function(){
throw "you must override the method [AbstractConnectionModel.prototype.getTargetPortName]";
};
draw2d.AbstractConnectionModel.prototype.getSourcePortModel=function(){
throw "you must override the method [AbstractConnectionModel.prototype.getSourcePortModel]";
};
draw2d.AbstractConnectionModel.prototype.getTargetPortModel=function(){
throw "you must override the method [AbstractConnectionModel.prototype.getTargetPortModel]";
};
draw2d.PropertyChangeEvent=function(model,_5468,_5469,_546a){
this.model=model;
this.property=_5468;
this.oldValue=_5469;
this.newValue=_546a;
};
draw2d.PropertyChangeEvent.prototype.type="draw2d.PropertyChangeEvent";
draw2d.GraphicalViewer=function(id){
try{
draw2d.Workflow.call(this,id);
this.factory=null;
this.model=null;
this.initDone=false;
}
catch(e){
pushErrorStack(e,"draw2d.GraphicalViewer=function(/*:String*/ id)");
}
};
draw2d.GraphicalViewer.prototype=new draw2d.Workflow();
draw2d.GraphicalViewer.prototype.type="draw2d.GraphicalViewer";
draw2d.GraphicalViewer.prototype.setEditPartFactory=function(_5b7c){
this.factory=_5b7c;
this.checkInit();
};
draw2d.GraphicalViewer.prototype.setModel=function(model){
try{
if(model instanceof draw2d.AbstractObjectModel){
this.model=model;
this.checkInit();
this.model.addPropertyChangeListener(this);
}else{
alert("Invalid model class type:"+model.type);
}
}
catch(e){
pushErrorStack(e,"draw2d.GraphicalViewer.prototype.setModel=function(/*:draw2d.AbstractObjectModel*/ model )");
}
};
draw2d.GraphicalViewer.prototype.propertyChange=function(event){
switch(event.property){
case draw2d.AbstractObjectModel.EVENT_ELEMENT_REMOVED:
var _5b7f=this.getFigure(event.oldValue.getId());
this.removeFigure(_5b7f);
break;
case draw2d.AbstractObjectModel.EVENT_ELEMENT_ADDED:
var _5b7f=this.factory.createEditPart(event.newValue);
_5b7f.setId(event.newValue.getId());
this.addFigure(_5b7f);
this.setCurrentSelection(_5b7f);
break;
}
};
draw2d.GraphicalViewer.prototype.checkInit=function(){
if(this.factory!==null&&this.model!==null&&this.initDone==false){
try{
var _5b80=this.model.getModelChildren();
var count=_5b80.getSize();
for(var i=0;i<count;i++){
var child=_5b80.get(i);
var _5b84=this.factory.createEditPart(child);
_5b84.setId(child.getId());
this.addFigure(_5b84);
}
}
catch(e){
pushErrorStack(e,"draw2d.GraphicalViewer.prototype.checkInit=function()[addFigures]");
}
try{
var _5b85=this.getDocument().getFigures();
var count=_5b85.getSize();
for(var i=0;i<count;i++){
var _5b84=_5b85.get(i);
if(_5b84 instanceof draw2d.Node){
this.refreshConnections(_5b84);
}
}
}
catch(e){
pushErrorStack(e,"draw2d.GraphicalViewer.prototype.checkInit=function()[refreshConnections]");
}
}
};
draw2d.GraphicalViewer.prototype.refreshConnections=function(node){
try{
var _5b87=new draw2d.ArrayList();
var _5b88=node.getModelSourceConnections();
var count=_5b88.getSize();
for(var i=0;i<count;i++){
var _5b8b=_5b88.get(i);
_5b87.add(_5b8b.getId());
var _5b8c=this.getLine(_5b8b.getId());
if(_5b8c===null){
_5b8c=this.factory.createEditPart(_5b8b);
var _5b8d=_5b8b.getSourceModel();
var _5b8e=_5b8b.getTargetModel();
var _5b8f=this.getFigure(_5b8d.getId());
var _5b90=this.getFigure(_5b8e.getId());
var _5b91=_5b8f.getOutputPort(_5b8b.getSourcePortName());
var _5b92=_5b90.getInputPort(_5b8b.getTargetPortName());
_5b8c.setTarget(_5b92);
_5b8c.setSource(_5b91);
_5b8c.setId(_5b8b.getId());
this.addFigure(_5b8c);
this.setCurrentSelection(_5b8c);
}
}
var ports=node.getOutputPorts();
count=ports.getSize();
for(var i=0;i<count;i++){
var _5b94=ports.get(i).getConnections();
var _5b95=_5b94.getSize();
for(var ii=0;ii<_5b95;ii++){
var _5b97=_5b94.get(ii);
if(!_5b87.contains(_5b97.getId())){
this.removeFigure(_5b97);
_5b87.add(_5b97.getId());
}
}
}
}
catch(e){
pushErrorStack(e,"draw2d.GraphicalViewer.prototype.refreshConnections=function(/*:draw2d.Node*/ node )");
}
};
draw2d.GraphicalEditor=function(id){
try{
this.view=new draw2d.GraphicalViewer(id);
this.initializeGraphicalViewer();
}
catch(e){
pushErrorStack(e,"draw2d.GraphicalEditor=function(/*:String*/ id)");
}
};
draw2d.GraphicalEditor.prototype.type="draw2d.GraphicalEditor";
draw2d.GraphicalEditor.prototype.initializeGraphicalViewer=function(){
};
draw2d.GraphicalEditor.prototype.getGraphicalViewer=function(){
return this.view;
};
draw2d.GraphicalEditor.prototype.executeCommand=function(_4caa){
this.view.getCommandStack().execute(_4caa);
};
var whitespace="\n\r\t ";
XMLP=function(_5593){
_5593=SAXStrings.replace(_5593,null,null,"\r\n","\n");
_5593=SAXStrings.replace(_5593,null,null,"\r","\n");
this.m_xml=_5593;
this.m_iP=0;
this.m_iState=XMLP._STATE_PROLOG;
this.m_stack=new Stack();
this._clearAttributes();
};
XMLP._NONE=0;
XMLP._ELM_B=1;
XMLP._ELM_E=2;
XMLP._ELM_EMP=3;
XMLP._ATT=4;
XMLP._TEXT=5;
XMLP._ENTITY=6;
XMLP._PI=7;
XMLP._CDATA=8;
XMLP._COMMENT=9;
XMLP._DTD=10;
XMLP._ERROR=11;
XMLP._CONT_XML=0;
XMLP._CONT_ALT=1;
XMLP._ATT_NAME=0;
XMLP._ATT_VAL=1;
XMLP._STATE_PROLOG=1;
XMLP._STATE_DOCUMENT=2;
XMLP._STATE_MISC=3;
XMLP._errs=[];
XMLP._errs[XMLP.ERR_CLOSE_PI=0]="PI: missing closing sequence";
XMLP._errs[XMLP.ERR_CLOSE_DTD=1]="DTD: missing closing sequence";
XMLP._errs[XMLP.ERR_CLOSE_COMMENT=2]="Comment: missing closing sequence";
XMLP._errs[XMLP.ERR_CLOSE_CDATA=3]="CDATA: missing closing sequence";
XMLP._errs[XMLP.ERR_CLOSE_ELM=4]="Element: missing closing sequence";
XMLP._errs[XMLP.ERR_CLOSE_ENTITY=5]="Entity: missing closing sequence";
XMLP._errs[XMLP.ERR_PI_TARGET=6]="PI: target is required";
XMLP._errs[XMLP.ERR_ELM_EMPTY=7]="Element: cannot be both empty and closing";
XMLP._errs[XMLP.ERR_ELM_NAME=8]="Element: name must immediatly follow \"<\"";
XMLP._errs[XMLP.ERR_ELM_LT_NAME=9]="Element: \"<\" not allowed in element names";
XMLP._errs[XMLP.ERR_ATT_VALUES=10]="Attribute: values are required and must be in quotes";
XMLP._errs[XMLP.ERR_ATT_LT_NAME=11]="Element: \"<\" not allowed in attribute names";
XMLP._errs[XMLP.ERR_ATT_LT_VALUE=12]="Attribute: \"<\" not allowed in attribute values";
XMLP._errs[XMLP.ERR_ATT_DUP=13]="Attribute: duplicate attributes not allowed";
XMLP._errs[XMLP.ERR_ENTITY_UNKNOWN=14]="Entity: unknown entity";
XMLP._errs[XMLP.ERR_INFINITELOOP=15]="Infininte loop";
XMLP._errs[XMLP.ERR_DOC_STRUCTURE=16]="Document: only comments, processing instructions, or whitespace allowed outside of document element";
XMLP._errs[XMLP.ERR_ELM_NESTING=17]="Element: must be nested correctly";
XMLP.prototype._addAttribute=function(name,value){
this.m_atts[this.m_atts.length]=new Array(name,value);
};
XMLP.prototype._checkStructure=function(_5596){
if(XMLP._STATE_PROLOG==this.m_iState){
if((XMLP._TEXT==_5596)||(XMLP._ENTITY==_5596)){
if(SAXStrings.indexOfNonWhitespace(this.getContent(),this.getContentBegin(),this.getContentEnd())!=-1){
return this._setErr(XMLP.ERR_DOC_STRUCTURE);
}
}
if((XMLP._ELM_B==_5596)||(XMLP._ELM_EMP==_5596)){
this.m_iState=XMLP._STATE_DOCUMENT;
}
}
if(XMLP._STATE_DOCUMENT==this.m_iState){
if((XMLP._ELM_B==_5596)||(XMLP._ELM_EMP==_5596)){
this.m_stack.push(this.getName());
}
if((XMLP._ELM_E==_5596)||(XMLP._ELM_EMP==_5596)){
var _5597=this.m_stack.pop();
if((_5597===null)||(_5597!=this.getName())){
return this._setErr(XMLP.ERR_ELM_NESTING);
}
}
if(this.m_stack.count()===0){
this.m_iState=XMLP._STATE_MISC;
return _5596;
}
}
if(XMLP._STATE_MISC==this.m_iState){
if((XMLP._ELM_B==_5596)||(XMLP._ELM_E==_5596)||(XMLP._ELM_EMP==_5596)||(XMLP.EVT_DTD==_5596)){
return this._setErr(XMLP.ERR_DOC_STRUCTURE);
}
if((XMLP._TEXT==_5596)||(XMLP._ENTITY==_5596)){
if(SAXStrings.indexOfNonWhitespace(this.getContent(),this.getContentBegin(),this.getContentEnd())!=-1){
return this._setErr(XMLP.ERR_DOC_STRUCTURE);
}
}
}
return _5596;
};
XMLP.prototype._clearAttributes=function(){
this.m_atts=[];
};
XMLP.prototype._findAttributeIndex=function(name){
for(var i=0;i<this.m_atts.length;i++){
if(this.m_atts[i][XMLP._ATT_NAME]==name){
return i;
}
}
return -1;
};
XMLP.prototype.getAttributeCount=function(){
return this.m_atts?this.m_atts.length:0;
};
XMLP.prototype.getAttributeName=function(index){
return ((index<0)||(index>=this.m_atts.length))?null:this.m_atts[index][XMLP._ATT_NAME];
};
XMLP.prototype.getAttributeValue=function(index){
return ((index<0)||(index>=this.m_atts.length))?null:__unescapeString(this.m_atts[index][XMLP._ATT_VAL]);
};
XMLP.prototype.getAttributeValueByName=function(name){
return this.getAttributeValue(this._findAttributeIndex(name));
};
XMLP.prototype.getColumnNumber=function(){
return SAXStrings.getColumnNumber(this.m_xml,this.m_iP);
};
XMLP.prototype.getContent=function(){
return (this.m_cSrc==XMLP._CONT_XML)?this.m_xml:this.m_cAlt;
};
XMLP.prototype.getContentBegin=function(){
return this.m_cB;
};
XMLP.prototype.getContentEnd=function(){
return this.m_cE;
};
XMLP.prototype.getLineNumber=function(){
return SAXStrings.getLineNumber(this.m_xml,this.m_iP);
};
XMLP.prototype.getName=function(){
return this.m_name;
};
XMLP.prototype.next=function(){
return this._checkStructure(this._parse());
};
XMLP.prototype._parse=function(){
if(this.m_iP==this.m_xml.length){
return XMLP._NONE;
}
if(this.m_iP==this.m_xml.indexOf("<?",this.m_iP)){
return this._parsePI(this.m_iP+2);
}else{
if(this.m_iP==this.m_xml.indexOf("<!DOCTYPE",this.m_iP)){
return this._parseDTD(this.m_iP+9);
}else{
if(this.m_iP==this.m_xml.indexOf("<!--",this.m_iP)){
return this._parseComment(this.m_iP+4);
}else{
if(this.m_iP==this.m_xml.indexOf("<![CDATA[",this.m_iP)){
return this._parseCDATA(this.m_iP+9);
}else{
if(this.m_iP==this.m_xml.indexOf("<",this.m_iP)){
return this._parseElement(this.m_iP+1);
}else{
if(this.m_iP==this.m_xml.indexOf("&",this.m_iP)){
return this._parseEntity(this.m_iP+1);
}else{
return this._parseText(this.m_iP);
}
}
}
}
}
}
};
XMLP.prototype._parseAttribute=function(iB,iE){
var iNB,iNE,iEq,iVB,iVE;
var _55a0,strN,strV;
this.m_cAlt="";
iNB=SAXStrings.indexOfNonWhitespace(this.m_xml,iB,iE);
if((iNB==-1)||(iNB>=iE)){
return iNB;
}
iEq=this.m_xml.indexOf("=",iNB);
if((iEq==-1)||(iEq>iE)){
return this._setErr(XMLP.ERR_ATT_VALUES);
}
iNE=SAXStrings.lastIndexOfNonWhitespace(this.m_xml,iNB,iEq);
iVB=SAXStrings.indexOfNonWhitespace(this.m_xml,iEq+1,iE);
if((iVB==-1)||(iVB>iE)){
return this._setErr(XMLP.ERR_ATT_VALUES);
}
_55a0=this.m_xml.charAt(iVB);
if(SAXStrings.QUOTES.indexOf(_55a0)==-1){
return this._setErr(XMLP.ERR_ATT_VALUES);
}
iVE=this.m_xml.indexOf(_55a0,iVB+1);
if((iVE==-1)||(iVE>iE)){
return this._setErr(XMLP.ERR_ATT_VALUES);
}
strN=this.m_xml.substring(iNB,iNE+1);
strV=this.m_xml.substring(iVB+1,iVE);
if(strN.indexOf("<")!=-1){
return this._setErr(XMLP.ERR_ATT_LT_NAME);
}
if(strV.indexOf("<")!=-1){
return this._setErr(XMLP.ERR_ATT_LT_VALUE);
}
strV=SAXStrings.replace(strV,null,null,"\n"," ");
strV=SAXStrings.replace(strV,null,null,"\t"," ");
iRet=this._replaceEntities(strV);
if(iRet==XMLP._ERROR){
return iRet;
}
strV=this.m_cAlt;
if(this._findAttributeIndex(strN)==-1){
this._addAttribute(strN,strV);
}else{
return this._setErr(XMLP.ERR_ATT_DUP);
}
this.m_iP=iVE+2;
return XMLP._ATT;
};
XMLP.prototype._parseCDATA=function(iB){
var iE=this.m_xml.indexOf("]]>",iB);
if(iE==-1){
return this._setErr(XMLP.ERR_CLOSE_CDATA);
}
this._setContent(XMLP._CONT_XML,iB,iE);
this.m_iP=iE+3;
return XMLP._CDATA;
};
XMLP.prototype._parseComment=function(iB){
var iE=this.m_xml.indexOf("-"+"->",iB);
if(iE==-1){
return this._setErr(XMLP.ERR_CLOSE_COMMENT);
}
this._setContent(XMLP._CONT_XML,iB,iE);
this.m_iP=iE+3;
return XMLP._COMMENT;
};
XMLP.prototype._parseDTD=function(iB){
var iE,strClose,iInt,iLast;
iE=this.m_xml.indexOf(">",iB);
if(iE==-1){
return this._setErr(XMLP.ERR_CLOSE_DTD);
}
iInt=this.m_xml.indexOf("[",iB);
strClose=((iInt!=-1)&&(iInt<iE))?"]>":">";
while(true){
if(iE==iLast){
return this._setErr(XMLP.ERR_INFINITELOOP);
}
iLast=iE;
iE=this.m_xml.indexOf(strClose,iB);
if(iE==-1){
return this._setErr(XMLP.ERR_CLOSE_DTD);
}
if(this.m_xml.substring(iE-1,iE+2)!="]]>"){
break;
}
}
this.m_iP=iE+strClose.length;
return XMLP._DTD;
};
XMLP.prototype._parseElement=function(iB){
var iE,iDE,iNE,iRet;
var iType,strN,iLast;
iDE=iE=this.m_xml.indexOf(">",iB);
if(iE==-1){
return this._setErr(XMLP.ERR_CLOSE_ELM);
}
if(this.m_xml.charAt(iB)=="/"){
iType=XMLP._ELM_E;
iB++;
}else{
iType=XMLP._ELM_B;
}
if(this.m_xml.charAt(iE-1)=="/"){
if(iType==XMLP._ELM_E){
return this._setErr(XMLP.ERR_ELM_EMPTY);
}
iType=XMLP._ELM_EMP;
iDE--;
}
iDE=SAXStrings.lastIndexOfNonWhitespace(this.m_xml,iB,iDE);
if(iE-iB!=1){
if(SAXStrings.indexOfNonWhitespace(this.m_xml,iB,iDE)!=iB){
return this._setErr(XMLP.ERR_ELM_NAME);
}
}
this._clearAttributes();
iNE=SAXStrings.indexOfWhitespace(this.m_xml,iB,iDE);
if(iNE==-1){
iNE=iDE+1;
}else{
this.m_iP=iNE;
while(this.m_iP<iDE){
if(this.m_iP==iLast){
return this._setErr(XMLP.ERR_INFINITELOOP);
}
iLast=this.m_iP;
iRet=this._parseAttribute(this.m_iP,iDE);
if(iRet==XMLP._ERROR){
return iRet;
}
}
}
strN=this.m_xml.substring(iB,iNE);
if(strN.indexOf("<")!=-1){
return this._setErr(XMLP.ERR_ELM_LT_NAME);
}
this.m_name=strN;
this.m_iP=iE+1;
return iType;
};
XMLP.prototype._parseEntity=function(iB){
var iE=this.m_xml.indexOf(";",iB);
if(iE==-1){
return this._setErr(XMLP.ERR_CLOSE_ENTITY);
}
this.m_iP=iE+1;
return this._replaceEntity(this.m_xml,iB,iE);
};
XMLP.prototype._parsePI=function(iB){
var iE,iTB,iTE,iCB,iCE;
iE=this.m_xml.indexOf("?>",iB);
if(iE==-1){
return this._setErr(XMLP.ERR_CLOSE_PI);
}
iTB=SAXStrings.indexOfNonWhitespace(this.m_xml,iB,iE);
if(iTB==-1){
return this._setErr(XMLP.ERR_PI_TARGET);
}
iTE=SAXStrings.indexOfWhitespace(this.m_xml,iTB,iE);
if(iTE==-1){
iTE=iE;
}
iCB=SAXStrings.indexOfNonWhitespace(this.m_xml,iTE,iE);
if(iCB==-1){
iCB=iE;
}
iCE=SAXStrings.lastIndexOfNonWhitespace(this.m_xml,iCB,iE);
if(iCE==-1){
iCE=iE-1;
}
this.m_name=this.m_xml.substring(iTB,iTE);
this._setContent(XMLP._CONT_XML,iCB,iCE+1);
this.m_iP=iE+2;
return XMLP._PI;
};
XMLP.prototype._parseText=function(iB){
var iE,iEE;
iE=this.m_xml.indexOf("<",iB);
if(iE==-1){
iE=this.m_xml.length;
}
iEE=this.m_xml.indexOf("&",iB);
if((iEE!=-1)&&(iEE<=iE)){
iE=iEE;
}
this._setContent(XMLP._CONT_XML,iB,iE);
this.m_iP=iE;
return XMLP._TEXT;
};
XMLP.prototype._replaceEntities=function(strD,iB,iE){
if(SAXStrings.isEmpty(strD)){
return "";
}
iB=iB||0;
iE=iE||strD.length;
var iEB,iEE,strRet="";
iEB=strD.indexOf("&",iB);
iEE=iB;
while((iEB>0)&&(iEB<iE)){
strRet+=strD.substring(iEE,iEB);
iEE=strD.indexOf(";",iEB)+1;
if((iEE===0)||(iEE>iE)){
return this._setErr(XMLP.ERR_CLOSE_ENTITY);
}
iRet=this._replaceEntity(strD,iEB+1,iEE-1);
if(iRet==XMLP._ERROR){
return iRet;
}
strRet+=this.m_cAlt;
iEB=strD.indexOf("&",iEE);
}
if(iEE!=iE){
strRet+=strD.substring(iEE,iE);
}
this._setContent(XMLP._CONT_ALT,strRet);
return XMLP._ENTITY;
};
XMLP.prototype._replaceEntity=function(strD,iB,iE){
if(SAXStrings.isEmpty(strD)){
return -1;
}
iB=iB||0;
iE=iE||strD.length;
switch(strD.substring(iB,iE)){
case "amp":
strEnt="&";
break;
case "lt":
strEnt="<";
break;
case "gt":
strEnt=">";
break;
case "apos":
strEnt="'";
break;
case "quot":
strEnt="\"";
break;
default:
if(strD.charAt(iB)=="#"){
strEnt=String.fromCharCode(parseInt(strD.substring(iB+1,iE)));
}else{
return this._setErr(XMLP.ERR_ENTITY_UNKNOWN);
}
break;
}
this._setContent(XMLP._CONT_ALT,strEnt);
return XMLP._ENTITY;
};
XMLP.prototype._setContent=function(iSrc){
var args=arguments;
if(XMLP._CONT_XML==iSrc){
this.m_cAlt=null;
this.m_cB=args[1];
this.m_cE=args[2];
}else{
this.m_cAlt=args[1];
this.m_cB=0;
this.m_cE=args[1].length;
}
this.m_cSrc=iSrc;
};
XMLP.prototype._setErr=function(iErr){
var _55ba=XMLP._errs[iErr];
this.m_cAlt=_55ba;
this.m_cB=0;
this.m_cE=_55ba.length;
this.m_cSrc=XMLP._CONT_ALT;
return XMLP._ERROR;
};
SAXDriver=function(){
this.m_hndDoc=null;
this.m_hndErr=null;
this.m_hndLex=null;
};
SAXDriver.DOC_B=1;
SAXDriver.DOC_E=2;
SAXDriver.ELM_B=3;
SAXDriver.ELM_E=4;
SAXDriver.CHARS=5;
SAXDriver.PI=6;
SAXDriver.CD_B=7;
SAXDriver.CD_E=8;
SAXDriver.CMNT=9;
SAXDriver.DTD_B=10;
SAXDriver.DTD_E=11;
SAXDriver.prototype.parse=function(strD){
var _55bc=new XMLP(strD);
if(this.m_hndDoc&&this.m_hndDoc.setDocumentLocator){
this.m_hndDoc.setDocumentLocator(this);
}
this.m_parser=_55bc;
this.m_bErr=false;
if(!this.m_bErr){
this._fireEvent(SAXDriver.DOC_B);
}
this._parseLoop();
if(!this.m_bErr){
this._fireEvent(SAXDriver.DOC_E);
}
this.m_xml=null;
this.m_iP=0;
};
SAXDriver.prototype.setDocumentHandler=function(hnd){
this.m_hndDoc=hnd;
};
SAXDriver.prototype.setErrorHandler=function(hnd){
this.m_hndErr=hnd;
};
SAXDriver.prototype.setLexicalHandler=function(hnd){
this.m_hndLex=hnd;
};
SAXDriver.prototype.getColumnNumber=function(){
return this.m_parser.getColumnNumber();
};
SAXDriver.prototype.getLineNumber=function(){
return this.m_parser.getLineNumber();
};
SAXDriver.prototype.getMessage=function(){
return this.m_strErrMsg;
};
SAXDriver.prototype.getPublicId=function(){
return null;
};
SAXDriver.prototype.getSystemId=function(){
return null;
};
SAXDriver.prototype.getLength=function(){
return this.m_parser.getAttributeCount();
};
SAXDriver.prototype.getName=function(index){
return this.m_parser.getAttributeName(index);
};
SAXDriver.prototype.getValue=function(index){
return this.m_parser.getAttributeValue(index);
};
SAXDriver.prototype.getValueByName=function(name){
return this.m_parser.getAttributeValueByName(name);
};
SAXDriver.prototype._fireError=function(_55c3){
this.m_strErrMsg=_55c3;
this.m_bErr=true;
if(this.m_hndErr&&this.m_hndErr.fatalError){
this.m_hndErr.fatalError(this);
}
};
SAXDriver.prototype._fireEvent=function(iEvt){
var hnd,func,args=arguments,iLen=args.length-1;
if(this.m_bErr){
return;
}
if(SAXDriver.DOC_B==iEvt){
func="startDocument";
hnd=this.m_hndDoc;
}else{
if(SAXDriver.DOC_E==iEvt){
func="endDocument";
hnd=this.m_hndDoc;
}else{
if(SAXDriver.ELM_B==iEvt){
func="startElement";
hnd=this.m_hndDoc;
}else{
if(SAXDriver.ELM_E==iEvt){
func="endElement";
hnd=this.m_hndDoc;
}else{
if(SAXDriver.CHARS==iEvt){
func="characters";
hnd=this.m_hndDoc;
}else{
if(SAXDriver.PI==iEvt){
func="processingInstruction";
hnd=this.m_hndDoc;
}else{
if(SAXDriver.CD_B==iEvt){
func="startCDATA";
hnd=this.m_hndLex;
}else{
if(SAXDriver.CD_E==iEvt){
func="endCDATA";
hnd=this.m_hndLex;
}else{
if(SAXDriver.CMNT==iEvt){
func="comment";
hnd=this.m_hndLex;
}
}
}
}
}
}
}
}
}
if(hnd&&hnd[func]){
if(0==iLen){
hnd[func]();
}else{
if(1==iLen){
hnd[func](args[1]);
}else{
if(2==iLen){
hnd[func](args[1],args[2]);
}else{
if(3==iLen){
hnd[func](args[1],args[2],args[3]);
}
}
}
}
}
};
SAXDriver.prototype._parseLoop=function(_55c6){
var _55c7,_55c6;
_55c6=this.m_parser;
while(!this.m_bErr){
_55c7=_55c6.next();
if(_55c7==XMLP._ELM_B){
this._fireEvent(SAXDriver.ELM_B,_55c6.getName(),this);
}else{
if(_55c7==XMLP._ELM_E){
this._fireEvent(SAXDriver.ELM_E,_55c6.getName());
}else{
if(_55c7==XMLP._ELM_EMP){
this._fireEvent(SAXDriver.ELM_B,_55c6.getName(),this);
this._fireEvent(SAXDriver.ELM_E,_55c6.getName());
}else{
if(_55c7==XMLP._TEXT){
this._fireEvent(SAXDriver.CHARS,_55c6.getContent(),_55c6.getContentBegin(),_55c6.getContentEnd()-_55c6.getContentBegin());
}else{
if(_55c7==XMLP._ENTITY){
this._fireEvent(SAXDriver.CHARS,_55c6.getContent(),_55c6.getContentBegin(),_55c6.getContentEnd()-_55c6.getContentBegin());
}else{
if(_55c7==XMLP._PI){
this._fireEvent(SAXDriver.PI,_55c6.getName(),_55c6.getContent().substring(_55c6.getContentBegin(),_55c6.getContentEnd()));
}else{
if(_55c7==XMLP._CDATA){
this._fireEvent(SAXDriver.CD_B);
this._fireEvent(SAXDriver.CHARS,_55c6.getContent(),_55c6.getContentBegin(),_55c6.getContentEnd()-_55c6.getContentBegin());
this._fireEvent(SAXDriver.CD_E);
}else{
if(_55c7==XMLP._COMMENT){
this._fireEvent(SAXDriver.CMNT,_55c6.getContent(),_55c6.getContentBegin(),_55c6.getContentEnd()-_55c6.getContentBegin());
}else{
if(_55c7==XMLP._DTD){
}else{
if(_55c7==XMLP._ERROR){
this._fireError(_55c6.getContent());
}else{
if(_55c7==XMLP._NONE){
return;
}
}
}
}
}
}
}
}
}
}
}
}
};
SAXStrings=function(){
};
SAXStrings.WHITESPACE=" \t\n\r";
SAXStrings.QUOTES="\"'";
SAXStrings.getColumnNumber=function(strD,iP){
if(SAXStrings.isEmpty(strD)){
return -1;
}
iP=iP||strD.length;
var arrD=strD.substring(0,iP).split("\n");
var _55cb=arrD[arrD.length-1];
arrD.length--;
var _55cc=arrD.join("\n").length;
return iP-_55cc;
};
SAXStrings.getLineNumber=function(strD,iP){
if(SAXStrings.isEmpty(strD)){
return -1;
}
iP=iP||strD.length;
return strD.substring(0,iP).split("\n").length;
};
SAXStrings.indexOfNonWhitespace=function(strD,iB,iE){
if(SAXStrings.isEmpty(strD)){
return -1;
}
iB=iB||0;
iE=iE||strD.length;
for(var i=iB;i<iE;i++){
if(SAXStrings.WHITESPACE.indexOf(strD.charAt(i))==-1){
return i;
}
}
return -1;
};
SAXStrings.indexOfWhitespace=function(strD,iB,iE){
if(SAXStrings.isEmpty(strD)){
return -1;
}
iB=iB||0;
iE=iE||strD.length;
for(var i=iB;i<iE;i++){
if(SAXStrings.WHITESPACE.indexOf(strD.charAt(i))!=-1){
return i;
}
}
return -1;
};
SAXStrings.isEmpty=function(strD){
return (strD===null)||(strD.length===0);
};
SAXStrings.lastIndexOfNonWhitespace=function(strD,iB,iE){
if(SAXStrings.isEmpty(strD)){
return -1;
}
iB=iB||0;
iE=iE||strD.length;
for(var i=iE-1;i>=iB;i--){
if(SAXStrings.WHITESPACE.indexOf(strD.charAt(i))==-1){
return i;
}
}
return -1;
};
SAXStrings.replace=function(strD,iB,iE,strF,strR){
if(SAXStrings.isEmpty(strD)){
return "";
}
iB=iB||0;
iE=iE||strD.length;
return strD.substring(iB,iE).split(strF).join(strR);
};
Stack=function(){
this.m_arr=[];
};
Stack.prototype.clear=function(){
this.m_arr=[];
};
Stack.prototype.count=function(){
return this.m_arr.length;
};
Stack.prototype.destroy=function(){
this.m_arr=null;
};
Stack.prototype.peek=function(){
if(this.m_arr.length===0){
return null;
}
return this.m_arr[this.m_arr.length-1];
};
Stack.prototype.pop=function(){
if(this.m_arr.length===0){
return null;
}
var o=this.m_arr[this.m_arr.length-1];
this.m_arr.length--;
return o;
};
Stack.prototype.push=function(o){
this.m_arr[this.m_arr.length]=o;
};
function isEmpty(str){
return (str===null)||(str.length==0);
}
function trim(_55e4,_55e5,_55e6){
if(isEmpty(_55e4)){
return "";
}
if(_55e5===null){
_55e5=true;
}
if(_55e6===null){
_55e6=true;
}
var left=0;
var right=0;
var i=0;
var k=0;
if(_55e5==true){
while((i<_55e4.length)&&(whitespace.indexOf(_55e4.charAt(i++))!=-1)){
left++;
}
}
if(_55e6==true){
k=_55e4.length-1;
while((k>=left)&&(whitespace.indexOf(_55e4.charAt(k--))!=-1)){
right++;
}
}
return _55e4.substring(left,_55e4.length-right);
}
function __escapeString(str){
var _55ec=/&/g;
var _55ed=/</g;
var _55ee=/>/g;
var _55ef=/"/g;
var _55f0=/'/g;
str=str.replace(_55ec,"&amp;");
str=str.replace(_55ed,"&lt;");
str=str.replace(_55ee,"&gt;");
str=str.replace(_55ef,"&quot;");
str=str.replace(_55f0,"&apos;");
return str;
}
function __unescapeString(str){
var _55f2=/&amp;/g;
var _55f3=/&lt;/g;
var _55f4=/&gt;/g;
var _55f5=/&quot;/g;
var _55f6=/&apos;/g;
str=str.replace(_55f2,"&");
str=str.replace(_55f3,"<");
str=str.replace(_55f4,">");
str=str.replace(_55f5,"\"");
str=str.replace(_55f6,"'");
return str;
}
function addClass(_55f9,_55fa){
if(_55f9){
if(_55f9.indexOf("|"+_55fa+"|")<0){
_55f9+=_55fa+"|";
}
}else{
_55f9="|"+_55fa+"|";
}
return _55f9;
}
DOMException=function(code){
this._class=addClass(this._class,"DOMException");
this.code=code;
};
DOMException.INDEX_SIZE_ERR=1;
DOMException.DOMSTRING_SIZE_ERR=2;
DOMException.HIERARCHY_REQUEST_ERR=3;
DOMException.WRONG_DOCUMENT_ERR=4;
DOMException.INVALID_CHARACTER_ERR=5;
DOMException.NO_DATA_ALLOWED_ERR=6;
DOMException.NO_MODIFICATION_ALLOWED_ERR=7;
DOMException.NOT_FOUND_ERR=8;
DOMException.NOT_SUPPORTED_ERR=9;
DOMException.INUSE_ATTRIBUTE_ERR=10;
DOMException.INVALID_STATE_ERR=11;
DOMException.SYNTAX_ERR=12;
DOMException.INVALID_MODIFICATION_ERR=13;
DOMException.NAMESPACE_ERR=14;
DOMException.INVALID_ACCESS_ERR=15;
DOMImplementation=function(){
this._class=addClass(this._class,"DOMImplementation");
this._p=null;
this.preserveWhiteSpace=false;
this.namespaceAware=true;
this.errorChecking=true;
};
DOMImplementation.prototype.escapeString=function DOMNode__escapeString(str){
return __escapeString(str);
};
DOMImplementation.prototype.unescapeString=function DOMNode__unescapeString(str){
return __unescapeString(str);
};
DOMImplementation.prototype.hasFeature=function DOMImplementation_hasFeature(_55fe,_55ff){
var ret=false;
if(_55fe.toLowerCase()=="xml"){
ret=(!_55ff||(_55ff=="1.0")||(_55ff=="2.0"));
}else{
if(_55fe.toLowerCase()=="core"){
ret=(!_55ff||(_55ff=="2.0"));
}
}
return ret;
};
DOMImplementation.prototype.loadXML=function DOMImplementation_loadXML(_5601){
var _5602;
try{
_5602=new XMLP(_5601);
}
catch(e){
alert("Error Creating the SAX Parser. Did you include xmlsax.js or tinyxmlsax.js in your web page?\nThe SAX parser is needed to populate XML for <SCRIPT>'s W3C DOM Parser with data.");
}
var doc=new DOMDocument(this);
this._parseLoop(doc,_5602);
doc._parseComplete=true;
return doc;
};
DOMImplementation.prototype.translateErrCode=function DOMImplementation_translateErrCode(code){
var msg="";
switch(code){
case DOMException.INDEX_SIZE_ERR:
msg="INDEX_SIZE_ERR: Index out of bounds";
break;
case DOMException.DOMSTRING_SIZE_ERR:
msg="DOMSTRING_SIZE_ERR: The resulting string is too long to fit in a DOMString";
break;
case DOMException.HIERARCHY_REQUEST_ERR:
msg="HIERARCHY_REQUEST_ERR: The Node can not be inserted at this location";
break;
case DOMException.WRONG_DOCUMENT_ERR:
msg="WRONG_DOCUMENT_ERR: The source and the destination Documents are not the same";
break;
case DOMException.INVALID_CHARACTER_ERR:
msg="INVALID_CHARACTER_ERR: The string contains an invalid character";
break;
case DOMException.NO_DATA_ALLOWED_ERR:
msg="NO_DATA_ALLOWED_ERR: This Node / NodeList does not support data";
break;
case DOMException.NO_MODIFICATION_ALLOWED_ERR:
msg="NO_MODIFICATION_ALLOWED_ERR: This object cannot be modified";
break;
case DOMException.NOT_FOUND_ERR:
msg="NOT_FOUND_ERR: The item cannot be found";
break;
case DOMException.NOT_SUPPORTED_ERR:
msg="NOT_SUPPORTED_ERR: This implementation does not support function";
break;
case DOMException.INUSE_ATTRIBUTE_ERR:
msg="INUSE_ATTRIBUTE_ERR: The Attribute has already been assigned to another Element";
break;
case DOMException.INVALID_STATE_ERR:
msg="INVALID_STATE_ERR: The object is no longer usable";
break;
case DOMException.SYNTAX_ERR:
msg="SYNTAX_ERR: Syntax error";
break;
case DOMException.INVALID_MODIFICATION_ERR:
msg="INVALID_MODIFICATION_ERR: Cannot change the type of the object";
break;
case DOMException.NAMESPACE_ERR:
msg="NAMESPACE_ERR: The namespace declaration is incorrect";
break;
case DOMException.INVALID_ACCESS_ERR:
msg="INVALID_ACCESS_ERR: The object does not support this function";
break;
default:
msg="UNKNOWN: Unknown Exception Code ("+code+")";
}
return msg;
};
DOMImplementation.prototype._parseLoop=function DOMImplementation__parseLoop(doc,p){
var iEvt,iNode,iAttr,strName;
iNodeParent=doc;
var _5609=0;
var _560a=[];
var _560b=[];
if(this.namespaceAware){
var iNS=doc.createNamespace("");
iNS.setValue("http://www.w3.org/2000/xmlns/");
doc._namespaces.setNamedItem(iNS);
}
while(true){
iEvt=p.next();
if(iEvt==XMLP._ELM_B){
var pName=p.getName();
pName=trim(pName,true,true);
if(!this.namespaceAware){
iNode=doc.createElement(p.getName());
for(var i=0;i<p.getAttributeCount();i++){
strName=p.getAttributeName(i);
iAttr=iNode.getAttributeNode(strName);
if(!iAttr){
iAttr=doc.createAttribute(strName);
}
iAttr.setValue(p.getAttributeValue(i));
iNode.setAttributeNode(iAttr);
}
}else{
iNode=doc.createElementNS("",p.getName());
iNode._namespaces=iNodeParent._namespaces._cloneNodes(iNode);
for(var i=0;i<p.getAttributeCount();i++){
strName=p.getAttributeName(i);
if(this._isNamespaceDeclaration(strName)){
var _560f=this._parseNSName(strName);
if(strName!="xmlns"){
iNS=doc.createNamespace(strName);
}else{
iNS=doc.createNamespace("");
}
iNS.setValue(p.getAttributeValue(i));
iNode._namespaces.setNamedItem(iNS);
}else{
iAttr=iNode.getAttributeNode(strName);
if(!iAttr){
iAttr=doc.createAttributeNS("",strName);
}
iAttr.setValue(p.getAttributeValue(i));
iNode.setAttributeNodeNS(iAttr);
if(this._isIdDeclaration(strName)){
iNode.id=p.getAttributeValue(i);
}
}
}
if(iNode._namespaces.getNamedItem(iNode.prefix)){
iNode.namespaceURI=iNode._namespaces.getNamedItem(iNode.prefix).value;
}
for(var i=0;i<iNode.attributes.length;i++){
if(iNode.attributes.item(i).prefix!=""){
if(iNode._namespaces.getNamedItem(iNode.attributes.item(i).prefix)){
iNode.attributes.item(i).namespaceURI=iNode._namespaces.getNamedItem(iNode.attributes.item(i).prefix).value;
}
}
}
}
if(iNodeParent.nodeType==DOMNode.DOCUMENT_NODE){
iNodeParent.documentElement=iNode;
}
iNodeParent.appendChild(iNode);
iNodeParent=iNode;
}else{
if(iEvt==XMLP._ELM_E){
iNodeParent=iNodeParent.parentNode;
}else{
if(iEvt==XMLP._ELM_EMP){
pName=p.getName();
pName=trim(pName,true,true);
if(!this.namespaceAware){
iNode=doc.createElement(pName);
for(var i=0;i<p.getAttributeCount();i++){
strName=p.getAttributeName(i);
iAttr=iNode.getAttributeNode(strName);
if(!iAttr){
iAttr=doc.createAttribute(strName);
}
iAttr.setValue(p.getAttributeValue(i));
iNode.setAttributeNode(iAttr);
}
}else{
iNode=doc.createElementNS("",p.getName());
iNode._namespaces=iNodeParent._namespaces._cloneNodes(iNode);
for(var i=0;i<p.getAttributeCount();i++){
strName=p.getAttributeName(i);
if(this._isNamespaceDeclaration(strName)){
var _560f=this._parseNSName(strName);
if(strName!="xmlns"){
iNS=doc.createNamespace(strName);
}else{
iNS=doc.createNamespace("");
}
iNS.setValue(p.getAttributeValue(i));
iNode._namespaces.setNamedItem(iNS);
}else{
iAttr=iNode.getAttributeNode(strName);
if(!iAttr){
iAttr=doc.createAttributeNS("",strName);
}
iAttr.setValue(p.getAttributeValue(i));
iNode.setAttributeNodeNS(iAttr);
if(this._isIdDeclaration(strName)){
iNode.id=p.getAttributeValue(i);
}
}
}
if(iNode._namespaces.getNamedItem(iNode.prefix)){
iNode.namespaceURI=iNode._namespaces.getNamedItem(iNode.prefix).value;
}
for(var i=0;i<iNode.attributes.length;i++){
if(iNode.attributes.item(i).prefix!=""){
if(iNode._namespaces.getNamedItem(iNode.attributes.item(i).prefix)){
iNode.attributes.item(i).namespaceURI=iNode._namespaces.getNamedItem(iNode.attributes.item(i).prefix).value;
}
}
}
}
if(iNodeParent.nodeType==DOMNode.DOCUMENT_NODE){
iNodeParent.documentElement=iNode;
}
iNodeParent.appendChild(iNode);
}else{
if(iEvt==XMLP._TEXT||iEvt==XMLP._ENTITY){
var _5610=p.getContent().substring(p.getContentBegin(),p.getContentEnd());
if(!this.preserveWhiteSpace){
if(trim(_5610,true,true)==""){
_5610="";
}
}
if(_5610.length>0){
var _5611=doc.createTextNode(_5610);
iNodeParent.appendChild(_5611);
if(iEvt==XMLP._ENTITY){
_560a[_560a.length]=_5611;
}else{
_560b[_560b.length]=_5611;
}
}
}else{
if(iEvt==XMLP._PI){
iNodeParent.appendChild(doc.createProcessingInstruction(p.getName(),p.getContent().substring(p.getContentBegin(),p.getContentEnd())));
}else{
if(iEvt==XMLP._CDATA){
_5610=p.getContent().substring(p.getContentBegin(),p.getContentEnd());
if(!this.preserveWhiteSpace){
_5610=trim(_5610,true,true);
_5610.replace(/ +/g," ");
}
if(_5610.length>0){
iNodeParent.appendChild(doc.createCDATASection(_5610));
}
}else{
if(iEvt==XMLP._COMMENT){
var _5610=p.getContent().substring(p.getContentBegin(),p.getContentEnd());
if(!this.preserveWhiteSpace){
_5610=trim(_5610,true,true);
_5610.replace(/ +/g," ");
}
if(_5610.length>0){
iNodeParent.appendChild(doc.createComment(_5610));
}
}else{
if(iEvt==XMLP._DTD){
}else{
if(iEvt==XMLP._ERROR){
throw (new DOMException(DOMException.SYNTAX_ERR));
}else{
if(iEvt==XMLP._NONE){
if(iNodeParent==doc){
break;
}else{
throw (new DOMException(DOMException.SYNTAX_ERR));
}
}
}
}
}
}
}
}
}
}
}
}
var _5612=_560a.length;
for(intLoop=0;intLoop<_5612;intLoop++){
var _5613=_560a[intLoop];
var _5614=_5613.getParentNode();
if(_5614){
_5614.normalize();
if(!this.preserveWhiteSpace){
var _5615=_5614.getChildNodes();
var _5616=_5615.getLength();
for(intLoop2=0;intLoop2<_5616;intLoop2++){
var child=_5615.item(intLoop2);
if(child.getNodeType()==DOMNode.TEXT_NODE){
var _5618=child.getData();
_5618=trim(_5618,true,true);
_5618.replace(/ +/g," ");
child.setData(_5618);
}
}
}
}
}
if(!this.preserveWhiteSpace){
var _5612=_560b.length;
for(intLoop=0;intLoop<_5612;intLoop++){
var node=_560b[intLoop];
if(node.getParentNode()!==null){
var _561a=node.getData();
_561a=trim(_561a,true,true);
_561a.replace(/ +/g," ");
node.setData(_561a);
}
}
}
};
DOMImplementation.prototype._isNamespaceDeclaration=function DOMImplementation__isNamespaceDeclaration(_561b){
return (_561b.indexOf("xmlns")>-1);
};
DOMImplementation.prototype._isIdDeclaration=function DOMImplementation__isIdDeclaration(_561c){
return (_561c.toLowerCase()=="id");
};
DOMImplementation.prototype._isValidName=function DOMImplementation__isValidName(name){
return name.match(re_validName);
};
re_validName=/^[a-zA-Z_:][a-zA-Z0-9\.\-_:]*$/;
DOMImplementation.prototype._isValidString=function DOMImplementation__isValidString(name){
return (name.search(re_invalidStringChars)<0);
};
re_invalidStringChars=/\x01|\x02|\x03|\x04|\x05|\x06|\x07|\x08|\x0B|\x0C|\x0E|\x0F|\x10|\x11|\x12|\x13|\x14|\x15|\x16|\x17|\x18|\x19|\x1A|\x1B|\x1C|\x1D|\x1E|\x1F|\x7F/;
DOMImplementation.prototype._parseNSName=function DOMImplementation__parseNSName(_561f){
var _5620={};
_5620.prefix=_561f;
_5620.namespaceName="";
delimPos=_561f.indexOf(":");
if(delimPos>-1){
_5620.prefix=_561f.substring(0,delimPos);
_5620.namespaceName=_561f.substring(delimPos+1,_561f.length);
}
return _5620;
};
DOMImplementation.prototype._parseQName=function DOMImplementation__parseQName(_5621){
var _5622={};
_5622.localName=_5621;
_5622.prefix="";
delimPos=_5621.indexOf(":");
if(delimPos>-1){
_5622.prefix=_5621.substring(0,delimPos);
_5622.localName=_5621.substring(delimPos+1,_5621.length);
}
return _5622;
};
DOMNodeList=function(_5623,_5624){
this._class=addClass(this._class,"DOMNodeList");
this._nodes=[];
this.length=0;
this.parentNode=_5624;
this.ownerDocument=_5623;
this._readonly=false;
};
DOMNodeList.prototype.getLength=function DOMNodeList_getLength(){
return this.length;
};
DOMNodeList.prototype.item=function DOMNodeList_item(index){
var ret=null;
if((index>=0)&&(index<this._nodes.length)){
ret=this._nodes[index];
}
return ret;
};
DOMNodeList.prototype._findItemIndex=function DOMNodeList__findItemIndex(id){
var ret=-1;
if(id>-1){
for(var i=0;i<this._nodes.length;i++){
if(this._nodes[i]._id==id){
ret=i;
break;
}
}
}
return ret;
};
DOMNodeList.prototype._insertBefore=function DOMNodeList__insertBefore(_562a,_562b){
if((_562b>=0)&&(_562b<this._nodes.length)){
var _562c=[];
_562c=this._nodes.slice(0,_562b);
if(_562a.nodeType==DOMNode.DOCUMENT_FRAGMENT_NODE){
_562c=_562c.concat(_562a.childNodes._nodes);
}else{
_562c[_562c.length]=_562a;
}
this._nodes=_562c.concat(this._nodes.slice(_562b));
this.length=this._nodes.length;
}
};
DOMNodeList.prototype._replaceChild=function DOMNodeList__replaceChild(_562d,_562e){
var ret=null;
if((_562e>=0)&&(_562e<this._nodes.length)){
ret=this._nodes[_562e];
if(_562d.nodeType==DOMNode.DOCUMENT_FRAGMENT_NODE){
var _5630=[];
_5630=this._nodes.slice(0,_562e);
_5630=_5630.concat(_562d.childNodes._nodes);
this._nodes=_5630.concat(this._nodes.slice(_562e+1));
}else{
this._nodes[_562e]=_562d;
}
}
return ret;
};
DOMNodeList.prototype._removeChild=function DOMNodeList__removeChild(_5631){
var ret=null;
if(_5631>-1){
ret=this._nodes[_5631];
var _5633=[];
_5633=this._nodes.slice(0,_5631);
this._nodes=_5633.concat(this._nodes.slice(_5631+1));
this.length=this._nodes.length;
}
return ret;
};
DOMNodeList.prototype._appendChild=function DOMNodeList__appendChild(_5634){
if(_5634.nodeType==DOMNode.DOCUMENT_FRAGMENT_NODE){
this._nodes=this._nodes.concat(_5634.childNodes._nodes);
}else{
this._nodes[this._nodes.length]=_5634;
}
this.length=this._nodes.length;
};
DOMNodeList.prototype._cloneNodes=function DOMNodeList__cloneNodes(deep,_5636){
var _5637=new DOMNodeList(this.ownerDocument,_5636);
for(var i=0;i<this._nodes.length;i++){
_5637._appendChild(this._nodes[i].cloneNode(deep));
}
return _5637;
};
DOMNodeList.prototype.toString=function DOMNodeList_toString(){
var ret="";
for(var i=0;i<this.length;i++){
ret+=this._nodes[i].toString();
}
return ret;
};
DOMNamedNodeMap=function(_563b,_563c){
this._class=addClass(this._class,"DOMNamedNodeMap");
this.DOMNodeList=DOMNodeList;
this.DOMNodeList(_563b,_563c);
};
DOMNamedNodeMap.prototype=new DOMNodeList;
DOMNamedNodeMap.prototype.getNamedItem=function DOMNamedNodeMap_getNamedItem(name){
var ret=null;
var _563f=this._findNamedItemIndex(name);
if(_563f>-1){
ret=this._nodes[_563f];
}
return ret;
};
DOMNamedNodeMap.prototype.setNamedItem=function DOMNamedNodeMap_setNamedItem(arg){
if(this.ownerDocument.implementation.errorChecking){
if(this.ownerDocument!=arg.ownerDocument){
throw (new DOMException(DOMException.WRONG_DOCUMENT_ERR));
}
if(this._readonly||(this.parentNode&&this.parentNode._readonly)){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if(arg.ownerElement&&(arg.ownerElement!=this.parentNode)){
throw (new DOMException(DOMException.INUSE_ATTRIBUTE_ERR));
}
}
var _5641=this._findNamedItemIndex(arg.name);
var ret=null;
if(_5641>-1){
ret=this._nodes[_5641];
if(this.ownerDocument.implementation.errorChecking&&ret._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}else{
this._nodes[_5641]=arg;
}
}else{
this._nodes[this.length]=arg;
}
this.length=this._nodes.length;
arg.ownerElement=this.parentNode;
return ret;
};
DOMNamedNodeMap.prototype.removeNamedItem=function DOMNamedNodeMap_removeNamedItem(name){
var ret=null;
if(this.ownerDocument.implementation.errorChecking&&(this._readonly||(this.parentNode&&this.parentNode._readonly))){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
var _5645=this._findNamedItemIndex(name);
if(this.ownerDocument.implementation.errorChecking&&(_5645<0)){
throw (new DOMException(DOMException.NOT_FOUND_ERR));
}
var _5646=this._nodes[_5645];
if(this.ownerDocument.implementation.errorChecking&&_5646._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
return this._removeChild(_5645);
};
DOMNamedNodeMap.prototype.getNamedItemNS=function DOMNamedNodeMap_getNamedItemNS(_5647,_5648){
var ret=null;
var _564a=this._findNamedItemNSIndex(_5647,_5648);
if(_564a>-1){
ret=this._nodes[_564a];
}
return ret;
};
DOMNamedNodeMap.prototype.setNamedItemNS=function DOMNamedNodeMap_setNamedItemNS(arg){
if(this.ownerDocument.implementation.errorChecking){
if(this._readonly||(this.parentNode&&this.parentNode._readonly)){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if(this.ownerDocument!=arg.ownerDocument){
throw (new DOMException(DOMException.WRONG_DOCUMENT_ERR));
}
if(arg.ownerElement&&(arg.ownerElement!=this.parentNode)){
throw (new DOMException(DOMException.INUSE_ATTRIBUTE_ERR));
}
}
var _564c=this._findNamedItemNSIndex(arg.namespaceURI,arg.localName);
var ret=null;
if(_564c>-1){
ret=this._nodes[_564c];
if(this.ownerDocument.implementation.errorChecking&&ret._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}else{
this._nodes[_564c]=arg;
}
}else{
this._nodes[this.length]=arg;
}
this.length=this._nodes.length;
arg.ownerElement=this.parentNode;
return ret;
};
DOMNamedNodeMap.prototype.removeNamedItemNS=function DOMNamedNodeMap_removeNamedItemNS(_564e,_564f){
var ret=null;
if(this.ownerDocument.implementation.errorChecking&&(this._readonly||(this.parentNode&&this.parentNode._readonly))){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
var _5651=this._findNamedItemNSIndex(_564e,_564f);
if(this.ownerDocument.implementation.errorChecking&&(_5651<0)){
throw (new DOMException(DOMException.NOT_FOUND_ERR));
}
var _5652=this._nodes[_5651];
if(this.ownerDocument.implementation.errorChecking&&_5652._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
return this._removeChild(_5651);
};
DOMNamedNodeMap.prototype._findNamedItemIndex=function DOMNamedNodeMap__findNamedItemIndex(name){
var ret=-1;
for(var i=0;i<this._nodes.length;i++){
if(this._nodes[i].name==name){
ret=i;
break;
}
}
return ret;
};
DOMNamedNodeMap.prototype._findNamedItemNSIndex=function DOMNamedNodeMap__findNamedItemNSIndex(_5656,_5657){
var ret=-1;
if(_5657){
for(var i=0;i<this._nodes.length;i++){
if((this._nodes[i].namespaceURI==_5656)&&(this._nodes[i].localName==_5657)){
ret=i;
break;
}
}
}
return ret;
};
DOMNamedNodeMap.prototype._hasAttribute=function DOMNamedNodeMap__hasAttribute(name){
var ret=false;
var _565c=this._findNamedItemIndex(name);
if(_565c>-1){
ret=true;
}
return ret;
};
DOMNamedNodeMap.prototype._hasAttributeNS=function DOMNamedNodeMap__hasAttributeNS(_565d,_565e){
var ret=false;
var _5660=this._findNamedItemNSIndex(_565d,_565e);
if(_5660>-1){
ret=true;
}
return ret;
};
DOMNamedNodeMap.prototype._cloneNodes=function DOMNamedNodeMap__cloneNodes(_5661){
var _5662=new DOMNamedNodeMap(this.ownerDocument,_5661);
for(var i=0;i<this._nodes.length;i++){
_5662._appendChild(this._nodes[i].cloneNode(false));
}
return _5662;
};
DOMNamedNodeMap.prototype.toString=function DOMNamedNodeMap_toString(){
var ret="";
for(var i=0;i<this.length-1;i++){
ret+=this._nodes[i].toString()+" ";
}
if(this.length>0){
ret+=this._nodes[this.length-1].toString();
}
return ret;
};
DOMNamespaceNodeMap=function(_5666,_5667){
this._class=addClass(this._class,"DOMNamespaceNodeMap");
this.DOMNamedNodeMap=DOMNamedNodeMap;
this.DOMNamedNodeMap(_5666,_5667);
};
DOMNamespaceNodeMap.prototype=new DOMNamedNodeMap;
DOMNamespaceNodeMap.prototype._findNamedItemIndex=function DOMNamespaceNodeMap__findNamedItemIndex(_5668){
var ret=-1;
for(var i=0;i<this._nodes.length;i++){
if(this._nodes[i].localName==_5668){
ret=i;
break;
}
}
return ret;
};
DOMNamespaceNodeMap.prototype._cloneNodes=function DOMNamespaceNodeMap__cloneNodes(_566b){
var _566c=new DOMNamespaceNodeMap(this.ownerDocument,_566b);
for(var i=0;i<this._nodes.length;i++){
_566c._appendChild(this._nodes[i].cloneNode(false));
}
return _566c;
};
DOMNamespaceNodeMap.prototype.toString=function DOMNamespaceNodeMap_toString(){
var ret="";
for(var ind=0;ind<this._nodes.length;ind++){
var ns=null;
try{
var ns=this.parentNode.parentNode._namespaces.getNamedItem(this._nodes[ind].localName);
}
catch(e){
break;
}
if(!(ns&&(""+ns.nodeValue==""+this._nodes[ind].nodeValue))){
ret+=this._nodes[ind].toString()+" ";
}
}
return ret;
};
DOMNode=function(_5671){
this._class=addClass(this._class,"DOMNode");
if(_5671){
this._id=_5671._genId();
}
this.namespaceURI="";
this.prefix="";
this.localName="";
this.nodeName="";
this.nodeValue="";
this.nodeType=0;
this.parentNode=null;
this.childNodes=new DOMNodeList(_5671,this);
this.firstChild=null;
this.lastChild=null;
this.previousSibling=null;
this.nextSibling=null;
this.attributes=new DOMNamedNodeMap(_5671,this);
this.ownerDocument=_5671;
this._namespaces=new DOMNamespaceNodeMap(_5671,this);
this._readonly=false;
};
DOMNode.ELEMENT_NODE=1;
DOMNode.ATTRIBUTE_NODE=2;
DOMNode.TEXT_NODE=3;
DOMNode.CDATA_SECTION_NODE=4;
DOMNode.ENTITY_REFERENCE_NODE=5;
DOMNode.ENTITY_NODE=6;
DOMNode.PROCESSING_INSTRUCTION_NODE=7;
DOMNode.COMMENT_NODE=8;
DOMNode.DOCUMENT_NODE=9;
DOMNode.DOCUMENT_TYPE_NODE=10;
DOMNode.DOCUMENT_FRAGMENT_NODE=11;
DOMNode.NOTATION_NODE=12;
DOMNode.NAMESPACE_NODE=13;
DOMNode.prototype.hasAttributes=function DOMNode_hasAttributes(){
if(this.attributes.length===0){
return false;
}else{
return true;
}
};
DOMNode.prototype.getNodeName=function DOMNode_getNodeName(){
return this.nodeName;
};
DOMNode.prototype.getNodeValue=function DOMNode_getNodeValue(){
return this.nodeValue;
};
DOMNode.prototype.setNodeValue=function DOMNode_setNodeValue(_5672){
if(this.ownerDocument.implementation.errorChecking&&this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
this.nodeValue=_5672;
};
DOMNode.prototype.getNodeType=function DOMNode_getNodeType(){
return this.nodeType;
};
DOMNode.prototype.getParentNode=function DOMNode_getParentNode(){
return this.parentNode;
};
DOMNode.prototype.getChildNodes=function DOMNode_getChildNodes(){
return this.childNodes;
};
DOMNode.prototype.getFirstChild=function DOMNode_getFirstChild(){
return this.firstChild;
};
DOMNode.prototype.getLastChild=function DOMNode_getLastChild(){
return this.lastChild;
};
DOMNode.prototype.getPreviousSibling=function DOMNode_getPreviousSibling(){
return this.previousSibling;
};
DOMNode.prototype.getNextSibling=function DOMNode_getNextSibling(){
return this.nextSibling;
};
DOMNode.prototype.getAttributes=function DOMNode_getAttributes(){
return this.attributes;
};
DOMNode.prototype.getOwnerDocument=function DOMNode_getOwnerDocument(){
return this.ownerDocument;
};
DOMNode.prototype.getNamespaceURI=function DOMNode_getNamespaceURI(){
return this.namespaceURI;
};
DOMNode.prototype.getPrefix=function DOMNode_getPrefix(){
return this.prefix;
};
DOMNode.prototype.setPrefix=function DOMNode_setPrefix(_5673){
if(this.ownerDocument.implementation.errorChecking){
if(this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if(!this.ownerDocument.implementation._isValidName(_5673)){
throw (new DOMException(DOMException.INVALID_CHARACTER_ERR));
}
if(!this.ownerDocument._isValidNamespace(this.namespaceURI,_5673+":"+this.localName)){
throw (new DOMException(DOMException.NAMESPACE_ERR));
}
if((_5673=="xmlns")&&(this.namespaceURI!="http://www.w3.org/2000/xmlns/")){
throw (new DOMException(DOMException.NAMESPACE_ERR));
}
if((_5673=="")&&(this.localName=="xmlns")){
throw (new DOMException(DOMException.NAMESPACE_ERR));
}
}
this.prefix=_5673;
if(this.prefix!=""){
this.nodeName=this.prefix+":"+this.localName;
}else{
this.nodeName=this.localName;
}
};
DOMNode.prototype.getLocalName=function DOMNode_getLocalName(){
return this.localName;
};
DOMNode.prototype.insertBefore=function DOMNode_insertBefore(_5674,_5675){
var _5676;
if(this.ownerDocument.implementation.errorChecking){
if(this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if(this.ownerDocument!=_5674.ownerDocument){
throw (new DOMException(DOMException.WRONG_DOCUMENT_ERR));
}
if(this._isAncestor(_5674)){
throw (new DOMException(DOMException.HIERARCHY_REQUEST_ERR));
}
}
if(_5675){
var _5677=this.childNodes._findItemIndex(_5675._id);
if(this.ownerDocument.implementation.errorChecking&&(_5677<0)){
throw (new DOMException(DOMException.NOT_FOUND_ERR));
}
var _5678=_5674.parentNode;
if(_5678){
_5678.removeChild(_5674);
}
this.childNodes._insertBefore(_5674,this.childNodes._findItemIndex(_5675._id));
_5676=_5675.previousSibling;
if(_5674.nodeType==DOMNode.DOCUMENT_FRAGMENT_NODE){
if(_5674.childNodes._nodes.length>0){
for(var ind=0;ind<_5674.childNodes._nodes.length;ind++){
_5674.childNodes._nodes[ind].parentNode=this;
}
_5675.previousSibling=_5674.childNodes._nodes[_5674.childNodes._nodes.length-1];
}
}else{
_5674.parentNode=this;
_5675.previousSibling=_5674;
}
}else{
_5676=this.lastChild;
this.appendChild(_5674);
}
if(_5674.nodeType==DOMNode.DOCUMENT_FRAGMENT_NODE){
if(_5674.childNodes._nodes.length>0){
if(_5676){
_5676.nextSibling=_5674.childNodes._nodes[0];
}else{
this.firstChild=_5674.childNodes._nodes[0];
}
_5674.childNodes._nodes[0].previousSibling=_5676;
_5674.childNodes._nodes[_5674.childNodes._nodes.length-1].nextSibling=_5675;
}
}else{
if(_5676){
_5676.nextSibling=_5674;
}else{
this.firstChild=_5674;
}
_5674.previousSibling=_5676;
_5674.nextSibling=_5675;
}
return _5674;
};
DOMNode.prototype.replaceChild=function DOMNode_replaceChild(_567a,_567b){
var ret=null;
if(this.ownerDocument.implementation.errorChecking){
if(this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if(this.ownerDocument!=_567a.ownerDocument){
throw (new DOMException(DOMException.WRONG_DOCUMENT_ERR));
}
if(this._isAncestor(_567a)){
throw (new DOMException(DOMException.HIERARCHY_REQUEST_ERR));
}
}
var index=this.childNodes._findItemIndex(_567b._id);
if(this.ownerDocument.implementation.errorChecking&&(index<0)){
throw (new DOMException(DOMException.NOT_FOUND_ERR));
}
var _567e=_567a.parentNode;
if(_567e){
_567e.removeChild(_567a);
}
ret=this.childNodes._replaceChild(_567a,index);
if(_567a.nodeType==DOMNode.DOCUMENT_FRAGMENT_NODE){
if(_567a.childNodes._nodes.length>0){
for(var ind=0;ind<_567a.childNodes._nodes.length;ind++){
_567a.childNodes._nodes[ind].parentNode=this;
}
if(_567b.previousSibling){
_567b.previousSibling.nextSibling=_567a.childNodes._nodes[0];
}else{
this.firstChild=_567a.childNodes._nodes[0];
}
if(_567b.nextSibling){
_567b.nextSibling.previousSibling=_567a;
}else{
this.lastChild=_567a.childNodes._nodes[_567a.childNodes._nodes.length-1];
}
_567a.childNodes._nodes[0].previousSibling=_567b.previousSibling;
_567a.childNodes._nodes[_567a.childNodes._nodes.length-1].nextSibling=_567b.nextSibling;
}
}else{
_567a.parentNode=this;
if(_567b.previousSibling){
_567b.previousSibling.nextSibling=_567a;
}else{
this.firstChild=_567a;
}
if(_567b.nextSibling){
_567b.nextSibling.previousSibling=_567a;
}else{
this.lastChild=_567a;
}
_567a.previousSibling=_567b.previousSibling;
_567a.nextSibling=_567b.nextSibling;
}
return ret;
};
DOMNode.prototype.removeChild=function DOMNode_removeChild(_5680){
if(this.ownerDocument.implementation.errorChecking&&(this._readonly||_5680._readonly)){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
var _5681=this.childNodes._findItemIndex(_5680._id);
if(this.ownerDocument.implementation.errorChecking&&(_5681<0)){
throw (new DOMException(DOMException.NOT_FOUND_ERR));
}
this.childNodes._removeChild(_5681);
_5680.parentNode=null;
if(_5680.previousSibling){
_5680.previousSibling.nextSibling=_5680.nextSibling;
}else{
this.firstChild=_5680.nextSibling;
}
if(_5680.nextSibling){
_5680.nextSibling.previousSibling=_5680.previousSibling;
}else{
this.lastChild=_5680.previousSibling;
}
_5680.previousSibling=null;
_5680.nextSibling=null;
return _5680;
};
DOMNode.prototype.appendChild=function DOMNode_appendChild(_5682){
if(this.ownerDocument.implementation.errorChecking){
if(this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if(this.ownerDocument!=_5682.ownerDocument){
throw (new DOMException(DOMException.WRONG_DOCUMENT_ERR));
}
if(this._isAncestor(_5682)){
throw (new DOMException(DOMException.HIERARCHY_REQUEST_ERR));
}
}
var _5683=_5682.parentNode;
if(_5683){
_5683.removeChild(_5682);
}
this.childNodes._appendChild(_5682);
if(_5682.nodeType==DOMNode.DOCUMENT_FRAGMENT_NODE){
if(_5682.childNodes._nodes.length>0){
for(var ind=0;ind<_5682.childNodes._nodes.length;ind++){
_5682.childNodes._nodes[ind].parentNode=this;
}
if(this.lastChild){
this.lastChild.nextSibling=_5682.childNodes._nodes[0];
_5682.childNodes._nodes[0].previousSibling=this.lastChild;
this.lastChild=_5682.childNodes._nodes[_5682.childNodes._nodes.length-1];
}else{
this.lastChild=_5682.childNodes._nodes[_5682.childNodes._nodes.length-1];
this.firstChild=_5682.childNodes._nodes[0];
}
}
}else{
_5682.parentNode=this;
if(this.lastChild){
this.lastChild.nextSibling=_5682;
_5682.previousSibling=this.lastChild;
this.lastChild=_5682;
}else{
this.lastChild=_5682;
this.firstChild=_5682;
}
}
return _5682;
};
DOMNode.prototype.hasChildNodes=function DOMNode_hasChildNodes(){
return (this.childNodes.length>0);
};
DOMNode.prototype.cloneNode=function DOMNode_cloneNode(deep){
try{
return this.ownerDocument.importNode(this,deep);
}
catch(e){
return null;
}
};
DOMNode.prototype.normalize=function DOMNode_normalize(){
var inode;
var _5687=new DOMNodeList();
if(this.nodeType==DOMNode.ELEMENT_NODE||this.nodeType==DOMNode.DOCUMENT_NODE){
var _5688=null;
for(var i=0;i<this.childNodes.length;i++){
inode=this.childNodes.item(i);
if(inode.nodeType==DOMNode.TEXT_NODE){
if(inode.length<1){
_5687._appendChild(inode);
}else{
if(_5688){
_5688.appendData(inode.data);
_5687._appendChild(inode);
}else{
_5688=inode;
}
}
}else{
_5688=null;
inode.normalize();
}
}
for(var i=0;i<_5687.length;i++){
inode=_5687.item(i);
inode.parentNode.removeChild(inode);
}
}
};
DOMNode.prototype.isSupported=function DOMNode_isSupported(_568a,_568b){
return this.ownerDocument.implementation.hasFeature(_568a,_568b);
};
DOMNode.prototype.getElementsByTagName=function DOMNode_getElementsByTagName(_568c){
return this._getElementsByTagNameRecursive(_568c,new DOMNodeList(this.ownerDocument));
};
DOMNode.prototype._getElementsByTagNameRecursive=function DOMNode__getElementsByTagNameRecursive(_568d,_568e){
if(this.nodeType==DOMNode.ELEMENT_NODE||this.nodeType==DOMNode.DOCUMENT_NODE){
if((this.nodeName==_568d)||(_568d=="*")){
_568e._appendChild(this);
}
for(var i=0;i<this.childNodes.length;i++){
_568e=this.childNodes.item(i)._getElementsByTagNameRecursive(_568d,_568e);
}
}
return _568e;
};
DOMNode.prototype.getXML=function DOMNode_getXML(){
return this.toString();
};
DOMNode.prototype.getElementsByTagNameNS=function DOMNode_getElementsByTagNameNS(_5690,_5691){
return this._getElementsByTagNameNSRecursive(_5690,_5691,new DOMNodeList(this.ownerDocument));
};
DOMNode.prototype._getElementsByTagNameNSRecursive=function DOMNode__getElementsByTagNameNSRecursive(_5692,_5693,_5694){
if(this.nodeType==DOMNode.ELEMENT_NODE||this.nodeType==DOMNode.DOCUMENT_NODE){
if(((this.namespaceURI==_5692)||(_5692=="*"))&&((this.localName==_5693)||(_5693=="*"))){
_5694._appendChild(this);
}
for(var i=0;i<this.childNodes.length;i++){
_5694=this.childNodes.item(i)._getElementsByTagNameNSRecursive(_5692,_5693,_5694);
}
}
return _5694;
};
DOMNode.prototype._isAncestor=function DOMNode__isAncestor(node){
return ((this==node)||((this.parentNode)&&(this.parentNode._isAncestor(node))));
};
DOMNode.prototype.importNode=function DOMNode_importNode(_5697,deep){
var _5699;
this.getOwnerDocument()._performingImportNodeOperation=true;
try{
if(_5697.nodeType==DOMNode.ELEMENT_NODE){
if(!this.ownerDocument.implementation.namespaceAware){
_5699=this.ownerDocument.createElement(_5697.tagName);
for(var i=0;i<_5697.attributes.length;i++){
_5699.setAttribute(_5697.attributes.item(i).name,_5697.attributes.item(i).value);
}
}else{
_5699=this.ownerDocument.createElementNS(_5697.namespaceURI,_5697.nodeName);
for(var i=0;i<_5697.attributes.length;i++){
_5699.setAttributeNS(_5697.attributes.item(i).namespaceURI,_5697.attributes.item(i).name,_5697.attributes.item(i).value);
}
for(var i=0;i<_5697._namespaces.length;i++){
_5699._namespaces._nodes[i]=this.ownerDocument.createNamespace(_5697._namespaces.item(i).localName);
_5699._namespaces._nodes[i].setValue(_5697._namespaces.item(i).value);
}
}
}else{
if(_5697.nodeType==DOMNode.ATTRIBUTE_NODE){
if(!this.ownerDocument.implementation.namespaceAware){
_5699=this.ownerDocument.createAttribute(_5697.name);
}else{
_5699=this.ownerDocument.createAttributeNS(_5697.namespaceURI,_5697.nodeName);
for(var i=0;i<_5697._namespaces.length;i++){
_5699._namespaces._nodes[i]=this.ownerDocument.createNamespace(_5697._namespaces.item(i).localName);
_5699._namespaces._nodes[i].setValue(_5697._namespaces.item(i).value);
}
}
_5699.setValue(_5697.value);
}else{
if(_5697.nodeType==DOMNode.DOCUMENT_FRAGMENT){
_5699=this.ownerDocument.createDocumentFragment();
}else{
if(_5697.nodeType==DOMNode.NAMESPACE_NODE){
_5699=this.ownerDocument.createNamespace(_5697.nodeName);
_5699.setValue(_5697.value);
}else{
if(_5697.nodeType==DOMNode.TEXT_NODE){
_5699=this.ownerDocument.createTextNode(_5697.data);
}else{
if(_5697.nodeType==DOMNode.CDATA_SECTION_NODE){
_5699=this.ownerDocument.createCDATASection(_5697.data);
}else{
if(_5697.nodeType==DOMNode.PROCESSING_INSTRUCTION_NODE){
_5699=this.ownerDocument.createProcessingInstruction(_5697.target,_5697.data);
}else{
if(_5697.nodeType==DOMNode.COMMENT_NODE){
_5699=this.ownerDocument.createComment(_5697.data);
}else{
throw (new DOMException(DOMException.NOT_SUPPORTED_ERR));
}
}
}
}
}
}
}
}
if(deep){
for(var i=0;i<_5697.childNodes.length;i++){
_5699.appendChild(this.ownerDocument.importNode(_5697.childNodes.item(i),true));
}
}
this.getOwnerDocument()._performingImportNodeOperation=false;
return _5699;
}
catch(eAny){
this.getOwnerDocument()._performingImportNodeOperation=false;
throw eAny;
}
};
DOMNode.prototype.__escapeString=function DOMNode__escapeString(str){
return __escapeString(str);
};
DOMNode.prototype.__unescapeString=function DOMNode__unescapeString(str){
return __unescapeString(str);
};
DOMDocument=function(_569d){
this._class=addClass(this._class,"DOMDocument");
this.DOMNode=DOMNode;
this.DOMNode(this);
this.doctype=null;
this.implementation=_569d;
this.documentElement=null;
this.all=[];
this.nodeName="#document";
this.nodeType=DOMNode.DOCUMENT_NODE;
this._id=0;
this._lastId=0;
this._parseComplete=false;
this.ownerDocument=this;
this._performingImportNodeOperation=false;
};
DOMDocument.prototype=new DOMNode;
DOMDocument.prototype.getDoctype=function DOMDocument_getDoctype(){
return this.doctype;
};
DOMDocument.prototype.getImplementation=function DOMDocument_implementation(){
return this.implementation;
};
DOMDocument.prototype.getDocumentElement=function DOMDocument_getDocumentElement(){
return this.documentElement;
};
DOMDocument.prototype.createElement=function DOMDocument_createElement(_569e){
if(this.ownerDocument.implementation.errorChecking&&(!this.ownerDocument.implementation._isValidName(_569e))){
throw (new DOMException(DOMException.INVALID_CHARACTER_ERR));
}
var node=new DOMElement(this);
node.tagName=_569e;
node.nodeName=_569e;
this.all[this.all.length]=node;
return node;
};
DOMDocument.prototype.createDocumentFragment=function DOMDocument_createDocumentFragment(){
var node=new DOMDocumentFragment(this);
return node;
};
DOMDocument.prototype.createTextNode=function DOMDocument_createTextNode(data){
var node=new DOMText(this);
node.data=data;
node.nodeValue=data;
node.length=data.length;
return node;
};
DOMDocument.prototype.createComment=function DOMDocument_createComment(data){
var node=new DOMComment(this);
node.data=data;
node.nodeValue=data;
node.length=data.length;
return node;
};
DOMDocument.prototype.createCDATASection=function DOMDocument_createCDATASection(data){
var node=new DOMCDATASection(this);
node.data=data;
node.nodeValue=data;
node.length=data.length;
return node;
};
DOMDocument.prototype.createProcessingInstruction=function DOMDocument_createProcessingInstruction(_56a7,data){
if(this.ownerDocument.implementation.errorChecking&&(!this.implementation._isValidName(_56a7))){
throw (new DOMException(DOMException.INVALID_CHARACTER_ERR));
}
var node=new DOMProcessingInstruction(this);
node.target=_56a7;
node.nodeName=_56a7;
node.data=data;
node.nodeValue=data;
node.length=data.length;
return node;
};
DOMDocument.prototype.createAttribute=function DOMDocument_createAttribute(name){
if(this.ownerDocument.implementation.errorChecking&&(!this.ownerDocument.implementation._isValidName(name))){
throw (new DOMException(DOMException.INVALID_CHARACTER_ERR));
}
var node=new DOMAttr(this);
node.name=name;
node.nodeName=name;
return node;
};
DOMDocument.prototype.createElementNS=function DOMDocument_createElementNS(_56ac,_56ad){
if(this.ownerDocument.implementation.errorChecking){
if(!this.ownerDocument._isValidNamespace(_56ac,_56ad)){
throw (new DOMException(DOMException.NAMESPACE_ERR));
}
if(!this.ownerDocument.implementation._isValidName(_56ad)){
throw (new DOMException(DOMException.INVALID_CHARACTER_ERR));
}
}
var node=new DOMElement(this);
var qname=this.implementation._parseQName(_56ad);
node.nodeName=_56ad;
node.namespaceURI=_56ac;
node.prefix=qname.prefix;
node.localName=qname.localName;
node.tagName=_56ad;
this.all[this.all.length]=node;
return node;
};
DOMDocument.prototype.createAttributeNS=function DOMDocument_createAttributeNS(_56b0,_56b1){
if(this.ownerDocument.implementation.errorChecking){
if(!this.ownerDocument._isValidNamespace(_56b0,_56b1,true)){
throw (new DOMException(DOMException.NAMESPACE_ERR));
}
if(!this.ownerDocument.implementation._isValidName(_56b1)){
throw (new DOMException(DOMException.INVALID_CHARACTER_ERR));
}
}
var node=new DOMAttr(this);
var qname=this.implementation._parseQName(_56b1);
node.nodeName=_56b1;
node.namespaceURI=_56b0;
node.prefix=qname.prefix;
node.localName=qname.localName;
node.name=_56b1;
node.nodeValue="";
return node;
};
DOMDocument.prototype.createNamespace=function DOMDocument_createNamespace(_56b4){
var node=new DOMNamespace(this);
var qname=this.implementation._parseQName(_56b4);
node.nodeName=_56b4;
node.prefix=qname.prefix;
node.localName=qname.localName;
node.name=_56b4;
node.nodeValue="";
return node;
};
DOMDocument.prototype.getElementById=function DOMDocument_getElementById(_56b7){
retNode=null;
for(var i=0;i<this.all.length;i++){
var node=this.all[i];
if((node.id==_56b7)&&(node._isAncestor(node.ownerDocument.documentElement))){
retNode=node;
break;
}
}
return retNode;
};
DOMDocument.prototype._genId=function DOMDocument__genId(){
this._lastId+=1;
return this._lastId;
};
DOMDocument.prototype._isValidNamespace=function DOMDocument__isValidNamespace(_56ba,_56bb,_56bc){
if(this._performingImportNodeOperation==true){
return true;
}
var valid=true;
var qName=this.implementation._parseQName(_56bb);
if(this._parseComplete==true){
if(qName.localName.indexOf(":")>-1){
valid=false;
}
if((valid)&&(!_56bc)){
if(!_56ba){
valid=false;
}
}
if((valid)&&(qName.prefix=="")){
valid=false;
}
}
if((valid)&&(qName.prefix=="xml")&&(_56ba!="http://www.w3.org/XML/1998/namespace")){
valid=false;
}
return valid;
};
DOMDocument.prototype.toString=function DOMDocument_toString(){
return ""+this.childNodes;
};
DOMElement=function(_56bf){
this._class=addClass(this._class,"DOMElement");
this.DOMNode=DOMNode;
this.DOMNode(_56bf);
this.tagName="";
this.id="";
this.nodeType=DOMNode.ELEMENT_NODE;
};
DOMElement.prototype=new DOMNode;
DOMElement.prototype.getTagName=function DOMElement_getTagName(){
return this.tagName;
};
DOMElement.prototype.getAttribute=function DOMElement_getAttribute(name){
var ret="";
var attr=this.attributes.getNamedItem(name);
if(attr){
ret=attr.value;
}
return ret;
};
DOMElement.prototype.setAttribute=function DOMElement_setAttribute(name,value){
var attr=this.attributes.getNamedItem(name);
if(!attr){
attr=this.ownerDocument.createAttribute(name);
}
var value=new String(value);
if(this.ownerDocument.implementation.errorChecking){
if(attr._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if(!this.ownerDocument.implementation._isValidString(value)){
throw (new DOMException(DOMException.INVALID_CHARACTER_ERR));
}
}
if(this.ownerDocument.implementation._isIdDeclaration(name)){
this.id=value;
}
attr.value=value;
attr.nodeValue=value;
if(value.length>0){
attr.specified=true;
}else{
attr.specified=false;
}
this.attributes.setNamedItem(attr);
};
DOMElement.prototype.removeAttribute=function DOMElement_removeAttribute(name){
return this.attributes.removeNamedItem(name);
};
DOMElement.prototype.getAttributeNode=function DOMElement_getAttributeNode(name){
return this.attributes.getNamedItem(name);
};
DOMElement.prototype.setAttributeNode=function DOMElement_setAttributeNode(_56c8){
if(this.ownerDocument.implementation._isIdDeclaration(_56c8.name)){
this.id=_56c8.value;
}
return this.attributes.setNamedItem(_56c8);
};
DOMElement.prototype.removeAttributeNode=function DOMElement_removeAttributeNode(_56c9){
if(this.ownerDocument.implementation.errorChecking&&_56c9._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
var _56ca=this.attributes._findItemIndex(_56c9._id);
if(this.ownerDocument.implementation.errorChecking&&(_56ca<0)){
throw (new DOMException(DOMException.NOT_FOUND_ERR));
}
return this.attributes._removeChild(_56ca);
};
DOMElement.prototype.getAttributeNS=function DOMElement_getAttributeNS(_56cb,_56cc){
var ret="";
var attr=this.attributes.getNamedItemNS(_56cb,_56cc);
if(attr){
ret=attr.value;
}
return ret;
};
DOMElement.prototype.setAttributeNS=function DOMElement_setAttributeNS(_56cf,_56d0,value){
var attr=this.attributes.getNamedItem(_56cf,_56d0);
if(!attr){
attr=this.ownerDocument.createAttributeNS(_56cf,_56d0);
}
var value=new String(value);
if(this.ownerDocument.implementation.errorChecking){
if(attr._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if(!this.ownerDocument._isValidNamespace(_56cf,_56d0)){
throw (new DOMException(DOMException.NAMESPACE_ERR));
}
if(!this.ownerDocument.implementation._isValidString(value)){
throw (new DOMException(DOMException.INVALID_CHARACTER_ERR));
}
}
if(this.ownerDocument.implementation._isIdDeclaration(name)){
this.id=value;
}
attr.value=value;
attr.nodeValue=value;
if(value.length>0){
attr.specified=true;
}else{
attr.specified=false;
}
this.attributes.setNamedItemNS(attr);
};
DOMElement.prototype.removeAttributeNS=function DOMElement_removeAttributeNS(_56d3,_56d4){
return this.attributes.removeNamedItemNS(_56d3,_56d4);
};
DOMElement.prototype.getAttributeNodeNS=function DOMElement_getAttributeNodeNS(_56d5,_56d6){
return this.attributes.getNamedItemNS(_56d5,_56d6);
};
DOMElement.prototype.setAttributeNodeNS=function DOMElement_setAttributeNodeNS(_56d7){
if((_56d7.prefix=="")&&this.ownerDocument.implementation._isIdDeclaration(_56d7.name)){
this.id=_56d7.value;
}
return this.attributes.setNamedItemNS(_56d7);
};
DOMElement.prototype.hasAttribute=function DOMElement_hasAttribute(name){
return this.attributes._hasAttribute(name);
};
DOMElement.prototype.hasAttributeNS=function DOMElement_hasAttributeNS(_56d9,_56da){
return this.attributes._hasAttributeNS(_56d9,_56da);
};
DOMElement.prototype.toString=function DOMElement_toString(){
var ret="";
var ns=this._namespaces.toString();
if(ns.length>0){
ns=" "+ns;
}
var attrs=this.attributes.toString();
if(attrs.length>0){
attrs=" "+attrs;
}
ret+="<"+this.nodeName+ns+attrs+">";
ret+=this.childNodes.toString();
ret+="</"+this.nodeName+">";
return ret;
};
DOMAttr=function(_56de){
this._class=addClass(this._class,"DOMAttr");
this.DOMNode=DOMNode;
this.DOMNode(_56de);
this.name="";
this.specified=false;
this.value="";
this.nodeType=DOMNode.ATTRIBUTE_NODE;
this.ownerElement=null;
this.childNodes=null;
this.attributes=null;
};
DOMAttr.prototype=new DOMNode;
DOMAttr.prototype.getName=function DOMAttr_getName(){
return this.nodeName;
};
DOMAttr.prototype.getSpecified=function DOMAttr_getSpecified(){
return this.specified;
};
DOMAttr.prototype.getValue=function DOMAttr_getValue(){
return this.nodeValue;
};
DOMAttr.prototype.setValue=function DOMAttr_setValue(value){
if(this.ownerDocument.implementation.errorChecking&&this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
this.setNodeValue(value);
};
DOMAttr.prototype.setNodeValue=function DOMAttr_setNodeValue(value){
this.nodeValue=new String(value);
this.value=this.nodeValue;
this.specified=(this.value.length>0);
};
DOMAttr.prototype.toString=function DOMAttr_toString(){
var ret="";
ret+=this.nodeName+"=\""+this.__escapeString(this.nodeValue)+"\"";
return ret;
};
DOMAttr.prototype.getOwnerElement=function(){
return this.ownerElement;
};
DOMNamespace=function(_56e2){
this._class=addClass(this._class,"DOMNamespace");
this.DOMNode=DOMNode;
this.DOMNode(_56e2);
this.name="";
this.specified=false;
this.value="";
this.nodeType=DOMNode.NAMESPACE_NODE;
};
DOMNamespace.prototype=new DOMNode;
DOMNamespace.prototype.getValue=function DOMNamespace_getValue(){
return this.nodeValue;
};
DOMNamespace.prototype.setValue=function DOMNamespace_setValue(value){
this.nodeValue=new String(value);
this.value=this.nodeValue;
};
DOMNamespace.prototype.toString=function DOMNamespace_toString(){
var ret="";
if(this.nodeName!=""){
ret+=this.nodeName+"=\""+this.__escapeString(this.nodeValue)+"\"";
}else{
ret+="xmlns=\""+this.__escapeString(this.nodeValue)+"\"";
}
return ret;
};
DOMCharacterData=function(_56e5){
this._class=addClass(this._class,"DOMCharacterData");
this.DOMNode=DOMNode;
this.DOMNode(_56e5);
this.data="";
this.length=0;
};
DOMCharacterData.prototype=new DOMNode;
DOMCharacterData.prototype.getData=function DOMCharacterData_getData(){
return this.nodeValue;
};
DOMCharacterData.prototype.setData=function DOMCharacterData_setData(data){
this.setNodeValue(data);
};
DOMCharacterData.prototype.setNodeValue=function DOMCharacterData_setNodeValue(data){
if(this.ownerDocument.implementation.errorChecking&&this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
this.nodeValue=new String(data);
this.data=this.nodeValue;
this.length=this.nodeValue.length;
};
DOMCharacterData.prototype.getLength=function DOMCharacterData_getLength(){
return this.nodeValue.length;
};
DOMCharacterData.prototype.substringData=function DOMCharacterData_substringData(_56e8,count){
var ret=null;
if(this.data){
if(this.ownerDocument.implementation.errorChecking&&((_56e8<0)||(_56e8>this.data.length)||(count<0))){
throw (new DOMException(DOMException.INDEX_SIZE_ERR));
}
if(!count){
ret=this.data.substring(_56e8);
}else{
ret=this.data.substring(_56e8,_56e8+count);
}
}
return ret;
};
DOMCharacterData.prototype.appendData=function DOMCharacterData_appendData(arg){
if(this.ownerDocument.implementation.errorChecking&&this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
this.setData(""+this.data+arg);
};
DOMCharacterData.prototype.insertData=function DOMCharacterData_insertData(_56ec,arg){
if(this.ownerDocument.implementation.errorChecking&&this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if(this.data){
if(this.ownerDocument.implementation.errorChecking&&((_56ec<0)||(_56ec>this.data.length))){
throw (new DOMException(DOMException.INDEX_SIZE_ERR));
}
this.setData(this.data.substring(0,_56ec).concat(arg,this.data.substring(_56ec)));
}else{
if(this.ownerDocument.implementation.errorChecking&&(_56ec!=0)){
throw (new DOMException(DOMException.INDEX_SIZE_ERR));
}
this.setData(arg);
}
};
DOMCharacterData.prototype.deleteData=function DOMCharacterData_deleteData(_56ee,count){
if(this.ownerDocument.implementation.errorChecking&&this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if(this.data){
if(this.ownerDocument.implementation.errorChecking&&((_56ee<0)||(_56ee>this.data.length)||(count<0))){
throw (new DOMException(DOMException.INDEX_SIZE_ERR));
}
if(!count||(_56ee+count)>this.data.length){
this.setData(this.data.substring(0,_56ee));
}else{
this.setData(this.data.substring(0,_56ee).concat(this.data.substring(_56ee+count)));
}
}
};
DOMCharacterData.prototype.replaceData=function DOMCharacterData_replaceData(_56f0,count,arg){
if(this.ownerDocument.implementation.errorChecking&&this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if(this.data){
if(this.ownerDocument.implementation.errorChecking&&((_56f0<0)||(_56f0>this.data.length)||(count<0))){
throw (new DOMException(DOMException.INDEX_SIZE_ERR));
}
this.setData(this.data.substring(0,_56f0).concat(arg,this.data.substring(_56f0+count)));
}else{
this.setData(arg);
}
};
DOMText=function(_56f3){
this._class=addClass(this._class,"DOMText");
this.DOMCharacterData=DOMCharacterData;
this.DOMCharacterData(_56f3);
this.nodeName="#text";
this.nodeType=DOMNode.TEXT_NODE;
};
DOMText.prototype=new DOMCharacterData;
DOMText.prototype.splitText=function DOMText_splitText(_56f4){
var data,inode;
if(this.ownerDocument.implementation.errorChecking){
if(this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if((_56f4<0)||(_56f4>this.data.length)){
throw (new DOMException(DOMException.INDEX_SIZE_ERR));
}
}
if(this.parentNode){
data=this.substringData(_56f4);
inode=this.ownerDocument.createTextNode(data);
if(this.nextSibling){
this.parentNode.insertBefore(inode,this.nextSibling);
}else{
this.parentNode.appendChild(inode);
}
this.deleteData(_56f4);
}
return inode;
};
DOMText.prototype.toString=function DOMText_toString(){
return this.__escapeString(""+this.nodeValue);
};
DOMCDATASection=function(_56f6){
this._class=addClass(this._class,"DOMCDATASection");
this.DOMCharacterData=DOMCharacterData;
this.DOMCharacterData(_56f6);
this.nodeName="#cdata-section";
this.nodeType=DOMNode.CDATA_SECTION_NODE;
};
DOMCDATASection.prototype=new DOMCharacterData;
DOMCDATASection.prototype.splitText=function DOMCDATASection_splitText(_56f7){
var data,inode;
if(this.ownerDocument.implementation.errorChecking){
if(this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
if((_56f7<0)||(_56f7>this.data.length)){
throw (new DOMException(DOMException.INDEX_SIZE_ERR));
}
}
if(this.parentNode){
data=this.substringData(_56f7);
inode=this.ownerDocument.createCDATASection(data);
if(this.nextSibling){
this.parentNode.insertBefore(inode,this.nextSibling);
}else{
this.parentNode.appendChild(inode);
}
this.deleteData(_56f7);
}
return inode;
};
DOMCDATASection.prototype.toString=function DOMCDATASection_toString(){
var ret="";
ret+="<![CDATA["+this.nodeValue+"]]>";
return ret;
};
DOMComment=function(_56fa){
this._class=addClass(this._class,"DOMComment");
this.DOMCharacterData=DOMCharacterData;
this.DOMCharacterData(_56fa);
this.nodeName="#comment";
this.nodeType=DOMNode.COMMENT_NODE;
};
DOMComment.prototype=new DOMCharacterData;
DOMComment.prototype.toString=function DOMComment_toString(){
var ret="";
ret+="<!--"+this.nodeValue+"-->";
return ret;
};
DOMProcessingInstruction=function(_56fc){
this._class=addClass(this._class,"DOMProcessingInstruction");
this.DOMNode=DOMNode;
this.DOMNode(_56fc);
this.target="";
this.data="";
this.nodeType=DOMNode.PROCESSING_INSTRUCTION_NODE;
};
DOMProcessingInstruction.prototype=new DOMNode;
DOMProcessingInstruction.prototype.getTarget=function DOMProcessingInstruction_getTarget(){
return this.nodeName;
};
DOMProcessingInstruction.prototype.getData=function DOMProcessingInstruction_getData(){
return this.nodeValue;
};
DOMProcessingInstruction.prototype.setData=function DOMProcessingInstruction_setData(data){
this.setNodeValue(data);
};
DOMProcessingInstruction.prototype.setNodeValue=function DOMProcessingInstruction_setNodeValue(data){
if(this.ownerDocument.implementation.errorChecking&&this._readonly){
throw (new DOMException(DOMException.NO_MODIFICATION_ALLOWED_ERR));
}
this.nodeValue=new String(data);
this.data=this.nodeValue;
};
DOMProcessingInstruction.prototype.toString=function DOMProcessingInstruction_toString(){
var ret="";
ret+="<?"+this.nodeName+" "+this.nodeValue+" ?>";
return ret;
};
DOMDocumentFragment=function(_5700){
this._class=addClass(this._class,"DOMDocumentFragment");
this.DOMNode=DOMNode;
this.DOMNode(_5700);
this.nodeName="#document-fragment";
this.nodeType=DOMNode.DOCUMENT_FRAGMENT_NODE;
};
DOMDocumentFragment.prototype=new DOMNode;
DOMDocumentFragment.prototype.toString=function DOMDocumentFragment_toString(){
var xml="";
var _5702=this.getChildNodes().getLength();
for(intLoop=0;intLoop<_5702;intLoop++){
xml+=this.getChildNodes().item(intLoop).toString();
}
return xml;
};
DOMDocumentType=function(){
alert("DOMDocumentType.constructor(): Not Implemented");
};
DOMEntity=function(){
alert("DOMEntity.constructor(): Not Implemented");
};
DOMEntityReference=function(){
alert("DOMEntityReference.constructor(): Not Implemented");
};
DOMNotation=function(){
alert("DOMNotation.constructor(): Not Implemented");
};
Strings=new Object();
Strings.WHITESPACE=" \t\n\r";
Strings.QUOTES="\"'";
Strings.isEmpty=function Strings_isEmpty(strD){
return (strD===null)||(strD.length===0);
};
Strings.indexOfNonWhitespace=function Strings_indexOfNonWhitespace(strD,iB,iE){
if(Strings.isEmpty(strD)){
return -1;
}
iB=iB||0;
iE=iE||strD.length;
for(var i=iB;i<iE;i++){
if(Strings.WHITESPACE.indexOf(strD.charAt(i))==-1){
return i;
}
}
return -1;
};
Strings.lastIndexOfNonWhitespace=function Strings_lastIndexOfNonWhitespace(strD,iB,iE){
if(Strings.isEmpty(strD)){
return -1;
}
iB=iB||0;
iE=iE||strD.length;
for(var i=iE-1;i>=iB;i--){
if(Strings.WHITESPACE.indexOf(strD.charAt(i))==-1){
return i;
}
}
return -1;
};
Strings.indexOfWhitespace=function Strings_indexOfWhitespace(strD,iB,iE){
if(Strings.isEmpty(strD)){
return -1;
}
iB=iB||0;
iE=iE||strD.length;
for(var i=iB;i<iE;i++){
if(Strings.WHITESPACE.indexOf(strD.charAt(i))!=-1){
return i;
}
}
return -1;
};
Strings.replace=function Strings_replace(strD,iB,iE,strF,strR){
if(Strings.isEmpty(strD)){
return "";
}
iB=iB||0;
iE=iE||strD.length;
return strD.substring(iB,iE).split(strF).join(strR);
};
Strings.getLineNumber=function Strings_getLineNumber(strD,iP){
if(Strings.isEmpty(strD)){
return -1;
}
iP=iP||strD.length;
return strD.substring(0,iP).split("\n").length;
};
Strings.getColumnNumber=function Strings_getColumnNumber(strD,iP){
if(Strings.isEmpty(strD)){
return -1;
}
iP=iP||strD.length;
var arrD=strD.substring(0,iP).split("\n");
var _571a=arrD[arrD.length-1];
arrD.length--;
var _571b=arrD.join("\n").length;
return iP-_571b;
};
StringBuffer=function(){
this._a=[];
};
StringBuffer.prototype.append=function StringBuffer_append(d){
this._a[this._a.length]=d;
};
StringBuffer.prototype.toString=function StringBuffer_toString(){
return this._a.join("");
};
draw2d.XMLSerializer=function(){
alert("do not init this class. Use the static methods instead");
};
draw2d.XMLSerializer.toXML=function(obj,_4c92,_4c93){
if(_4c92==undefined){
_4c92="model";
}
_4c93=_4c93?_4c93:"";
var t=draw2d.XMLSerializer.getTypeName(obj);
var s=_4c93+"<"+_4c92+" type=\""+t+"\">";
switch(t){
case "int":
case "number":
case "boolean":
s+=obj;
break;
case "string":
s+=draw2d.XMLSerializer.xmlEncode(obj);
break;
case "date":
s+=obj.toLocaleString();
break;
case "Array":
case "array":
s+="\n";
var _4c96=_4c93+"   ";
for(var i=0;i<obj.length;i++){
s+=draw2d.XMLSerializer.toXML(obj[i],("element"),_4c96);
}
s+=_4c93;
break;
default:
if(obj!==null){
s+="\n";
if(obj instanceof draw2d.ArrayList){
obj.trimToSize();
}
var _4c98=obj.getPersistentAttributes();
var _4c96=_4c93+"   ";
for(var name in _4c98){
s+=draw2d.XMLSerializer.toXML(_4c98[name],name,_4c96);
}
s+=_4c93;
}
break;
}
s+="</"+_4c92+">\n";
return s;
};
draw2d.XMLSerializer.isSimpleVar=function(t){
switch(t){
case "int":
case "string":
case "String":
case "Number":
case "number":
case "Boolean":
case "boolean":
case "bool":
case "dateTime":
case "Date":
case "date":
case "float":
return true;
}
return false;
};
draw2d.XMLSerializer.getTypeName=function(obj){
if(obj===null){
return "undefined";
}
if(obj instanceof Array){
return "Array";
}
if(obj instanceof Date){
return "Date";
}
var t=typeof (obj);
if(t=="number"){
return (parseInt(obj).toString()==obj)?"int":"number";
}
if(draw2d.XMLSerializer.isSimpleVar(t)){
return t;
}
return obj.type.replace("@NAMESPACE"+"@","");
};
draw2d.XMLSerializer.xmlEncode=function(_4c9d){
var _4c9e=_4c9d;
var amp=/&/gi;
var gt=/>/gi;
var lt=/</gi;
var quot=/"/gi;
var apos=/'/gi;
var _4ca4="&#62;";
var _4ca5="&#38;#60;";
var _4ca6="&#38;#38;";
var _4ca7="&#34;";
var _4ca8="&#39;";
_4c9e=_4c9e.replace(amp,_4ca6);
_4c9e=_4c9e.replace(quot,_4ca7);
_4c9e=_4c9e.replace(lt,_4ca5);
_4c9e=_4c9e.replace(gt,_4ca4);
_4c9e=_4c9e.replace(apos,_4ca8);
return _4c9e;
};
draw2d.XMLDeserializer=function(){
alert("do not init this class. Use the static methods instead");
};
draw2d.XMLDeserializer.fromXML=function(node,_5566){
var _5567=""+node.getAttributes().getNamedItem("type").getNodeValue();
var value=node.getNodeValue();
switch(_5567){
case "int":
try{
return parseInt(""+node.getChildNodes().item(0).getNodeValue());
}
catch(e){
alert("Error:"+e+"\nDataType:"+_5567+"\nXML Node:"+node);
}
case "string":
case "String":
try{
if(node.getChildNodes().getLength()>0){
return ""+node.getChildNodes().item(0).getNodeValue();
}
return "";
}
catch(e){
alert("Error:"+e+"\nDataType:"+_5567+"\nXML Node:"+node);
}
case "Number":
case "number":
try{
return parseFloat(""+node.getChildNodes().item(0).getNodeValue());
}
catch(e){
alert("Error:"+e+"\nDataType:"+_5567+"\nXML Node:"+node);
}
case "Boolean":
case "boolean":
case "bool":
try{
return "true"==(""+node.getChildNodes().item(0).getNodeValue()).toLowerCase();
}
catch(e){
alert("Error:"+e+"\nDataType:"+_5567+"\nXML Node:"+node);
}
case "dateTime":
case "Date":
case "date":
try{
return new Date(""+node.getChildNodes().item(0).getNodeValue());
}
catch(e){
alert("Error:"+e+"\nDataType:"+_5567+"\nXML Node:"+node);
}
case "float":
try{
return parseFloat(""+node.getChildNodes().item(0).getNodeValue());
}
catch(e){
alert("Error:"+e+"\nDataType:"+_5567+"\nXML Node:"+node);
}
break;
}
_5567=_5567.replace("@NAMESPACE"+"@","");
var obj=eval("new "+_5567+"()");
if(_5566!=undefined&&obj.setModelParent!=undefined){
obj.setModelParent(_5566);
}
var _556a=node.getChildNodes();
for(var i=0;i<_556a.length;i++){
var child=_556a.item(i);
var _556d=child.getNodeName();
if(obj instanceof Array){
_556d=i;
}
obj[_556d]=draw2d.XMLDeserializer.fromXML(child,obj instanceof draw2d.AbstractObjectModel?obj:_5566);
}
return obj;
};
draw2d.EditPolicy=function(_5a1b){
this.policy=_5a1b;
};
draw2d.EditPolicy.DELETE="DELETE";
draw2d.EditPolicy.MOVE="MOVE";
draw2d.EditPolicy.CONNECT="CONNECT";
draw2d.EditPolicy.RESIZE="RESIZE";
draw2d.EditPolicy.prototype.type="draw2d.EditPolicy";
draw2d.EditPolicy.prototype.getPolicy=function(){
return this.policy;
};
draw2d.AbstractPalettePart=function(){
this.x=0;
this.y=0;
this.html=null;
};
draw2d.AbstractPalettePart.prototype.type="draw2d.AbstractPalettePart";
draw2d.AbstractPalettePart.prototype=new draw2d.Draggable();
draw2d.AbstractPalettePart.prototype.createHTMLElement=function(){
var item=document.createElement("div");
item.id=this.id;
item.style.position="absolute";
item.style.height="24px";
item.style.width="24px";
return item;
};
draw2d.AbstractPalettePart.prototype.setEnviroment=function(_4cee,_4cef){
this.palette=_4cef;
this.workflow=_4cee;
};
draw2d.AbstractPalettePart.prototype.getHTMLElement=function(){
if(this.html===null){
this.html=this.createHTMLElement();
draw2d.Draggable.call(this,this.html);
}
return this.html;
};
draw2d.AbstractPalettePart.prototype.onDrop=function(_4cf0,_4cf1){
var _4cf2=this.workflow.getScrollLeft();
var _4cf3=this.workflow.getScrollTop();
var _4cf4=this.workflow.getAbsoluteX();
var _4cf5=this.workflow.getAbsoluteY();
this.setPosition(this.x,this.y);
this.execute(_4cf0+_4cf2-_4cf4,_4cf1+_4cf3-_4cf5);
};
draw2d.AbstractPalettePart.prototype.execute=function(x,y){
alert("inerited class should override the method 'draw2d.AbstractPalettePart.prototype.execute'");
};
draw2d.AbstractPalettePart.prototype.setTooltip=function(_4cf8){
this.tooltip=_4cf8;
if(this.tooltip!==null){
this.html.title=this.tooltip;
}else{
this.html.title="";
}
};
draw2d.AbstractPalettePart.prototype.setDimension=function(w,h){
this.width=w;
this.height=h;
if(this.html===null){
return;
}
this.html.style.width=this.width+"px";
this.html.style.height=this.height+"px";
};
draw2d.AbstractPalettePart.prototype.setPosition=function(xPos,yPos){
this.x=Math.max(0,xPos);
this.y=Math.max(0,yPos);
if(this.html===null){
return;
}
this.html.style.left=this.x+"px";
this.html.style.top=this.y+"px";
this.html.style.cursor="move";
};
draw2d.AbstractPalettePart.prototype.getWidth=function(){
return this.width;
};
draw2d.AbstractPalettePart.prototype.getHeight=function(){
return this.height;
};
draw2d.AbstractPalettePart.prototype.getY=function(){
return this.y;
};
draw2d.AbstractPalettePart.prototype.getX=function(){
return this.x;
};
draw2d.AbstractPalettePart.prototype.getPosition=function(){
return new draw2d.Point(this.x,this.y);
};
draw2d.AbstractPalettePart.prototype.disableTextSelection=function(e){
if(typeof e.onselectstart!="undefined"){
e.onselectstart=function(){
return false;
};
}else{
if(typeof e.style.MozUserSelect!="undefined"){
e.style.MozUserSelect="none";
}
}
};
draw2d.ExternalPalette=function(_603e,divId){
this.html=document.getElementById(divId);
this.workflow=_603e;
this.parts=new draw2d.ArrayList();
};
draw2d.ExternalPalette.prototype.type="draw2d.ExternalPalette";
draw2d.ExternalPalette.prototype.getHTMLElement=function(){
return this.html;
};
draw2d.ExternalPalette.prototype.addPalettePart=function(part){
if(!(part instanceof draw2d.AbstractPalettePart)){
throw "parameter is not instanceof [draw2d.AbstractPalettePart]";
}
this.parts.add(part);
this.html.appendChild(part.getHTMLElement());
part.setEnviroment(this.workflow,this);
};
