draw2d.End=function(){
    draw2d.ImageFigure.call(this,"/static/images/"+this.type+".png");
    this.inputPort=null;
    this.setDimension(50,50);
};
draw2d.End.prototype=new draw2d.ImageFigure();
draw2d.End.prototype.type="End";
draw2d.End.prototype.setWorkflow=function(_5b0c){
    draw2d.ImageFigure.prototype.setWorkflow.call(this,_5b0c);
    if(_5b0c!==null&&this.inputPort===null){
        this.inputPort=new draw2d.MyInputPort();
        this.inputPort.setName("input");
        this.inputPort.setHideIfConnected(true);
        this.inputPort.setWorkflow(_5b0c);
        this.inputPort.setBackgroundColor(new draw2d.Color(115,115,245));
        this.addPort(this.inputPort,0,this.height/2);
    }
};
