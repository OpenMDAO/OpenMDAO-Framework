draw2d.Component=function(name,type,path){
    // the default image will be a letter corresponding to the object type
    imgURL = "/static/images/alphabet/"+type.charAt(0).toUpperCase()+".png";
    draw2d.ImageFigure.call(this,imgURL);
    
    this.name = name
    this.type = type
    this.path = path
    
    // default size & border
    this.setDimension(50,50);
    this.setBorder(new draw2d.LineBorder(1))
    
    // TODO: inputs & outputs based on type
    this.inputPort=null;
    this.outputPort=null;
};
draw2d.Component.prototype=new draw2d.ImageFigure();
draw2d.Component.prototype.setWorkflow=function(arg){
    draw2d.ImageFigure.prototype.setWorkflow.call(this,arg);
    if(arg!==null&&this.inputPort===null){
        this.inputPort=new draw2d.MyInputPort();
        this.inputPort.setName("input");
        this.inputPort.setHideIfConnected(true);
        this.inputPort.setWorkflow(arg);
        this.inputPort.setBackgroundColor(new draw2d.Color(115,115,245));
        this.addPort(this.inputPort,0,this.height/2);
    }
    if(arg!==null&&this.outputPort===null){
        this.outputPort=new draw2d.MyOutputPort();
        this.outputPort.setMaxFanOut(5);
        this.outputPort.setWorkflow(arg);
        this.outputPort.setHideIfConnected(true);
        this.outputPort.setName("output");
        this.outputPort.setBackgroundColor(new draw2d.Color(245,115,115));
        this.addPort(this.outputPort,this.width,this.height/2);
    }
};


