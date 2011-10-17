openmdao.Toolbar=function(){
    draw2d.ToolPalette.call(this,"Schematic Tools");
    this.tool1=new openmdao.ToolShowGrid(this);
    this.tool1.setPosition(10,10);
    this.addChild(this.tool1);
    this.setDimension(300,60);
    this.setBackgroundColor(new draw2d.Color(200,200,200))
    if(this.hasTitleBar()) {
       this.titlebar.style.backgroundColor="gray";
    }
};

openmdao.Toolbar.prototype=new draw2d.ToolPalette();
openmdao.Toolbar.prototype.type="Toolbar";
openmdao.Toolbar.prototype.onSetDocumentDirty=function(){};
