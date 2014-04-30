openmdao.ToolShowGrid=function(toolbar){
    draw2d.ToggleButton.call(this,toolbar);
};
openmdao.ToolShowGrid.prototype=new draw2d.ToggleButton();
openmdao.ToolShowGrid.prototype.type="ToolShowGrid";
openmdao.ToolShowGrid.prototype.getImageURL=function() {
    return "/static/images/ToolShowGrid.png";
}
openmdao.ToolShowGrid.prototype.execute=function(){
    if(this.isDown()){
        this.getToolPalette().getWorkflow().setBackgroundImage("/static/images/grid_10.png",true);
        
    }else{
        this.getToolPalette().getWorkflow().setBackgroundImage(null,false);
    }
};
