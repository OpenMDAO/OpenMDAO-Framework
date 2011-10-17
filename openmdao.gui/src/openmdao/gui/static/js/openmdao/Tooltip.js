openmdao.Tooltip=function(msg){
    draw2d.Annotation.call(this,msg);
    this.setCanDrag(false);
    this.setFontSize(8);
    this.setSelectable(false);
    this.setDeleteable(false);
    this.setBorder(new draw2d.LineBorder(1));
};
openmdao.Tooltip.prototype=new draw2d.Annotation;
openmdao.Tooltip.prototype.type="Tooltip";
openmdao.Tooltip.prototype.createHTMLElement=function(){
    var item=draw2d.Annotation.prototype.createHTMLElement.call(this);
    item.style.margin="3px";
    item.style.padding="3px";
    item.style.paddingLeft="3px";
    item.style.background="rgb(255,255,128) no-repeat 3px 1px";
    item.style.zIndex=(draw2d.Figure.ZOrderIndex+1);
    return item;
};
