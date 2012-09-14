openmdao.ContextMenuConnection=function(){
    draw2d.Connection.call(this);
    this.sourcePort=null;
    this.targetPort=null;
    this.lineSegments=[];
    this.setColor(new draw2d.Color(0,0,0));
    this.setLineWidth(1);
};
openmdao.ContextMenuConnection.prototype=new draw2d.Connection();
openmdao.ContextMenuConnection.prototype.getContextMenu=function(){
    var menu=new draw2d.Menu();
    var oThis=this;
    menu.appendMenuItem(new draw2d.MenuItem("NULL Router",null,function(){
            oThis.setRouter(null);
        })
    );
    menu.appendMenuItem(
        new draw2d.MenuItem("Manhatten Router",null,function(){
            oThis.setRouter(new draw2d.ManhattanConnectionRouter());
        })
    );
    menu.appendMenuItem(
        new draw2d.MenuItem("Bezier Router",null,function(){
            oThis.setRouter(new draw2d.BezierConnectionRouter());
        })
    );
    menu.appendMenuItem(
        new draw2d.MenuItem("Fan Router",null,function(){
            oThis.setRouter(new draw2d.FanConnectionRouter());
        })
    );
    return menu;
};
openmdao.ContextMenuConnection.prototype.addlabel=function(text){
    var label=new draw2d.Label(text);
    label.setBackgroundColor(new draw2d.Color(230,230,250));
    label.setBorder(new draw2d.LineBorder(1));
    this.addFigure(label,new draw2d.ManhattanMidpointLocator(this));
}
