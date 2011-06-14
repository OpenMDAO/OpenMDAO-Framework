openmdao.ContextMenuConnection=function(){
    draw2d.Connection.call(this);
    this.sourcePort=null;
    this.targetPort=null;
    this.lineSegments=[];
    this.setColor(new draw2d.Color(128,128,255));
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
