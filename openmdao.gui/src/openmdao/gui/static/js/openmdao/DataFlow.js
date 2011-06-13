/* 
Copyright (c) 2010. All rights reserved.
LICENSE: NASA Open Source License
*/

var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

/**
 * 
 * @version 0.0.0
 * @constructor
 */
openmdao.DataFlow = function(id,model) {
    /***********************************************************************
     *  private (available only to privileged methods) 
     ***********************************************************************/
     
    var self = this,
        elm = jQuery("#"+id).width(screen.width).height(screen.height),
        dataflow  = new draw2d.Workflow(id)
        
    // set background image
    dataflow.setBackgroundImage( "/static/images/grid_10.png", true)
    
    // make the dataflow pane droppable
    elm.droppable ({
        accept: '.objtype',
        drop: function(ev,ui) { 
            // get the object that was dropped and where it was dropped
            var droppedObject = jQuery(ui.draggable).clone()
            var off = elm.parent().offset()
            x = Math.round(ui.offset.left - off.left)
            y = Math.round(ui.offset.top - off.top)
            // get the type name and path
            var typename = droppedObject.text()
            var typepath = droppedObject.attr("path")
            openmdao.Util.promptForName(function(name) { 
                model.addComponent(typepath,name,x,y)
            })
        }
    });

    /** update dataflow by recreating figures from JSON model data
     *  TODO: iterating through & updating existing figures would be faster
     */
    function updateFigures(json) {
        dataflow.clear();        
        var x = 50, y = 50;
        debug.info("dataflow json=",json)
        jQuery.each(json,function(idx,name) {
            //if (val) {
                // FIXME: just getting a name atm
                // also want type, inputs/ouputs
                var typepath = name 
                if (typepath !== undefined) {
                    var tokens = typepath.split('.'),
                        typename = tokens[tokens.length-1]
                    
                    var newObj = new draw2d.Component(name,typename,typepath)
                    //newObj.setTitle(name);
                    dataflow.addFigure(newObj,x,y)
                    
                    // TODO: get x & y from model... 
                    x = x+75;
                    y = y+75;
                    
                    // try to load a custom image for this component
                    // TODO: should look in the model folder for these
                    var img = new Image();
                    img.src = "/static/images/"+typename+".png";
                    jQuery(img).ready(function(){
                        if (img.width > 0) {
                            img.style.width = "50px";
                            img.style.height = "50px";
                            newObj.setImage(img.src);
                            newObj.setDimension(img.width,img.height)
                        }
                    })
                }
            //}
        })
    }
    
    /** update the schematic, with data from the model */
    function update() {
        model.getWorkflow(updateFigures)
    }
    
    // ask model for an update whenever something changes
    model.addListener(update)

    /***********************************************************************
     *  privileged (can access privates, accessible to public and outside) 
     ***********************************************************************/
        
}
