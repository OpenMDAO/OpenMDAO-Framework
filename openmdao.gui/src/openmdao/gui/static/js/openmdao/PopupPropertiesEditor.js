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
openmdao.PopupPropertiesEditor = function(model,path) {
    /***********************************************************************
     *  private
     ***********************************************************************/
    var tableID = 'PopupPropertiesEditor-'+path.replace(/\./g,'-'),
        table = jQuery('#'+tableID)
    if (table.length === 0)
        table = jQuery('<div><table id='+tableID+'></table></div>')
    table.dialog({
        'modal': false,
        'title': 'Properties: '+path,
        'close': function(ev, ui) { jQuery(tableID).remove() },
        width: 200, 
        height: 300 
    })
    new openmdao.PropertiesEditor(tableID,model).editObject(path)
}