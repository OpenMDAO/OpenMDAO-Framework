/*
 * simple key-stroke & mouse driven UI for WV
 *
 * Notes: wv.sceneUpd should be set to 1 if the scene should rerendered
 *        wv.sgUpdate will be set to 1 if the sceneGraph has changed
 *                    should be set back to 0 after UI responds
 */

var openmdao = (typeof openmdao === "undefined" || !openmdao) ? {} : openmdao ;

openmdao.WVTreeFrame = function(wv, id) {
    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var _self = this,
        _elem = jQuery('#'+id),
        _edges_tree = jQuery('<div>')
            .appendTo(_elem),
        _faces_tree =  jQuery('<div>')
            .appendTo(_elem);

    function isAttributeSet(attrs, attr) {
        return (attrs & attr) !== 0;
    }

    function setAttribute(gprim_name, attr, enabled) {
        var gprim = wv.sceneGraph[gprim_name];

        if (enabled) {
            gprim.attrs |= attr;
        }
        else {
            gprim.attrs &= ~attr;
        }

        wv.sceneUpd = 1;
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    this.wvUpdateUI = function() {
        // if the scene graph has been updated, (re-)build the Tree
        if (wv.sgUpdate == 1) {

            if (wv.sceneGraph === undefined) {
                alert("wv.sceneGraph is undefined --- but we need it");
            }

            // create tree data structures for the edge and face primitives
            // in the scenegraph. the attributes for each primitive in the tree
            // are three GPtype-specific checkboxes for toggling visualization
            // options for that primitive
            var gprim,
                gprim_name,
                edges = [],
                faces = [],
                template = '<input type="checkbox" name="NAME" value="VALUE" CHECKED>';

            for (gprim_name in wv.sceneGraph) {
                gprim = wv.sceneGraph[gprim_name];
                if (gprim.GPtype === 1) {
                    edges.push({
                        data: gprim_name,
                        attr: {
                            viz: template
                                    .replace('NAME',    gprim_name)
                                    .replace('VALUE',   'viz')
                                    .replace('CHECKED', isAttributeSet(gprim.attrs, wv.plotAttrs.ON) ? 'checked="yes"' : ''),
                            grd: template
                                    .replace('NAME',    gprim_name)
                                    .replace('VALUE',   wv.plotAttrs.LINES)
                                    .replace('CHECKED', isAttributeSet(gprim.attrs, wv.plotAttrs.LINES) ? 'checked="yes"' : ''),
                            ori: template
                                    .replace('NAME',    gprim_name)
                                    .replace('VALUE',   wv.plotAttrs.ORIENTATION)
                                    .replace('CHECKED', isAttributeSet(gprim.attrs, wv.plotAttrs.ORIENTATION) ? 'checked="yes"' : ''),
                        },
                        children: []
                    })
                }
                else if (gprim.GPtype === 2) {
                    faces.push({
                        data: gprim_name,
                        attr: {
                            viz: template
                                    .replace('NAME',    gprim_name)
                                    .replace('VALUE',   wv.plotAttrs.ON)
                                    .replace('CHECKED', isAttributeSet(gprim.attrs, wv.plotAttrs.ON) ? 'checked="yes"' : ''),
                            grd: template
                                    .replace('NAME',    gprim_name)
                                    .replace('VALUE',   wv.plotAttrs.LINES)
                                    .replace('CHECKED', isAttributeSet(gprim.attrs, wv.plotAttrs.LINES) ? 'checked="yes"' : ''),
                            trn: template
                                    .replace('NAME',    gprim_name)
                                    .replace('VALUE',   wv.plotAttrs.TRANSPARENT)
                                    .replace('CHECKED', isAttributeSet(gprim.attrs, wv.plotAttrs.TRANSPARENT) ? 'checked="yes"' : ''),
                        },
                        children: []
                    })
                }
                else {
                    console.log('ERROR: unknown GPtype for', gprim_name, '=', gprim.GPtype);
                }
            }

            // create the tree for the edge primitives
            _edges_tree.jstree({
                plugins: ["themes", "json_data", "grid"],
                themes : { "theme" : "openmdao" },
                json_data: {data: [{
                    data: 'Edges',
                    attr: {
                        viz: template
                                .replace('NAME',    'Edges')
                                .replace('VALUE',   'viz')
                                .replace('CHECKED', 'no'),
                        grd: template
                                .replace('NAME',    'Edges')
                                .replace('VALUE',   'grd')
                                .replace('CHECKED', 'no'),
                        ori: template
                                .replace('NAME',    'Edges')
                                .replace('VALUE',   'ori')
                                .replace('CHECKED', 'no')
                    },
                    children: edges
                }]},
                grid: {
                    columns: [
                        {width: 160, header: "Edges", title:"_DATA_"},
                        {cellClass: "col1", value: "viz", width: 30, header: "Viz", title: "Visible"},
                        {cellClass: "col2", value: "grd", width: 30, header: "Grd", title: "Grid"},
                        {cellClass: "col3", value: "ori", width: 30, header: "Ori", title: "Orientation"}
                    ],
                    resizable:true
                }
            })
            .bind("loaded.jstree", function(event, data) {
                _edges_tree.click(function(e) {
                    if (jQuery(e.target).is('input:checkbox')) {
                        setAttribute(e.target.name, Number(e.target.value), e.target.checked);
                    }
                });
            });

            // create the tree for the edge primitives
            _faces_tree.jstree({
                plugins: ["themes", "json_data", "grid"],
                themes : { "theme" : "openmdao" },
                json_data: {data: [{
                    data: 'Faces',
                    attr: {
                        viz: template
                                .replace('NAME',    'Faces')
                                .replace('VALUE',   'viz')
                                .replace('CHECKED', 'no'),
                        grd: template
                                .replace('NAME',    'Faces')
                                .replace('VALUE',   'grd')
                                .replace('CHECKED', 'no'),
                        trn: template
                                .replace('NAME',    'Faces')
                                .replace('VALUE',   'trn')
                                .replace('CHECKED', 'no')
                    },
                    children: faces
                }]},
                grid: {
                    columns: [
                        {width: 160, header: "Faces", title:"_DATA_"},
                        {cellClass: "col1", value: "viz", width: 30, header: "Viz", title: "Visible"},
                        {cellClass: "col2", value: "grd", width: 30, header: "Grd", title: "Grid"},
                        {cellClass: "col3", value: "trn", width: 30, header: "Trn", title: "Transparent"}
                    ],
                    resizable:true
                }
            })
            .bind("loaded.jstree", function(event, data) {
                _faces_tree.click(function(e) {
                    if (jQuery(e.target).is('input:checkbox')) {
                        setAttribute(e.target.name, Number(e.target.value), e.target.checked);
                    }
                });
            });
        }
    }
}