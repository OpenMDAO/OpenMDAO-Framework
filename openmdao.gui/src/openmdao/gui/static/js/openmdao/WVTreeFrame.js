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
        _edgesTree = jQuery('<div id="'+id+'_edges_tree">')
            .appendTo(_elem),
        _facesTree = jQuery('<div id="'+id+'_faces_tree">')
            .appendTo(_elem);

    /** determine if an attribute is set in a grpim attributes value */
    function isAttributeSet(attrs, attr) {
        return (attrs & attr) !== 0;
    }

    /** set the enabled/disabled state of a gprim attribute */
    function setAttribute(gprim_name, attr, enabled) {
        var gprim = wv.sceneGraph[gprim_name];

        console.log('setAttribute()', gprim_name, attr, enabled);
        console.log('BEFORE: isAttributeSet?', gprim.attrs, isAttributeSet(gprim.attrs, attr));

        if (enabled) {
            gprim.attrs |= attr;
        }
        else {
            gprim.attrs &= ~attr;
        }

        console.log('AFTER: isAttributeSet?', gprim.attrs, isAttributeSet(gprim.attrs, attr));

        wv.sceneUpd = 1;
    }


    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** if the scene graph has been updated, (re)build the Edges/Faces trees
        from the scenegraph. the attributes for each primitive in the tree
        are three GPtype-specific checkboxes for toggling visualization
        options for that primitive.
     */
    this.wvUpdateUI = function() {
        if (wv.sgUpdate !== 1) {
            // no updates to scene graph, tree update not required
            return;
        }

        if (wv.sceneGraph === undefined) {
            alert("wv.sceneGraph is undefined --- but we need it");
            return;
        }

        console.log('WVTreeFrame.wvUpdateUI() wv.sceneGraph:', wv.sceneGraph);
        console.log('WVTreeFrame.wvUpdateUI() wv.plotAttrs:', wv.plotAttrs);

        var edgesData,
            facesData,
            gprim,
            gprim_name,
            template = '<input type="checkbox" name="NAME" value="VALUE" CHECKED>';

        // initialize edges data with root node
        edgesData = [{
            data: 'Edges',
            attr: {
                viz: template
                        .replace('NAME',    'Edges')
                        .replace('VALUE',   wv.plotAttrs.ON)
                        .replace('CHECKED', ''),
                grd: template
                        .replace('NAME',    'Edges')
                        .replace('VALUE',   wv.plotAttrs.POINTS)
                        .replace('CHECKED', ''),
                ori: template
                        .replace('NAME',    'Edges')
                        .replace('VALUE',   wv.plotAttrs.ORIENTATION)
                        .replace('CHECKED', '')
            },
            children: []
        }];

        // initialize faces data with root node
        facesData = [{
            data: 'Faces',
            attr: {
                viz: template
                        .replace('NAME',    'Faces')
                        .replace('VALUE',   wv.plotAttrs.ON)
                        .replace('CHECKED', ''),
                grd: template
                        .replace('NAME',    'Faces')
                        .replace('VALUE',   wv.plotAttrs.LINES)
                        .replace('CHECKED', ''),
                trn: template
                        .replace('NAME',    'Faces')
                        .replace('VALUE',   wv.plotAttrs.TRANSPARENT)
                        .replace('CHECKED', '')
            },
            children: []
        }]

        // get data for edge and face gprims in sceneGraph
        for (gprim_name in wv.sceneGraph) {
            gprim = wv.sceneGraph[gprim_name];
            if (gprim.GPtype === 1) {
                edgesData[0].children.push({
                    data: gprim_name,
                    attr: {
                        viz: template
                                .replace('NAME',    gprim_name)
                                .replace('VALUE',   wv.plotAttrs.ON)
                                .replace('CHECKED', isAttributeSet(gprim.attrs, wv.plotAttrs.ON) ? 'checked="yes"' : ''),
                        grd: template
                                .replace('NAME',    gprim_name)
                                .replace('VALUE',   wv.plotAttrs.POINTS)
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
                facesData[0].children.push({
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
                console.log('ERROR: unknown GPtype for', gprim_name, '=', gprim.GPtype, gprim);
            }
        }

        console.log('wvUpdatWVTreeFrame.wvUpdateUI() edgesData:', edgesData);
        console.log('wvUpdatWVTreeFrame.wvUpdateUI() facesData:', facesData);

        // (re)create tree for the edge primitives
        _edgesTree.jstree({
            plugins: ["themes", "json_data", "grid"],
            themes : { "theme" : "openmdao" },
            json_data: {data: edgesData},
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
            _edgesTree.click(function(e) {
                if (jQuery(e.target).is('input:checkbox')) {
                    if (e.target.name === 'Edges') {
                        // FIXME: this is hacky and doesn't update all the checkboxes
                        for (var i in edgesData[0].children) {
                            setAttribute(edgesData[0].children[i].data,
                                Number(e.target.value), e.target.checked);
                        }
                    }
                    else {
                        setAttribute(e.target.name, Number(e.target.value), e.target.checked);
                    }
                }
            });
        });

        // (re)create tree for the face primitives
        _facesTree.jstree({
            plugins: ["themes", "json_data", "grid"],
            themes : { "theme" : "openmdao" },
            json_data: {data: facesData},
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
            _facesTree.click(function(e) {
                if (jQuery(e.target).is('input:checkbox')) {
                    console.log(e.target, e.target.name);
                    if (e.target.name === 'Faces') {
                        // FIXME: this is hacky and doesn't update all the checkboxes
                        for (var i in facesData[0].children) {
                            setAttribute(facesData[0].children[i].data,
                                Number(e.target.value), e.target.checked);
                        }
                    }
                    else {
                        setAttribute(e.target.name, Number(e.target.value), e.target.checked);
                    }
                }
            });
        });
    }

}