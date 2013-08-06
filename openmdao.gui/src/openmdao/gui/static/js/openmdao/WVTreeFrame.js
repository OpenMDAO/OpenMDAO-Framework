/*
 * simple key-stroke & mouse driven UI for WV
 *
 * Notes: wv.sceneUpd should be set to 1 if the scene should rerendered
 *        wv.sgUpdate will be set to 1 if the sceneGraph has changed
 *                    should be set back to 0 after UI responds
 */

var openmdao = (typeof openmdao === "undefined" || !openmdao) ? {} : openmdao ;

openmdao.WVTreeFrame = function(id, wv) {
    /***********************************************************************
     *  private
     ***********************************************************************/

    // initialize private variables
    var _self = this,
        _elem = jQuery('#'+id),
        _update = false;

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
        if (wv.sceneGraph === undefined) {
            alert("wv.sceneGraph is undefined!");
            return;
        }

        if (!_update && wv.sgUpdate !== 1 && wv.sceneUpd !== 1) {
            // no updates, tree update not required
            return;
        }
        _update = false;

        // TODO: if it's just a tree _update (due to toggled checkbox),
        //       we should just update the checkboxes and not rebuild the tree.
        //       should separate the checkbox logic from tree building entirely!

        console.log('WVTreeFrame.wvUpdateUI() wv BEFORE:', wv);

        var edgesData,
            edgesTree,
            facesData,
            facesTree,
            gprim,
            gprim_name,
            template = '<input type="checkbox" name="NAME" value="VALUE" CHECKED>';

        // Grab paths of currently open nodes.
        var openNodes = [];
        _elem.find('li.jstree-open').each(function () {
            openNodes.push(this.getAttribute('nom'));
        });

        // initialize edges data with root node
        edgesData = [{
            data: 'Edges',
            attr: {
                nom: 'Edges',
                viz: template
                        .replace('NAME',    'Edges')
                        .replace('VALUE',   wv.plotAttrs.ON),
                grd: template
                        .replace('NAME',    'Edges')
                        .replace('VALUE',   wv.plotAttrs.POINTS),
                ori: template
                        .replace('NAME',    'Edges')
                        .replace('VALUE',   wv.plotAttrs.ORIENTATION)
            },
            state: openNodes.indexOf('Edges') >= 0 ? 'open' : 'closed',
            children: []
        }];

        // initialize faces data with root node
        facesData = [{
            data: 'Faces',
            attr: {
                nom: 'Faces',
                viz: template
                        .replace('NAME',    'Faces')
                        .replace('VALUE',   wv.plotAttrs.ON),
                grd: template
                        .replace('NAME',    'Faces')
                        .replace('VALUE',   wv.plotAttrs.LINES),
                trn: template
                        .replace('NAME',    'Faces')
                        .replace('VALUE',   wv.plotAttrs.TRANSPARENT)
            },
            state: openNodes.indexOf('Faces') >= 0 ? 'open' : 'closed',
            children: []
        }]

        var edges_viz_cnt = 0,
            edges_grd_cnt = 0,
            edges_ori_cnt = 0;
            faces_viz_cnt = 0,
            faces_grd_cnt = 0,
            faces_trn_cnt = 0;

        var is_viz, is_grd, is_ori, is_trn;

        // get data for edge and face gprims in sceneGraph
        for (gprim_name in wv.sceneGraph) {
            gprim = wv.sceneGraph[gprim_name];
            if (gprim.GPtype === 1) {
                is_viz = isAttributeSet(gprim.attrs, wv.plotAttrs.ON);
                is_grd = isAttributeSet(gprim.attrs, wv.plotAttrs.POINTS);
                is_ori = isAttributeSet(gprim.attrs, wv.plotAttrs.ORIENTATION);

                if (is_viz) edges_viz_cnt += 1;
                if (is_grd) edges_grd_cnt += 1;
                if (is_ori) edges_ori_cnt += 1;

                edgesData[0].children.push({
                    data: gprim_name,
                    attr: {
                        nom: gprim_name,
                        viz: template
                                .replace('NAME',    gprim_name)
                                .replace('VALUE',   wv.plotAttrs.ON)
                                .replace('CHECKED', is_viz ? 'checked="yes"' : ''),
                        grd: template
                                .replace('NAME',    gprim_name)
                                .replace('VALUE',   wv.plotAttrs.POINTS)
                                .replace('CHECKED', is_grd ? 'checked="yes"' : ''),
                        ori: template
                                .replace('NAME',    gprim_name)
                                .replace('VALUE',   wv.plotAttrs.ORIENTATION)
                                .replace('CHECKED', is_ori ? 'checked="yes"' : ''),
                    },
                    state: openNodes.indexOf(gprim_name) >= 0 ? 'open' : undefined,
                    children: []
                })
            }
            else if (gprim.GPtype === 2) {
                is_viz = isAttributeSet(gprim.attrs, wv.plotAttrs.ON);
                is_grd = isAttributeSet(gprim.attrs, wv.plotAttrs.LINES);
                is_trn = isAttributeSet(gprim.attrs, wv.plotAttrs.TRANSPARENT);

                if (is_viz) faces_viz_cnt += 1;
                if (is_grd) faces_grd_cnt += 1;
                if (is_trn) faces_trn_cnt += 1;

                facesData[0].children.push({
                    data: gprim_name,
                    attr: {
                        nom: gprim_name,
                        viz: template
                                .replace('NAME',    gprim_name)
                                .replace('VALUE',   wv.plotAttrs.ON)
                                .replace('CHECKED', is_viz ? 'checked="yes"' : ''),
                        grd: template
                                .replace('NAME',    gprim_name)
                                .replace('VALUE',   wv.plotAttrs.LINES)
                                .replace('CHECKED', is_grd ? 'checked="yes"' : ''),
                        trn: template
                                .replace('NAME',    gprim_name)
                                .replace('VALUE',   wv.plotAttrs.TRANSPARENT)
                                .replace('CHECKED', is_trn ? 'checked="yes"' : ''),
                    },
                    state: openNodes.indexOf(gprim_name) >= 0 ? 'open' : undefined,
                    children: []
                })
            }
            else {
                console.log('ERROR: unknown GPtype for', gprim_name, '=', gprim.GPtype, gprim);
            }
        }

        // set checkboxes for edges root node (FUGLY)
        if (edges_viz_cnt === facesData[0].children.length) {
            edgesData[0].attr.viz = edgesData[0].attr.viz.replace('CHECKED', 'checked="yes"');
        }
        else {
            edgesData[0].attr.viz = edgesData[0].attr.viz.replace('CHECKED', '');            
        }
        if (edges_grd_cnt === facesData[0].children.length) {
            edgesData[0].attr.grd = edgesData[0].attr.grd.replace('CHECKED', 'checked="yes"');
        }
        else {
            edgesData[0].attr.grd = edgesData[0].attr.grd.replace('CHECKED', '');            
        }
        if (edges_ori_cnt === facesData[0].children.length) {
            edgesData[0].attr.ori = edgesData[0].attr.ori.replace('CHECKED', 'checked="yes"');
        }
        else {
            edgesData[0].attr.ori = edgesData[0].attr.ori.replace('CHECKED', '');            
        }
        console.log('WVTreeFrame.wvUpdateUI() edgesData:', edgesData);

        // set checkboxes for faces root node
        if (faces_viz_cnt === facesData[0].children.length) {
            facesData[0].attr.viz = facesData[0].attr.viz.replace('CHECKED', 'checked="yes"');
        }
        else {
            facesData[0].attr.viz = facesData[0].attr.viz.replace('CHECKED', '');            
        }
        if (faces_grd_cnt === facesData[0].children.length) {
            facesData[0].attr.grd = facesData[0].attr.grd.replace('CHECKED', 'checked="yes"');
        }
        else {
            facesData[0].attr.grd = facesData[0].attr.grd.replace('CHECKED', '');            
        }
        if (faces_trn_cnt === facesData[0].children.length) {
            facesData[0].attr.trn = facesData[0].attr.trn.replace('CHECKED', 'checked="yes"');
        }
        else {
            facesData[0].attr.trn = facesData[0].attr.trn.replace('CHECKED', '');            
        }
        console.log('WVTreeFrame.wvUpdateUI() facesData:', facesData);

        // remove any existing trees (with their event handlers, etc.)
        _elem.empty();

        //create new tree elements
        edgesTree = jQuery('<div id="'+id+'_edges_tree">')
            .appendTo(_elem),
        facesTree = jQuery('<div id="'+id+'_faces_tree">')
            .appendTo(_elem);

        // (re)create tree for the edge primitives
        edgesTree.jstree({
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
            edgesTree.click(function(e) {
                if (jQuery(e.target).is('input:checkbox')) {
                    if (e.target.name === 'Edges') {
                        for (var i in edgesData[0].children) {
                            setAttribute(edgesData[0].children[i].data,
                                Number(e.target.value), e.target.checked);
                        }
                    }
                    else {
                        setAttribute(e.target.name, Number(e.target.value), e.target.checked);
                    }
                    _update = true;
                }
            });
        });

        // (re)create tree for the face primitives
        facesTree.jstree({
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
            facesTree.click(function(e) {
                if (jQuery(e.target).is('input:checkbox')) {
                    console.log('check:', e.target.name, e.target.value, e.target.checked);
                    if (e.target.name === 'Faces') {
                        for (var i in facesData[0].children) {
                            setAttribute(facesData[0].children[i].data,
                                Number(e.target.value), e.target.checked);
                        }
                    }
                    else {
                        setAttribute(e.target.name, Number(e.target.value), e.target.checked);
                    }
                    _update = true;
                }
            });
        });

        console.log('WVTreeFrame.wvUpdateUI() wv AFTER:', wv);
    }

}