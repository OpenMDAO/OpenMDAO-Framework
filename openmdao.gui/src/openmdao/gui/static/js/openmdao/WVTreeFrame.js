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
        _elem = jQuery('#'+id);

    function isControlActive(control) {
        return control.hasClass("active");
    }

    function isAttributeSet(setValue, currentValue) {
        return (currentValue & setValue) >= 1;
    }

    function setAttribute(attributes, mask, value) {
        return (attributes & (~mask)) | (value & mask);
    }

    function getNode(id) {
        return jQuery.jstree._reference("#tree")._get_node("#" + id);
    }

    function getNodeChildren(node) {
        return jQuery.jstree._reference("#tree")._get_children(node);
    }

    function gprimToId(gprim) {
        return gprim.replace(/ /g, "_");
    }

    function setNodeData(node, dataKey, data) {
        node.data(dataKey, data);
    }

    function getNodeData(node, dataKey) {
        return node.data(dataKey);
    }

    function getNodeControl(node, controlName) {
        return jQuery("." + controlName, node).first();
    }

    function setNodeControl(node, control, activate) {
        if (activate) {
            control.addClass("active");
        }
        else {
            control.removeClass("active");
        }
    }

    function getDisplayControl(display, isActive) {
        isActive = isActive ? "active" : "";
        return " \
                <button type='button' class='btn btn-primary btn-small " + display + " " + isActive + "'> \
                    <span>" + display + "</span> \
                </button>";
    }

    function getDisplayControls(attrs) {
        return jQuery("<div class='btn-group' data-toggle='buttons-checkbox'></div>")
                    .append(getDisplayControl("viz", isAttributeSet(attrs, wv.plotAttrs.ON)),
                            getDisplayControl("grd", isAttributeSet(attrs, wv.plotAttrs.LINES | wv.plotAttrs.POINTS)),
                            getDisplayControl("trn", isAttributeSet(attrs, wv.plotAttrs.TRANSPARENT)),
                            getDisplayControl("ori", isAttributeSet(attrs, wv.plotAttrs.ORIENTATION)));
    }

    function handleControlClick(attribute, mask) {
        console.log('handleControlClick, attribute=', attribute, 'mask=', mask);

        function updateTree(root, flag) {
            var control = getNodeControl(root, attribute);
            var activateControl = !isControlActive(control);
            var children = getNodeChildren(root);

            var data = undefined;
            var dataKey = "gprim";

            if (children.length > 0) {
                dataKey = "attrs";
                data = getNodeData(root, dataKey);

                data = setAttribute(getNodeData(root, dataKey), mask, flag);
                setNodeData(root, dataKey, data);

                children.each(function(index, child) {
                    child = jQuery(child);
                    setNodeControl(child, getNodeControl(child, attribute), activateControl);
                    updateTree(child, flag);
                });
            }
            else {
                data = getNodeData(root, dataKey);
                data.attrs = setAttribute(data.attrs, mask, flag);
                setNodeData(root, dataKey, data);
            }
        }

        return function(e) {
            var root = jQuery(this).is("button") ? jQuery(this).parent().parent() : jQuery(this).parent();

            console.log(this, 'button?', jQuery(this).is("button"), 'root:', root, 'data:', root.data());

            var data = getNodeData(root, "gprim");

            data =  data ? data.attrs : getNodeData(root, "attrs");

            var flag = (data & mask) > 0 ? 0 : mask;

            console.log('handleControlClick, attribute=', attribute, 'mask=', mask, 'data=', data, 'flag=', flag);

            updateTree(root, flag);
            wv.sceneUpd = 1;
        };
    }


    /***********************************************************************
     *  privileged
     ***********************************************************************/

    this.wvUpdateUI = function() {
        // if the tree has not been created but the scene graph (possibly) exists...
        // if the scene graph and Parameters have been updated, (re-)build the Tree
        if (wv.sgUpdate == 1) {
            console.log('WVTreeFrame.wvUpdateUI, wv.sgUpdate:', wv.sgUpdate);

            if (wv.sceneGraph === undefined) {
                alert("wv.sceneGraph is undefined --- but we need it");
            }

            var nodes = [[],[]];
            var parentIndex = 0;
            var node = undefined;

            console.log('wv:', wv);

            if (_elem.find("#tree").length === 0) {
                for(var gprim in wv.sceneGraph) {
                    parentIndex = (wv.sceneGraph[gprim].GPtype === 1) ? 0 : 1;

                    node = jQuery("<li id='" + gprimToId(gprim) + "'><a href='#'>" + gprim + "</a></li>");
                    node.data("gprim", wv.sceneGraph[gprim]);
                    node.append(getDisplayControls(wv.sceneGraph[gprim].attrs));

                    nodes[parentIndex].push(node);
                }

                console.log('nodes:', nodes);

                var edges = jQuery("<ul></ul>");
                var faces = jQuery("<ul></ul>");
                var tree  = jQuery("<ul></ul>");

                edges.append(nodes[0]);
                faces.append(nodes[1]);

                edges = jQuery("<li id='Edges'><a href='#'>Edges</a></li>").append(edges);
                faces = jQuery("<li id='Faces'><a href='#'>Faces</li>").append(faces);

                jQuery("ul", edges).before(getDisplayControls(1));
                jQuery("ul", faces).before(getDisplayControls(9));

                edges.data("attrs", 1);
                faces.data("attrs", 9);

                tree.append(edges, faces);

                var treeDiv = jQuery("<div id='tree'></div>")
                    .appendTo(_elem);

                treeDiv.jstree({
                    "plugins" : ["html_data", "themes", "ui", "sort"],

                    "themes" : {
                        "theme" : "openmdao",
                    },

                    "html_data" : {
                        "data" : tree.html(),
                    },
                })
                .bind("loaded.jstree", function(event, data) {
                    treeDiv.find(".viz").click(handleControlClick("viz", wv.plotAttrs.ON));
                    treeDiv.find(".grd").click(handleControlClick("grd", wv.plotAttrs.LINES | wv.plotAttrs.POINTS));
                    treeDiv.find(".trn").click(handleControlClick("trn", wv.plotAttrs.TRANSPARENT));
                    treeDiv.find(".ori").click(handleControlClick("ori", wv.plotAttrs.ORIENTATION));
                });
            }
        }
    };
}