
var openmdao = (typeof openmdao === "undefined" || !openmdao ) ? {} : openmdao ;

openmdao.Menu = function(id, json) {
    /***********************************************************************
     *  private
     ***********************************************************************/
    var self = this,
        elm = jQuery("#"+id),
        _onclick = [];

    /** build menus from JSON data structure */
    function buildMenus(menus) {
        // generate HTML for the menus
        var i = 0,
            html = "<ul>";
        for (i = 0; i < menus.length; i++) {
            html += getMenuHTML(menus[i]);
        }
        html += "</ul>";
        elm.html(html);

        // Add indicators and hovers to submenu parents.
        // The slides look pretty, but cause problems for Selenium,
        // so they're disabled when testing.
        elm.find("li").each(function() {
            var header = jQuery(this).children(":first"),
                menu = jQuery(this).find("ul"),
                showMenu = function() {
                    if (typeof openmdao_test_mode === "undefined") {
                        menu.stop(true, true).slideDown();
                    }
                },
                hideMenu = function() {
                    if (typeof openmdao_test_mode === "undefined") {
                        menu.stop(true, true).slideUp();
                    }
                },
                settings = {
                    timeout: 500,
                    over: showMenu,
                    out: hideMenu
                };

            // When testing, toggle this menu and hide all the others on click.
            header.click(function() {
                if (typeof openmdao_test_mode !== "undefined") {
                    menu.toggle();
                    header.parent().siblings().find("ul").hide();
                }
            });

            if (menu.length > 0) {
                jQuery("<span>").text("^").appendTo(header);
                jQuery(this).hoverIntent( settings );
                menu.find("li").click(function() { menu.toggle(); });
            }
        });
    }

    /** recursively build HTML for JSON nested menu structure */
    function getMenuHTML(menu) {
        var menuHTML = '<li><a ';
        if (menu.hasOwnProperty('url')) {
            menuHTML += 'href="'+menu.url+'" ';
        }
        if (menu.hasOwnProperty('onclick')) {
            menuHTML += 'onclick="'+menu.onclick+'" ';
        }
        if (menu.hasOwnProperty('id')) {
            menuHTML += 'id="'+menu.id+'" ';
        }
        menuHTML += '>'+menu.text+'</a>';

        if (menu.hasOwnProperty('items')) {
            var i = 0;
            menuHTML += "<ul>";
            for (i = 0; i < menu.items.length; i++) {
                menuHTML += getMenuHTML(menu.items[i]);
            }
            menuHTML += "</ul>";
        }

        menuHTML += '</li>';
        return menuHTML;
    }

    /** Disable menu item given its ID */
    function disableById(id) {
        var button = jQuery('#'+id),
            binding = button.attr('onclick');
        if (binding) {  // We can get multiple disables in a row.
            _onclick[id] = binding;
            button.attr('onclick', null);
            button.addClass('omg-disabled');
        }
    }

    /** Enable menu item given its ID */
    function enableById(id) {
        var button = jQuery('#'+id),
            binding = _onclick[id];
        if (binding) {
            button.attr('onclick', binding);
            button.removeClass('omg-disabled');
        }
    }

    /** Control commit/revert menu buttons */
    function projectModified(message) {
        var modified = message[1];
        if (modified) {
            enableById('project-commit');
            enableById('project-revert');
        } else {
            disableById('project-commit');
            disableById('project-revert');
        }
    }

    /***********************************************************************
     *  privileged
     ***********************************************************************/

    /** rebuild menus from given JSON data */
    this.updateFromJSON = function(json) {
        buildMenus(json);
    };

    /** rebuild menus from JSON at given url */
    this.updateFromURL = function(url) {
        jQuery.ajax({
            type: 'GET',
            url: url,
            dataType: 'json',
            success: buildMenus,
            error: function(x,y,z) {
                debug.error("Error getting Menu data:",x,y,z);
            }
        });
    };

    buildMenus(json);

    // Initially nothing modified.
    projectModified(['@project-modified', false]);
    openmdao.project.addListener('@project-modified', projectModified);
};

