
var openmdao = (typeof openmdao == "undefined" || !openmdao ) ? {} : openmdao ; 

// TODO: may try es5 compat lib like https://bitbucket.org/JustinLove/es5/

/**
 * this is for older browsers (e.g. ffox 3.x) that don't implement ECMAScript5 create()
 * @see http://javascript.crockford.com/prototypal.html
 */
 if (typeof Object.create !== 'function') {
    //alert("You are using an older browser that is not supported.  We'll try anyway, but please upgrade to Chrome or Firefox 5...")
    Object.create = function (o) {
        function F() {}
        F.prototype = o;
        return new F();
    };
}

/** 
 * this is for older browsers (e.g. ffox 3.x) that don't implement ECMAScript5 Object.keys()
 * @see https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Object/keys
 */
if(!Object.keys) Object.keys = function(o){
    if (o !== Object(o))
        throw new TypeError('Object.keys called on non-object');
    var ret=[],p;
    for(p in o) if(Object.prototype.hasOwnProperty.call(o,p)) ret.push(p);
    return ret;
}

/**
 * utility functions used in the openmdao gui
 */ 
openmdao.Util = {

    /**
     * function to toggle visibility of an element 
     * 
     * id:   id of the element to hide/show
     */
    toggle_visibility: function(id) {
        var e = document.getElementById(id);
        if (e.style.display == 'block')
            e.style.display = 'none';
        else
            e.style.display = 'block';
    },
       
    /**
     * function to block all input on the page 
     * (by covering it with a semi-transparnet div)
     */
    toggle_screen: function() {
        var id = '_smokescreen',
            el = document.getElementById(id)
        if (el == null) {
            el = document.createElement('div')
            el.setAttribute('id',id)
            el.style.cssText='position:fixed;top:0px;left:0px;'+
                             'height:100%;width:100%;'+
                             'background:#EEE;opacity:.4;' +
                             'z-index:999;display:none'
            document.body.appendChild(el)
        }
        if (el.style.display == 'block')
            el.style.display = 'none'
        else
            el.style.display = 'block'
    },
       
    /**
     * open a popup window to view a URL
     *
     * url:     the url to open in the new window
     * title:   the title of the window (FIXME: doesn't work)
     * h:       the height of the window
     * w:       the width of the window
     */
    popupWindow: function(url,title,h,w) {
        LeftPosition = (screen.width) ? (screen.width-w)/2 : 10;
        TopPosition = (screen.height) ? (screen.height-h)/2 : 10;
        var settings = 'height='+h+',width='+w+',top='+TopPosition+',left='+LeftPosition+
                       ',resizable=no,scrollbars=no,toolbar=no,menubar=no'+
                       ',location=no,directories=no,status=no';
        return window.open(url,title,settings)
    },

    /**
     * open a popup window to view HTML
     *
     * url:     the url to open in the new window
     * title:   the title of the window (FIXME: doesn't work)
     * h:       the height of the window
     * w:       the width of the window
     */
    htmlWindow: function(html,title,h,w) {
        var win =  window.open('',title,'width='+w+',height='+h);
        win.document.open();
        win.document.write(html);
        win.document.close();
    },
    
    /**
     * add a handler to the onload event
     * ref: http://simonwillison.net/2004/May/26/addLoadEvent/
     *
     * func:    the function to add
     */
    addLoadEvent: function(func) {
      var oldonload = window.onload;
      if (typeof window.onload != 'function') {
        window.onload = func;
      } else {
        window.onload = function() {
          if (oldonload) {
            oldonload();
          }
          func();
        }
      }
    },

    /**
     * function to scroll to the bottom of an element (FIXME: doesn't work)
     *
     * el:      the element to scroll
     */
    scrollToBottom: function(el) {
        el.scrollTop = el.scrollHeight;
        el.scrollTop = el.scrollHeight - el.clientHeight;     
    },

    /**
     * prompt for a name
     *
     * callback:    the function to call with the provided name
     */
    promptForName: function(callback) {
        // if the user didn't specify a callback, just return
        if (typeof callback != 'function')
            return;

        // Build dialog markup
        var win = jQuery('<div><p>Enter name:</p></div>');
        var userInput = jQuery('<input type="text" style="width:100%"></input>');
        userInput.appendTo(win);

        // Display dialog
        jQuery(win).dialog({
            'modal': true,
            'buttons': {
                'Ok': function() {
                    jQuery(this).dialog('close');
                    callback(jQuery(userInput).val());
                },
                'Cancel': function() {
                    jQuery(this).dialog('close');
                }
            }
        });
    },

    /**
     * show the properties of an object on the log (debug only)
     *
     * obj:     the object for which properties are to be displayed
     */
    dumpProps: function(obj) {
        for (var prop in obj) {
            debug.log(prop + ": " + obj[prop]);
        }
    },
    
    /**
     * close the browser window
     */
    closeWindow: function() {
        jQuery('body').html('<center><b>Thank you for using OpenMDAO, You may close your browser now!')
        window.open('','_self')
        window.close()
    },
    
    /**
     * The purge function takes a reference to a DOM element as an argument. It loops through the
     * element's attributes. If it finds any functions, it nulls them out. This breaks the cycle,
     * allowing memory to be reclaimed. It will also look at all of the element's descendent
     * elements, and clear out all of their cycles as well. The purge function is harmless on
     * Mozilla and Opera. It is essential on IE. The purge function should be called before removing
     * any element, either by the removeChild method, or by setting the innerHTML property.  
     *
     * http://www.crockford.com/javascript/memory/leak.html
     */
    purge: function(d) {
        var a = d.attributes, i, l, n;
        if (a) {
            l = a.length;
            for (i = 0; i < l; i += 1) {
                n = a[i].name;
                if (typeof d[n] === 'function') {
                    d[n] = null;
                }
            }
        }
        a = d.childNodes;
        if (a) {
            l = a.length;
            for (i = 0; i < l; i += 1) {
                purge(d.childNodes[i]);
            }
        }
    },
   
    /**
     * refresh n times (for debugging memory leak)
     */
    refreshX: function(n) {
        if (n > 0) {
            model.updateListeners();
            n = n-1;
            setTimeout( "openmdao.Util.refreshX("+n+")", 2000 );
        }
    },

    /** get the pathname of the parent component */
    getParentPath: function(path) {
        parent_path = ''
        if (path) {
            var lastdot = path.lastIndexOf('.')
            if (lastdot > 0)
                parent_path = path.substring(0,lastdot)
        }
        return parent_path
    },
   
    /** get the name from the pathname */
    getName: function(pathname) {
        var tok = pathname.split('.'),
            name = pathname;        
        if (tok.length > 1) {
            name = tok[tok.length-1];
        }
        return name;
    }
}