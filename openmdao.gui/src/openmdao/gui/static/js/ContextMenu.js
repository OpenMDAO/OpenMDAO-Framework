/**
 * Class: ContextMenu
 * Version: 1.00.A
 * URL: http://www.openjs.com/scripts/ui/context_menu/
 * This library lets you create context menus(or right click menu as some call them) in your web pages. You can specify any 
 * 		list element as the menu. You can also set a context for the menu - or use the document - for a global context menu. 
 * 		This library supports multiple context menus for different contetxts.
 * Example: ContextMenu.set("menu-id");
 * 			ContextMenu.set("menu-element-id", "context-element-id");
 */
var ContextMenu = {
	"max_width"	: false, // The width,
	"max_height": false, // and height of the current window - to make sure that the menu dont go outside that limit.
	"menu"		: "",
	"context"	: "",
	"active_menu":"", // This will have the id of the current shown menu
		
	//Functions
	"_init":function() {
		var body = document.getElementsByTagName("body")[0];
		ContextMenu.max_width = body.clientWidth || body.offsetWidth;
		ContextMenu.max_height= body.clientHeight|| body.offsetHeight;
		
		ContextMenu._addEvent(window,"resize",ContextMenu._init);
	},
	
	/**
	 * This function binds a context menu list to the right click event.
	 * Arguments: menu_id - The ID of the ul/Li list that should be used as the Menu
	 * 			  context_id - [OPTIONAL] The ID of the element that would serve as the context for this menu. If none is specified, it defaults to 'document'
	 * Example: ContextMenu.set("menu-id");
	 * 			ContextMenu.set("menu-element-id", "context-element-id");
	 */
	"set": function(menu_id, context_id) {
		if(!ContextMenu.max_width) ContextMenu._init(); //Initalize the library if its not already done.
		if(!context_id) context_id = document;
		else if(typeof context_id == "string") context_id = document.getElementById(context_id);
		
		ContextMenu._addEvent(context_id, "contextmenu", function(e) {
			ContextMenu._stopEvent(e); //We don't want the browsers Context menu appearing - so disable it.
			ContextMenu.menu = menu_id;
			ContextMenu.context = context_id;
			ContextMenu._handleRightClick(e);
		});
	},
	
	"_handleRightClick":function(e) {
		e=e||window.event;
		var left= parseInt(e.clientX);
		var top = parseInt(e.clientY);

		//We should have the width and hight of the menu to caluclate the menu position correctly. 
		//	That why I did not used visility:hidden instead of display:none to hide the menu. 
		//	If it was display:none, getting this values would be not so easy.
		var menu = document.getElementById(ContextMenu.menu);
		var width = menu.clientWidth  || menu.offsetWidth ;
		var height= menu.clientHeight || menu.offsetHeight ;
		
		//If the context menu goes of the window, make sure it stays in.
		if(ContextMenu.max_width < left + width) left= left - width;
		if(ContextMenu.max_height< top + height) top = top - height;
		if(top < 0) top = 0;
		if(left< 0) left= 0;
		
		ContextMenu.show(ContextMenu.menu, left, top);
	},
	
	/**
	 * Show the given list as the context menu at the provided coordinates
	 * Arguments: menu_id - The ID of the ul/Li list that should be used as the Menu
	 *			  left - The x value of where the menu should appear
	 * 			  top - The y value.
	 * Example: ContextMenu.show("menu-id", 200, 350);
	 */
	"show": function(menu_id, left, top) {
		ContextMenu.hide(); //Remove any existing menus.
		left=left||0;top=top||0;//Default values for top and left
		var menu = document.getElementById(menu_id);
		menu.style.position = "fixed";
        menu.style.top = top + "px";
		menu.style.left= left + "px";
		menu.style.visibility= "visible";
		setTimeout("ContextMenu._addEvent(document,'click', ContextMenu.hide);", 200);
		ContextMenu.active_menu = menu_id;
	},
	
	/**
	 * Hides the currently displayed menu.
	 * Example: ContextMenu.hide();
	 */
	"hide": function() {
		if(!ContextMenu.active_menu) return;
		ContextMenu._removeEvent(document,"click", ContextMenu.hide);
		document.getElementById(ContextMenu.active_menu).style.visibility = "hidden";
		ContextMenu.active_menu = "";
	},
	
	
	/////////////////////////////// Library Functions ///////////////////////////////
	"_stopEvent": function(e) {
		e=e||window.event;
		e.cancelBubble = true;
		e.returnValue = false;
		if(e.stopPropagation) e.stopPropagation();
		if(e.preventDefault) e.preventDefault();
		return false;
	},
	
	"_addEvent" :function(ele,type,func,capture) {
		if(typeof ele == "string") ele = document.getElementById(ele);
		
		if(ele.attachEvent) return ele.attachEvent('on' + type, func);
		else if(ele.addEventListener) ele.addEventListener(type, func, false);
		else ele['on' + type] = func;
	},
	
	"_removeEvent": function(ele, type, func) {
		if(typeof ele == "string") ele = document.getElementById(ele);
		
		if(ele.removeEventListener) ele.removeEventListener(type, func, false);
		else if(ele.detachEvent) ele.detachEvent('on' + type, func);
		else if(ele['on' + type]) delete ele['on' + type];
	}
};

