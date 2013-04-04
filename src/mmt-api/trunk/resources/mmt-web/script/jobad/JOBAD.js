/*
	JOBAD v3
	Development version
	built: Thu, 28 Mar 2013 16:02:12 +0100
*/

/*
	JOBAD 3 Core Functions
	JOBAD.core.js
*/

var JOBAD = 
(function(){

/* 
	JOBAD 3 Main Function
	Creates a new JOBAD instance on a specefied DOM element.  
	@param element Element to link this element to. May be a DOM Element or a jQuery Object. 
	@param config Configuration for this JOBAD Instance. 

*/

var JOBAD = function(element){

	if(!(this instanceof JOBAD)){
		return new JOBAD(element);	
	}

	var me = this; //Kept in functions

	//The element the current JOBAD instance works on. 
	this.element = element;
	if(_.isElement(this.element)){
		this.element = $(this.element);
	}
	if(!(this.element instanceof jQuery)){
		JOBAD.error("Can't create JOBADInstance: Not a DOM Element. ");
	}

	/* modules */
	var InstanceModules = {};
	var disabledModules = [];

	this.modules = {};

	/*
		loads a JOBAD module if not yet loaded. 
		@param module Name of module to load. 
		@param options Array of options to pass to the module. 
		@param ignoredeps Boolean. Ignore dependencies? (Default: false). 
		@returns boolean
	*/
	this.modules.load = function(module, options, ignoredeps){
		if(me.modules.loaded(module)){
			return;	
		}

		var ignoredeps = (typeof ignoredeps == 'boolean')?ignoredeps:false;
	
		if(ignoredeps){
			if(!JOBAD.modules.available(module)){
				JOBAD.error('Error loading module '+module);			
			}
			InstanceModules[module] = new JOBAD.modules.loadedModule(module, options, me);
			return true;
		} else {
			var deps = JOBAD.modules.getDependencyList(module);
		        if(!deps){
				JOBAD.console.warn("Unresolved dependencies for module '"+module+"'. "); //Module not found (has no dependecnies)
				return false;	
			}
			for(var i=0;i<deps.length;i++){
				me.modules.load(deps[i], options, true);
			}
			return true;
		}
		

	 };

	/*
		Checks if a module is loaded. 
		@param module Name of the module to check. 
		@returns boolean
	*/
	this.modules.loaded = function(module){
		return InstanceModules.hasOwnProperty(module);
	}

	/*
		Deactivates a module
		@param module Module to be deactivated. 
	*/
	this.modules.deactivate = function(module){
		if(me.modules.isActive(module)){
			JOBAD.warn("Module '"+module+"' is already deactivated. ");
			return;
		}
		disabledModules.push(module);
	}

	/*
		Activates a module
		@param module Module to be activated. 
	*/
	this.modules.activate = function(module){
		if(me.modules.isActive(module)){
			JOBAD.warn("Module '"+module+"' is already activated. ");
			return;	
		}
		disabledModules = _.without(disabledModules, module);
	};
	
	/*
		Checks if a module is active. 
		@param module Module to check. 
	*/
	this.modules.isActive = function(module){
		return (_.indexOf(disabledModules, module)==-1); 
	};
	
	/*
		Iterate over all active modules with callback. 
		if cb returns false, abort. 
		@param callback Function to call. 
		@returns Array of results. 
	*/
	this.modules.iterate = function(callback){
		var res = [];
		for(var key in InstanceModules){
			if(InstanceModules.hasOwnProperty(key)){
				if(me.modules.isActive(key)){
					var cb = callback(InstanceModules[key]);
					if(!cb){
						return res;					
					} else {
						res.push(cb);					
					}
				}			
			}		
		}
		return res;
	};
	
	/*
		Iterate over all active modules with callback. Abort once some callback returns false. 
		@param callback Function to call. 
		@returns true if no callback returns false, otherwise false. 
	*/
	this.modules.iterateAnd = function(callback){
		for(var key in InstanceModules){
			if(InstanceModules.hasOwnProperty(key)){
				if(me.modules.isActive(key)){
					var cb = callback(InstanceModules[key]);
					if(!cb){
						return false;					
					}
				}			
			}		
		}
		return true;
	};
	
	/* Event namespace */
	this.Event = {};

	/* Setup core function */
	/* Setup on an Element */

	var enabled = false;

	/*
		Enables or disables this JOBAD instance. 
		@returns boolean indicating if the status was changed.  
	*/
	this.Setup = function(){
		if(enabled){
			return me.Setup.disable();
		} else {
			return me.Setup.enable();
		}
	}

	/*
		Enables this JOBAD instance 
		@returns boolean indicating success. 
	*/
	this.Setup.enable = function(){
		if(enabled){
			return false;
		}

		var root = me.element;

		for(var key in me.Event){
			JOBAD.Events[key].Setup.enable.call(me, root);
		}

		return true;
	}

	/*
		Disables this JOBAD instance. 
		@returns boolean indicating success. 
	*/
	this.Setup.disable = function(){
		if(!enabled){
			return false;
		}		
		var root = me.element;

		for(var key in JOBAD.Events){
			if(JOBAD.Events.hasOwnProperty(key) && !JOBAD.isEventDisabled(key)){
				JOBAD.Events[key].Setup.disable.call(me, root);
			}	
		}

		return true;
	}


	//Setup the events
	for(var key in JOBAD.Events){
		if(JOBAD.Events.hasOwnProperty(key) && !JOBAD.isEventDisabled(key)){

			me.Event[key] = JOBAD.util.bindEverything(JOBAD.Events[key].namespace, me);
			
			if(typeof JOBAD.Events[key].Setup.init == "function"){
				JOBAD.Events[key].Setup.init.call(me, me);
			} else if(typeof JOBAD.Events[key].Setup.init == "object"){
				for(var name in JOBAD.Events[key].Setup.init){
					if(JOBAD.Events[key].Setup.init.hasOwnProperty(name)){
						if(me.hasOwnProperty(name)){
							JOBAD.console.warn("Setup: Event '"+key+"' tried to override '"+name+"'")
						} else {
							me[name] = JOBAD.util.bindEverything(JOBAD.Events[key].Setup.init[name], me);
						}
					}
				}
			}


		}	
	}
};

/* JOBAD Version */
JOBAD.version = "3.0.0";

/* JOBAD Global config */
JOBAD.config = 
{
	    'debug': true, //Debugging enabled? (Logs etc)
	    'hoverdelay': 1000, //Delay for showing tooltip after hovering. (in milliseconds)
	    'cleanModuleNamespace': false,//if set to true this.loadedModule instances will not allow additional functions
	    'disabledEvents': [] //globally disabled Events
};

/* Available JOBAD Events */
JOBAD.Events = {};

/*
	Checks if an Event is disabled by the configuration. 
	@param evtname Name of the event that is disabled. 
*/
JOBAD.isEventDisabled = function(evtname){
	return (JOBAD.config.disabledEvents.indexOf(evtname) != -1);
};

/*
	JOBAD.console: Mimics  or wraps the native console object if available and debugging is enabled. 
*/
if(!_.isUndefined(console)){//Console available
	
	JOBAD.console = 
	{
		"log": function(msg){
			if(JOBAD.config.debug){
				console.log(msg);
			}
		},
		"warn": function(msg){
			if(JOBAD.config.debug){
				console.warn(msg);
			}		
		},
		"error": function(msg){
			if(JOBAD.config.debug){
				console.error(msg);
			}		
		}
	}
} else {
	JOBAD.console = 
	{
		"log": function(){},
		"warn": function(){},
		"error": function(){}	
	}
}


/*
	JOBAD.error: Produces an error message
*/
JOBAD.error = function(msg){
	JOBAD.console.error(msg);
	throw new Error(msg);
}

/*
	Module Registration
*/
JOBAD.modules = {};

var moduleList = {};
var moduleStorage = {};

/*
	Template for modules
*/
JOBAD.modules.TEMPLATE = 
{
	/*
		Provides info about this module. 
	*/
	info:{
		'identifier':	'template',  //(Unique) identifier for this module, preferably human readable. 
		'title':	'Template Module', //Human Readable title of the module. 
		'author':	'Tom Wiesing', //Author
		'description':	'A template you may use as a starting point for writing other modules. ', //A human readable description of the module. 
		'version':	'1.0', //string containing the version number. May be omitted. 
		'dependencies':	[], //Array of module dependencies. If ommited, assumed to have no dependencies. 
		'hasCleanNamespace': true // Does this module contain only standard functions?
	},
    	globalinit: function(){
	/* 
		Called exactly once GLOBALLY. Can be used to initialise global module ids, etc. May be ommitted. Will be called once a module is loaded. 
		@this undefined. 
		@returns nothing
	*/
	},
	init: function(JOBADInstance, param1, param2, param3 /*, ... */){
		/* 	
			Called to intialise a new instance of this module. 
			@this An instance of JOBAD.modules.loadedModule
			@param JOBADInstance The instance of JOBAD the module is initiated on. 
			@param *param Initial parameters passed to this.modules.load
			@return nothing. 
		*/
	},
	keyPressed: function(checkFunc, JOBADInstance){
		/*
			called when a key is pressed. At most one keyPressed event will be activated. May be ommitted. 
			@this An instance of JOBAD.modules.loadedModule
			@param checkFunc(key) Checks if the specefied combination was pressed. Order is: alt+ctrl+meta+shift+key
			@returns Returns true iff it performed some action. 
		*/
	},
	leftClick: function(target, JOBADInstance){
		/*
			called when a left click is performed.  Every left click action is performed. May be ommitted. 
			@this An instance of JOBAD.modules.loadedModule
			@param target The element that has been left clicked on. 
			@returns Returns true iff it performed some action. 
		*/
	},
	contextMenuEntries: function(target, JOBADInstance){
		/*
			called when a context menu is requested. Context Menu entries will be merged. May be ommitted.  
			@this An instance of JOBAD.modules.loadedModule
			@param target The element the menu has been requested on. 
			@returns returns context menu entries as array [[entry_1, function_1], ..., [entry_n, function_1]] or as a map {entry_1: function_1, entry_2: function_2, ...}
				All entry names must be non-empty. (Empty ones will be ignored). 
		*/
	},
	hoverText: function(target, JOBADInstance){
		/*
			called when a hover text is requested. May be ommitted. 
			@this An instance of JOBAD.modules.loadedModule
			@param target The element the hover has been requested on. 
			@returns a text, a jqueryish ($(...), Domnode, etc) object to use as hover or a boolean indicating either the text or if something was done. 
				
		*/
	}
};

/* 
	Registers a new JOBAD module with JOBAD. 
	@param ModuleObject The ModuleObject to register. 
	@returns boolean if successfull
*/
JOBAD.modules.register = function(ModuleObject){
	var moduleObject = JOBAD.modules.createProperModuleObject(ModuleObject);
	if(!moduleObject){
		return false;	
	}
	var identifier = moduleObject.info.identifier;
	if(JOBAD.modules.available(identifier)){
		return false;	
	} else {
		moduleList[identifier] = moduleObject;
		moduleStorage[identifier] = {};
		return true;
	}
};

/* 
	Creates a proper Module Object. 
	@param ModuleObject The ModuleObject to register. 
	@returns proper Module Object (adding omitted properties etc. Otherwise false. 
*/
JOBAD.modules.createProperModuleObject = function(ModuleObject){
	if(!_.isObject(ModuleObject)){
		return false;
	}
	var properObject = 
	{
		"globalinit": function(){},
		"init": function(){}
	};
	
	for(var key in properObject){
		if(properObject.hasOwnProperty(key) && 	ModuleObject.hasOwnProperty(key)){
			var obj = ModuleObject[key];
			if(typeof obj != 'function'){
				return false;			
			}
			properObject[key] = ModuleObject[key];
		}
	}

	if(ModuleObject.hasOwnProperty("info")){
		var info = ModuleObject.info;
		properObject.info = {
			'version': '',
			'dependencies': []	
		};
		
		if(info.hasOwnProperty('version')){
			if(typeof info['version'] != 'string'){
				return false;			
			}
			properObject.info['version'] = info['version'];
		}

		if(info.hasOwnProperty('hasCleanNamespace')){
			if(info['hasCleanNamespace'] == false){
				properObject.info.hasCleanNamespace = false;
			} else {
				properObject.info.hasCleanNamespace = true;
			}
		} else {
			properObject.info.hasCleanNamespace = true;			
		}

		if(info.hasOwnProperty('dependencies')){
			var arr = info['dependencies'];
			if(!_.isArray(arr)){
				return false;			
			}
			properObject.info['dependencies'] = arr;
		}

		try{
			_.map(['identifier', 'title', 'author', 'description'], function(key){
				if(info.hasOwnProperty(key)){
					var infoAttr = info[key];
					if(typeof infoAttr != 'string'){
						throw ""; //return false;
					}
					properObject.info[key] = infoAttr;
				} else {
					throw ""; //return false;
				}
			});
		} catch(e){
			return false;		
		}


		/* properties which are allowed (clean) */		
		var CleanProperties = 
		[
			'info',
			'globalinit',
			'init'
		];

		properObject.namespace = {};

		for(var key in ModuleObject){
			if(ModuleObject.hasOwnProperty(key) && CleanProperties.indexOf(key) == -1 && !JOBAD.Events.hasOwnProperty(key)){
				if(properObject.info.hasCleanNamespace){
					JOBAD.console.warn("Warning: Module '"+properObject.info.identifier+"' says its namespace is clean, but property '"+key+"' found. Check ModuleObject.info.hasCleanNamespace. ");	
				} else {
					properObject.namespace[key] = ModuleObject[key];
				}
			}
		}

		for(var key in JOBAD.Events){
			if(ModuleObject.hasOwnProperty(key)){
				properObject[key] = ModuleObject[key];
			}
		}
		
		
		
		return properObject;

	} else {
		return false;	
	}

};

/* 
	Checks if a module is available. 
	@param name The Name to check. 
	@param checkDeps Optional. Should dependencies be checked? (Will result in an endless loop if circular dependencies exist.) Default false. 
	@returns boolean.
*/
JOBAD.modules.available = function(name, checkDeps){
	var checkDeps = (typeof checkDeps == 'boolean')?checkDeps:false;
	var selfAvailable = moduleList.hasOwnProperty(name);
	if(checkDeps && selfAvailable){
		var deps = moduleList[name].info.dependencies;
		for(var i=0;i<deps.length;i++){
			if(!JOBAD.modules.available(deps[i], true)){
				return false;			
			}
		}
		return true;
	} else {
		return selfAvailable;
	}
};

/* 
	Returns an array of dependencies of name including name in such an order, thet they can all be loaded without unresolved dependencies. 
	@param name The Name to check. 
	@returns array of strings or false if some module is not available. 
*/
JOBAD.modules.getDependencyList = function(name){
	if(!JOBAD.modules.available(name, true)){
		return false;	
	}
	var depArray = [name];
	var deps = moduleList[name].info.dependencies;

        for(var i=deps.length-1;i>=0;i--){
		depArray = _.union(depArray, JOBAD.modules.getDependencyList(deps[i]));
	}
	return depArray;
};

/*
	Loads a module, assuming the dependencies are already available. 
	@param name Module to loads
	@param args Arguments to pass to the module. 
	@returns new JOBAD.modules.loadedModule instance. 
*/
JOBAD.modules.loadedModule = function(name, args, JOBADInstance){

	if(!JOBAD.modules.available(name)){
		JOBAD.error("Module is not available and cant be loaded. ");	
	}

	/*
		Storage shared accross all module instances. 
	*/
	this.globalStore = 
	{
		"get": function(prop){
			return  moduleStorage[name][prop+"_"];		
		},
		"set": function(prop, val){
			moduleStorage[name][prop+"_"] = val;
		},
		"delete": function(prop){
			delete moduleStorage[name][prop+"_"];
		}
	}
	
	var storage = {};
	/*
		Storage contained per instance of the module.  
	*/
	this.localStore = 
	{
		"get": function(prop){
			return  storage[prop];		
		},
		"set": function(prop, val){
			storage[prop] = val;
		},
		"delete": function(prop){
			delete storage[name];
		}
	}

	var ServiceObject = moduleList[name];
	/*
		Information about this module. 
	*/
	this.info = function(){
		return ServiceObject.info;
	}

	/*
		gets the JOBAD instance bound to this module object
	*/
	this.getJOBAD = function(){
		return JOBADInstance;	
	};


	//Initilisation

	if(!moduleStorage[name]["init"]){
		moduleStorage[name]["init"] = true;
		ServiceObject.globalinit.apply(undefined, []);
	}

	var params = [JOBADInstance];
	
	for(var i=0;i<args.length;i++){
		params.push(args[i]);	
	}


	if(JOBAD.config.cleanModuleNamespace){
		if(!ServiceObject.info.hasCleanNamespace){
			JOBAD.console.warn("Warning: Module '"+name+"' may have unclean namespace, but JOBAD.config.cleanModuleNamespace is true. ");		
		}
	} else {
		var orgClone = _.clone(ServiceObject.namespace);

		for(var key in orgClone){
			if(!JOBAD.Events.hasOwnProperty(key) && orgClone.hasOwnProperty(key)){//TODO: Read Event Names dynamically
				this[key] = orgClone[key];
			}
		}
	}

	for(var key in JOBAD.Events){
		if(ServiceObject.hasOwnProperty(key)){
			this[key] = ServiceObject[key];
		} else {
			this[key] = JOBAD.Events[key]["default"];
		}
	}

	ServiceObject.init.apply(this, params);		
};

/* various utility functions */
JOBAD.util = {};


/*
	Generates a list menu representation from an object representation. 
	@param menu Menu to generate. 
	@returns the new representation. 
*/
JOBAD.util.generateMenuList = function(menu){
	if(typeof menu == 'undefined'){
		return [];
	}
	var res = [];
	for(var key in menu){
		if(menu.hasOwnProperty(key)){
			var val = menu[key];
			if(typeof val == 'function'){
				res.push([key, val]);		
			} else {
				res.push([key, JOBAD.util.generateMenuList(val)]);
			}
		}
	}
	return res;
};

/*
	Binds every function within an object recursively. 
	@param obj Object to bind. 
	@param thisObj 'this' inside functions. 
*/
JOBAD.util.bindEverything = function(obj, thisObj){
	if(_.isObject(obj) && typeof obj != 'function' ){
		var ret = {};
		for(var key in obj){
			ret[key] = JOBAD.util.bindEverything(obj[key], thisObj);
		}
		return ret;
	} else if(typeof obj == 'function'){
		return _.bind(obj, thisObj);
	} else {
		return _.clone(obj);
	}
	
}

return JOBAD;
})();

/*
	JOBAD Core build configuration
*/
JOBAD.config.debug = false;

/*
	JOBAD 3 UI Functions
	JOBAD.ui.js
	
	requires: 
		JOBAD.core.js
*/

(function(JOBAD){

	//Mouse coordinates
	var mouseCoords = [0, 0];


	$(document).on("mousemove.JOBADListener", function(e){
		mouseCoords = [e.pageX-$(window).scrollLeft(), e.pageY-$(window).scrollTop()];
	});

	//UI Namespace. 
	JOBAD.UI = {}

	//Hover UI. 
	JOBAD.UI.hover = {}

	JOBAD.UI.hover.config = {
		"offsetX": 10, //offset from the mouse in X and Y
		"offsetY": 10	
	}
	
	var hoverActive = false;
	var hoverElement = undefined;

	/*
		Activates the hover ui which follows the mouse. 
		@param html HTML to use as content
		@return true. 
	*/
	JOBAD.UI.hover.enable = function(html){
		hoverActive = true;
		hoverElement = $("<div class='JOBAD JOBADHover'>").html(html).css({
			"position": "fixed",
			"background-color": "grey",
			"-webkit-border-radius": 5,
			"-moz-border-radius": 5,
			"border-radius": 5,
			"border": "1px solid black"
		});
		hoverElement.appendTo($("body"));

		$(document).on("mousemove.JOBAD.UI.hover", function(){
			JOBAD.UI.hover.refresh();
		});

		JOBAD.UI.hover.refresh();
		
		return true; 
	}

	/*
		Deactivates the hover UI if active. 
		@param element jQuery element to use as hover
		@return booelan boolean indicating of the UI has been deactived. 
	*/
	JOBAD.UI.hover.disable = function(){
		if(!hoverActive){
			return false;		
		}

		hoverActive = false;
		$(document).off("mousemove.JOBAD.UI.hover");
		hoverElement.remove();
	}
	/*
		Refreshes the position of the hover element if active. 
		@return nothing. 
	*/
	JOBAD.UI.hover.refresh = function(){
		if(hoverActive){
			hoverElement
			.css("top", Math.min(mouseCoords[1]+JOBAD.UI.hover.config.offsetY, window.innerHeight-hoverElement.outerHeight(true)))
			.css("left", Math.min(mouseCoords[0]+JOBAD.UI.hover.config.offsetX, window.innerWidth-hoverElement.outerWidth(true)))
		}
	}

	//Context Menu UI
	JOBAD.UI.ContextMenu = {}
	
	JOBAD.UI.ContextMenu.config = {
		'margin': 20, //margin from page borders
		'width': 250 //menu width
	};
	
	/*
		Registers a context menu on an element. 
		@param element jQuery element to register on. 
		@param demandFunction Function to call to get menu. 
		@param onEnable Optional. Will be called before the context menu is enabled. 
		@param onDisable Optional. Will be called after the context menu has been disabled. 
		@return the jquery element. 
	*/
	JOBAD.UI.ContextMenu.enable = function(element, demandFunction, onEnable, onDisable){
		if(typeof demandFunction != 'function'){
			JOBAD.error('JOBAD.UI.ContextMenu.enable: demandFunction is not a function'); //die
			return element;
		}
		
		if(typeof onEnable != 'function'){
			onEnable = function(element){}; //Default
		}
		if(typeof onDisable != 'function'){
			onDisable = function(element){}; //Default
		}

		element.on('contextmenu.JOBAD.UI.ContextMenu', function(e){
			if(e.ctrlKey){
				return true;
			}
			var targetElement = $(e.target);
			var elementOrg = $(e.target);
			var result = false;
			while(true){
				result = demandFunction(targetElement, elementOrg);
				if(result || element.is(this)){
					break;				
				}
				targetElement = targetElement.parent();
			}
			
			if(!result){
				return true; //Allow the browser to handle stuff			
			}
			
			$(document).trigger('JOBADContextMenuUnbind'); //close all other menus

			onEnable(element);

			var menuBuild = JOBAD.UI.ContextMenu.buildMenuList(result, targetElement, elementOrg)
			.menu()
			.css({
				'width': JOBAD.UI.ContextMenu.config.width,
				'position': 'fixed'
			})
			.on('contextmenu', function(){
				return false;			
			})
			.on('mousedown', function(e){
				e.stopPropagation();//prevent closemenu from triggering
			})
			.appendTo($("body"));
			
			

			menuBuild
			.css("top", Math.min(mouseCoords[1], window.innerHeight-menuBuild.outerHeight(true)-JOBAD.UI.ContextMenu.config.margin))
			.css("left", Math.min(mouseCoords[0], window.innerWidth-menuBuild.outerWidth(true)-JOBAD.UI.ContextMenu.config.margin))
			var closeHandler = function(e){
				menuBuild
				.remove();
				onDisable(element);
			};

			$(document).on('JOBADContextMenuUnbind', function(){
					closeHandler();
					$(document).unbind('mousedown.UI.ContextMenu.Unbind JOBADContextMenuUnbind');
			});

			$(document).on('mousedown.UI.ContextMenu.Unbind', function(){
				$(document).trigger('JOBADContextMenuUnbind');
			});
	
			
			return false;
			
		});

		return element;

	};

	/*
		Disables the Context Menu. 
		@param element jQuery element to remove the context menu from. 
		@return the jquery element. 
	*/
	JOBAD.UI.ContextMenu.disable = function(element){
		element.off('contextmenu.JOBAD.UI.ContextMenu'); //remove listener
		return element;
	};

	/*
		Builds the menu html element
		@param items The menu to build. 
		@param element The element the context menu has been requested on. 
		@param elementOrg The element the context menu call originates from. 
		@returns the menu element. 
	*/
	JOBAD.UI.ContextMenu.buildMenuList = function(items, element, elementOrg){
		var $ul = $("<ul>");
		for(var i=0;i<items.length;i++){
			var item = items[i];
			var $a = $("<a href='#'>");
			$li = $("<li>")
			.appendTo($ul)
			.append($a);
			$a
			.text(item[0])
			.on('click', function(e){
				return false; //Don't follow link. 
			});
			(function(){
				if(typeof item[1] == 'function'){
					var callback = item[1];

					$a.on('click', function(e){
						$(document).trigger('JOBADContextMenuUnbind');
						callback(element, elementOrg);
					});		
				} else {
					
					$li.append(JOBAD.UI.ContextMenu.buildMenuList(item[1], element, elementOrg));
				}
			})()
					
		}
		return $ul;
	};


	//Sidebar UI
	JOBAD.UI.Sidebar = {}; 

	JOBAD.UI.Sidebar.config = 
	{
		"width": 100 //Sidebar Width
	};

	/*
		Wraps an element to create a sidebar UI. 
		@param element The element to wrap. 
		@returns the original element, now wrapped. 
	*/
	JOBAD.UI.Sidebar.wrap = function(element){
		var org = $(element);

		var orgWrapper = $("<div>").css({"overflow": "hidden"});

		var sideBarElement = $("<div>").css({
			"float": "right",
			"width": JOBAD.UI.Sidebar.config.width,
			"height": 1, //something >0
			"position":"relative"
		});

		var container = $("<div>").css({
			"width": "100%",
			"float":"left",
			"overflow":"hidden"	
		});
	
		org.wrap(orgWrapper);

		orgWrapper = org.parent();

		orgWrapper.wrap(container);
	
		container = orgWrapper.parent();

		container.prepend(sideBarElement);

		return org;
	};

	/*
		Unwraps an element, destroying the sidebar. 
		@param The element which has a sidebar. 
		@returns the original element unwrapped. 
	*/
	JOBAD.UI.Sidebar.unwrap = function(element){
		var org = $(element);
		org
		.unwrap()
		.parent()
		.find("div")
		.first().remove();

		return org.unwrap();
	};

	/*
		Adds a new notification to the sidebar. (It must already exist)
		@param sidebar The element which has a sidebar. 
		@param element The element to bind the notification to. 
		@returns an empty new notification element. 
	*/
	JOBAD.UI.Sidebar.addNotification = function(sidebar, element){
		var sbar = $(sidebar);
		var child = $(element);
		var offset = child.offset().top - sbar.offset().top; //offset
		sbar = sbar.parent().parent().find("div").first();
	
		var newGuy =  $("<div>").css({"position": "absolute", "top": offset}).appendTo(sbar);


		var callback = function(){
			var offset = child.offset().top - sbar.offset().top; //offset
			newGuy.css({"top": offset});
		
		};
	

		$(window).on("resize.JOBAD.UI.Sidebar", callback);

		return newGuy.data("JOBAD.UI.Sidebar.ResizeHook", callback);
	};

	/*
		Forces a sidebar notification position update. 
		@returns nothing. 
	*/
	JOBAD.UI.Sidebar.forceNotificationUpdate = function(){
		$(window).trigger("resize.JOBAD.UI.Sidebar");
	};

	/*
		Removes a notification
		@param notification The notification element. 
		@returns nothing. 
	*/
	JOBAD.UI.Sidebar.removeNotification = function(notification){
		var callback = notification.data("JOBAD.UI.Sidebar.ResizeHook");
		$(window).off("resize.JOBAD.UI.Sidebar", callback);
		notification.remove();
	};


	/*
	JOBAD Keypress UI - Removed temporarily
	JOBAD.UI.keypress = {};
	//adapted code from https://raw.github.com/jeresig/jquery.hotkeys/master/jquery.hotkeys.js
	JOBAD.UI.keypress.keys = {
		specialKeys: {
			8: "backspace", 9: "tab", 13: "return", 16: "shift", 17: "ctrl", 18: "alt", 19: "pause",
			20: "capslock", 27: "esc", 32: "space", 33: "pageup", 34: "pagedown", 35: "end", 36: "home",
			37: "left", 38: "up", 39: "right", 40: "down", 45: "insert", 46: "del", 
			96: "0", 97: "1", 98: "2", 99: "3", 100: "4", 101: "5", 102: "6", 103: "7",
			104: "8", 105: "9", 106: "*", 107: "+", 109: "-", 110: ".", 111 : "/", 
			112: "f1", 113: "f2", 114: "f3", 115: "f4", 116: "f5", 117: "f6", 118: "f7", 119: "f8", 
			120: "f9", 121: "f10", 122: "f11", 123: "f12", 144: "numlock", 145: "scroll", 191: "/", 224: "meta"
		},

		shiftNums: {
			"`": "~", "1": "!", "2": "@", "3": "#", "4": "$", "5": "%", "6": "^", "7": "&", 
			"8": "*", "9": "(", "0": ")", "-": "_", "=": "+", ";": ": ", "'": "\"", ",": "<", 
			".": ">",  "/": "?",  "\\": "|"
		}
	};

	JOBAD.UI.keypress.matchesKey = function(event, comb) {
			
		var special = event.type !== "keypress" && JOBAD.UI.keypress.keys.specialKeys[ event.which ],
			character = String.fromCharCode( event.which ).toLowerCase(),
			key, modif = "", possible = {};

		// check combinations (alt|ctrl|shift+anything)
		if ( event.altKey && special !== "alt" ) {
			modif += "alt+";
		}

		if ( event.ctrlKey && special !== "ctrl" ) {
			modif += "ctrl+";
		}

		if ( event.metaKey && !event.ctrlKey && special !== "meta" ) {
			modif += "meta+";
		}

		if ( event.shiftKey && special !== "shift" ) {
			modif += "shift+";
		}

		if ( special ) {
			possible[ modif + special ] = true;

		} else {
			possible[ modif + character ] = true;
			possible[ modif + JOBAD.UI.keypress.keys.specialKeys[ character ] ] = true;

			// "$" can be triggered as "Shift+4" or "Shift+$" or just "$"
			if ( modif === "shift+" ) {
				possible[ JOBAD.UI.keypress.keys.shiftNums[ character ] ] = true;
			}
		}

		return possible.hasOwnProperty(comb.toLowerCase());
	};

	*/
})(JOBAD)

/*
	JOBAD 3 Event Functions
	JOBAD.event.js
	
	requires:
		JOBAD.core.js
		JOBAD.ui.js
*/

/* left click */
JOBAD.Events.leftClick = 
{
	'default': function(){
		return false;
	},
	'Setup': {
		'enable': function(root){
			var me = this;
			root.delegate("*", 'click.JOBAD.leftClick', function(event){
				var element = $(event.target); //The base element.  
				switch (event.which) {
					case 1:
						/* left mouse button => left click */
						me.Event.leftClick.trigger(element);
						event.stopPropagation(); //Not for the parent. 
						break;
					default:
						/* nothing */
				}
			});
		},
		'disable': function(root){
			root.undelegate("*", 'click.JOBAD.leftClick');	
		}
	},
	'namespace': 
	{
		
		'getResult': function(target){
			return this.modules.iterateAnd(function(module){
				module.leftClick.call(module, target, module.getJOBAD());
				return true;
			});
		},
		'trigger': function(target){
			return this.Event.leftClick.getResult(target);
		}
	}
};

/* context menu entries */
JOBAD.Events.contextMenuEntries = 
{
	'default': function(){
		return [];
	},
	'Setup': {
		'enable': function(root){
			var me = this;
			JOBAD.UI.ContextMenu.enable(root, function(target){
				return me.Event.contextMenuEntries.getResult(target);
			});
		},
		'disable': function(root){
			JOBAD.UI.ContextMenu.disable(root);
		}
	},
	'namespace': 
	{
		'getResult': function(target){
			var res = [];
			var mods = this.modules.iterate(function(module){
				var entries = module.contextMenuEntries.call(module, target, module.getJOBAD());
				return (_.isArray(entries))?entries:JOBAD.util.generateMenuList(entries);
			});
			for(var i=0;i<mods.length;i++){
				var mod = mods[i];
				for(var j=0;j<mod.length;j++){
					res.push(mod[j]);
				}
			}
			if(res.length == 0){
				return false;		
			} else {
				return res;		
			}
		}
	}
}

/* hover Text */
JOBAD.Events.hoverText = 
{
	'default': function(){
		return false;	
	},
	'Setup': {
		'init': function(){
			this.Event.hoverText.activeHoverElement = undefined; //the currently active element. 
		},
		'enable': function(root){
			
			var me = this;
			var trigger = function(event){
				var res = me.Event.hoverText.trigger($(this));
				if(res){//something happened here: dont trigger on parent
					event.stopPropagation();
				} else if(!$(this).is(root)){ //I have nothing => trigger the parent
					$(this).parent().trigger('mouseenter.JOBAD.hoverText', event); //Trigger parent if i'm not root. 	
				}
				return false;
			};


			var untrigger = function(event){
				return me.Event.hoverText.untrigger($(this));	
			};

			root
			.delegate("*", 'mouseenter.JOBAD.hoverText', trigger)
			.delegate("*", 'mouseleave.JOBAD.hoverText', untrigger);
		},
		'disable': function(root){
			if(typeof this.Event.hoverText.activeHoverElement != 'undefined')
			{
				me.Event.hoverText.untrigger(); //remove active Hover menu
			}
		
			root
			.undelegate("*", 'mouseenter.JOBAD.hoverText')
			.undelegate("*", 'mouseleave.JOBAD.hoverText');
		}
	},
	'namespace': {
		'getResult': function(target){
			var res = false;
			this.modules.iterate(function(module){
				var hoverText = module.hoverText.call(module, target, module.getJOBAD()); //call apply and stuff here
				if(typeof hoverText != 'undefined' && typeof res == "boolean"){//trigger all hover handlers ; display only the first one. 
					if(typeof hoverText == "string"){
						res = $("<p>").text(hoverText)			
					} else if(typeof hoverText != "boolean"){
						try{
							res = jQuery(hoverText);
						} catch(e){
							JOBAD.error("Module '"+module.info().identifier+"' returned invalid HOVER result. ");
						}
					} else if(hoverText === true){
						res = true;
					}
				}
				return true;
			});
			return res;
		},
		'trigger': function(source){
			if(source.data('JOBAD.hover.Active')){
				return false;		
			}

			var EventResult = this.Event.hoverText.getResult(source); //try to do the event
		
			if(typeof EventResult == 'boolean'){
				return EventResult;		
			}

			if(this.Event.hoverText.activeHoverElement instanceof jQuery)//something already active
			{
				if(this.Event.hoverText.activeHoverElement.is(source)){
					return true; //done and die			
				}
				this.Event.hoverText.untrigger(this.Event.hoverText.activeHoverElement);	
			}

			this.Event.hoverText.activeHoverElement = source;

			source.data('JOBAD.hover.Active', true);		
		
			var tid = window.setTimeout(function(){
				source.removeData('JOBAD.hover.timerId');
				JOBAD.UI.hover.enable(EventResult.html());
			}, JOBAD.config.hoverdelay)

			source.data('JOBAD.hover.timerId', tid);//save timeout id
			return true;
						
		},
		'untrigger': function(source){
			if(typeof source == 'undefined'){
				if(this.Event.hoverText.activeHoverElement instanceof jQuery){
					source = this.Event.hoverText.activeHoverElement;
				} else {
					return false;			
				}
			}		

			if(!source.data('JOBAD.hover.Active')){
				return false;		
			}

		

			if(typeof source.data('JOBAD.hover.timerId') == 'number'){
				window.clearTimeout(source.data('JOBAD.hover.timerId'));
				source.removeData('JOBAD.hover.timerId');		
			}

			source.removeData('JOBAD.hover.Active');

			this.Event.hoverText.activeHoverElement = undefined;

			JOBAD.UI.hover.disable();

			if(!source.is(this.element)){
				this.Event.hoverText.trigger(source.parent());//we are in the parent now
				return false;
			}

			return true;
		}
	}
}


/* sidebar: onUpDate Event */
JOBAD.Events.onUpdate = 
{
	'default': function(){
	},
	'Setup': {
		'init': {
			'Sidebar': {
				'forceUpdate': function(){
					
				}			
			}
		},
		'enable': function(root){
			this.Event.onUpdate.enabled = true;
		},
		'disable': function(root){
			this.Event.onUpdate.enabled = undefined;
		}
	},
	'namespace': 
	{
		
		'getResult': function(){
			if(this.Event.onUpdate.enabled){
				this.modules.iterateAnd(function(module){
					module.onUpdate.call(module, module.getJOBAD());
					return true;
				});
			}
		},
		'trigger': function(){
			this.Event.onUpdate.getResult();
		}
	}
};

