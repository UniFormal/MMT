/*
	JOBAD v3
	Development version
	built: Thu, 02 May 2013 16:31:35 +0200
*/

var JOBAD = (function(){
/* start <JOBAD.core.js> */
/*
	JOBAD 3 Core
*/

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
	
	//Add init arguments
	me.arguments = [];
	for(var i=0;i<arguments.length;i++){
		me.arguments.push(arguments[i]);
	}

	//The element the current JOBAD instance works on. 
	this.element = element;
	if(JOBAD.refs._.isElement(this.element)){
		this.element = JOBAD.refs.$(this.element);
	}
	if(!(this.element instanceof JOBAD.refs.$)){
		JOBAD.error("Can't create JOBADInstance: Not a DOM Element. ");
	}
	
	//IFace extensions
	for(var i=0; i < JOBAD.ifaces.length; i++){
		var mod = JOBAD.ifaces[i];
		if(typeof mod == 'function'){
			mod.call(this, this, this.arguments); 
		}
	}
};

JOBAD.ifaces = []; //JOBAD interfaces

/* JOBAD Version */
JOBAD.version = "3.0.0"; 

/*
	JOBAD.toString
*/
JOBAD.toString = function(){
	return "function(/* JOBAD "+JOBAD.version+" */){ [non-native non-trivial code] }";
};

/* JOBAD Global config */
JOBAD.config = 
{
	    'debug': true //Debugging enabled? (Logs etc)
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
	JOBAD Dependencies namespace. 
*/
JOBAD.refs = {};
JOBAD.refs.$ = jQuery;
JOBAD.refs._ = _; 

JOBAD.noConflict = function(){
	return {
		"_": JOBAD.noConflict._(),
		"$": JOBAD.noConflict.$()	
	}
}; //No conflict mode

/*
	sets jQuery in noConflict mode. 
	@returns jQuery.noConflict()
*/
JOBAD.noConflict.$ = function(){
	
	JOBAD.refs.$ = JOBAD.refs.$.noConflict();
	return JOBAD.refs.$;
};

/*
	sets Underscore in noConflict mode. 
	@returns _.noConflict()
*/
JOBAD.noConflict._ = function(){
	JOBAD.refs._ = JOBAD.refs._.noConflict();
	return JOBAD.refs._;
};/* end   <JOBAD.core.js> */
/* start <JOBAD.utils.js> */
/*
	JOBAD utility functions
*/

/* IE fixes: Array.indexOf */
//from https://developer.mozilla.org/en-US/docs/JavaScript/Reference/Global_Objects/Array/indexOf
if (!Array.prototype.indexOf) {
    Array.prototype.indexOf = function (searchElement /*, fromIndex */ ) {
        "use strict";
        if (this == null) {
            throw new TypeError();
        }
        var t = Object(this);
        var len = t.length >>> 0;
        if (len === 0) {
            return -1;
        }
        var n = 0;
        if (arguments.length > 1) {
            n = Number(arguments[1]);
            if (n != n) { // shortcut for verifying if it's NaN
                n = 0;
            } else if (n != 0 && n != Infinity && n != -Infinity) {
                n = (n > 0 || -1) * Math.floor(Math.abs(n));
            }
        }
        if (n >= len) {
            return -1;
        }
        var k = n >= 0 ? n : Math.max(len - Math.abs(n), 0);
        for (; k < len; k++) {
            if (k in t && t[k] === searchElement) {
                return k;
            }
        }
        return -1;
    }
}

/* various utility functions */
JOBAD.util = {};

/*
	Binds every function within an object recursively. 
	@param obj Object to bind. 
	@param thisObj 'this' inside functions. 
*/
JOBAD.util.bindEverything = function(obj, thisObj){
	if(JOBAD.refs._.isObject(obj) && typeof obj != 'function' ){
		var ret = {};
		for(var key in obj){
			ret[key] = JOBAD.util.bindEverything(obj[key], thisObj);
		}
		return ret;
	} else if(typeof obj == 'function'){
		return JOBAD.refs._.bind(obj, thisObj);
	} else {
		return JOBAD.refs._.clone(obj);
	}
	
}/* end   <JOBAD.utils.js> */
/* start <JOBAD.core.modules.js> */
/*
	JOBAD Core Module logic
	depends:
		JOBAD.core.js
*/


//JOBAD module iface for JOBADInstances
JOBAD.ifaces.push(function(me, args){
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
			InstanceModules[module].onActivate(me);
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
		if(!me.modules.isActive(module)){
			JOBAD.console.warn("Module '"+module+"' is already deactivated. ");
			return;
		}
		disabledModules.push(module);
		InstanceModules[module].onDeactivate(me);
	}

	/*
		Activates a module
		@param module Module to be activated. 
	*/
	this.modules.activate = function(module){
		if(me.modules.isActive(module)){
			JOBAD.console.warn("Module '"+module+"' is already activated. ");
			return;	
		}
		disabledModules = JOBAD.refs._.without(disabledModules, module);
		InstanceModules[module].onActivate(me);
	};
	
	/*
		Checks if a module is active. 
		@param module Module to check. 
	*/
	this.modules.isActive = function(module){
		return (JOBAD.refs._.indexOf(disabledModules, module)==-1); 
	};
	
	/*
		Gets the identifiers of all loaded modules. 
	*/	
	this.modules.getIdentifiers = function(){
		var keys = [];
		for(var key in InstanceModules){
			if(InstanceModules.hasOwnProperty(key)){
				keys.push(key);
			}	
		}
		return keys;
	};
	
	/*
		Gets the loaded module with the specefied identifier. 
	*/	
	this.modules.getLoadedModule = function(id){
		if(!InstanceModules.hasOwnProperty(id)){
			JOBAD.console.warn("Can't find JOBAD.modules.loadedModule instance of '"+id+"'");
			return;
		}
		return InstanceModules[id];
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
});

JOBAD.modules = {};
JOBAD.modules.extensions = {}; //EXtensions for modules
JOBAD.modules.ifaces = []; //JOABd Module ifaces
JOBAD.modules.cleanProperties = ["init", "activate", "deactivate", "globalinit", "info"];

var moduleList = {};
var moduleStorage = {};

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
	if(!JOBAD.refs._.isObject(ModuleObject)){
		return false;
	}
	var properObject = 
	{
		"globalinit": function(){},
		"init": function(){},
		"activate": function(){},
		"deactivate": function(){}
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
			if(!JOBAD.refs._.isArray(arr)){
				return false;			
			}
			properObject.info['dependencies'] = arr;
		}

		try{
			JOBAD.refs._.map(['identifier', 'title', 'author', 'description'], function(key){
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

		properObject.namespace = {};


		//TODO: make this an iface or an extension
		for(var key in ModuleObject){
			if(ModuleObject.hasOwnProperty(key) && JOBAD.modules.cleanProperties.indexOf(key) == -1){
				if(properObject.info.hasCleanNamespace){
					JOBAD.console.warn("Warning: Module '"+properObject.info.identifier+"' says its namespace is clean, but property '"+key+"' found. Check ModuleObject.info.hasCleanNamespace. ");	
				} else {
					properObject.namespace[key] = ModuleObject[key];
				}
			}
		}
		
		for(var i=0;i<JOBAD.modules.ifaces.length;i++){
			var mod = JOBAD.modules.ifaces[i];
			properObject = mod[0].call(this, properObject, ModuleObject);
		}
		
		for(var key in JOBAD.modules.extensions){
			var mod = JOBAD.modules.extensions[key];
			var av = ModuleObject.hasOwnProperty(key);
			var prop = ModuleObject[key];
			if(mod.required && !av){
				JOBAD.error("Error: Cannot load module '"+properObject.info.identifier+"'. Missing required core extension '"+key+"'. ");
			}

			if(av){
				if(!mod.validate(prop)){
					JOBAD.error("Error: Cannot load module '"+properObject.info.identifier+"'. Core Extension '"+key+"' failed to validate. ");
				}
			}
			
			properObject[key] = mod.init(av, prop, ModuleObject, properObject);
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
		depArray = JOBAD.refs._.union(depArray, JOBAD.modules.getDependencyList(deps[i]));
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
		},
		"keys": function(){
			var keys = [];
			for(var key in moduleStorage[name]){
				if(moduleStorage[name].hasOwnProperty(key) && key[key.length-1] == "_"){
					keys.push(key.substr(0, key.length-1));
				}
			}
			return keys;
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
		},
		"keys": function(){
			var keys = [];
			for(var key in storage){
				if(storage.hasOwnProperty(key)){
					keys.push(key);
				}
			}
			return keys;
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
		for(var key in JOBAD.modules.extensions){
			var mod = JOBAD.modules.extensions[key];
			var val = ServiceObject[key];
			if(typeof mod["onFirstLoad"] == 'function'){
				mod.onFirstLoad(this.globalStore);
			}
		}
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
		var orgClone = JOBAD.refs._.clone(ServiceObject.namespace);
		for(var key in orgClone){
			if(!JOBAD.modules.cleanProperties.hasOwnProperty(key) && orgClone.hasOwnProperty(key)){
				this[key] = orgClone[key];
			}
		};
	}
	
	//Init module extensions
	for(var key in JOBAD.modules.extensions){
		var mod = JOBAD.modules.extensions[key];
		var val = ServiceObject[key];
		if(typeof mod["onLoad"] == 'function'){
			mod.onLoad.call(this, val, ServiceObject, this);
		}
	}
	
	//Init module ifaces
	for(var i=0;i<JOBAD.modules.ifaces.length;i++){
		var mod = JOBAD.modules.ifaces[i];
		mod[1].call(this, ServiceObject);
	}
	
	//Core events: activate, deactivate
	this.onActivate = ServiceObject.activate;
	this.onDeactivate = ServiceObject.deactivate;
	
	this.activate = function(){
		return JOBADInstance.modules.activate(this.info().identifier);
	};
	
	this.deactivate = function(){
		return JOBADInstance.modules.deactivate(this.info().identifier);
	}
	
	ServiceObject.init.apply(this, params);		
};
/* end   <JOBAD.core.modules.js> */
/* start <JOBAD.core.events.js> */
/*
	JOBAD Core Event Logic
	depends:
		JOBAD.core.modules.js
		JOABD.core.js
*/

//Provides custom events for modules
JOBAD.ifaces.push(function(me, args){
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
	};

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
	};
	
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
});


JOBAD.modules.ifaces.push([
	function(properObject, ModuleObject){
		//Called whenever 
		for(var key in JOBAD.Events){
			if(ModuleObject.hasOwnProperty(key)){
				properObject[key] = ModuleObject[key];
			}
		}
		return properObject;
	},
	function(ServiceObject){
		for(var key in JOBAD.Events){
			if(ServiceObject.hasOwnProperty(key)){
				this[key] = ServiceObject[key];
			} else {
				this[key] = JOBAD.Events[key]["default"];
			}
		}
	}]);

/*
	Checks if an Event is disabled by the configuration. 
	@param evtname Name of the event that is disabled. 
*/
JOBAD.isEventDisabled = function(evtname){
	return (JOBAD.config.disabledEvents.indexOf(evtname) != -1);
};

JOBAD.Events = {};

//config
JOBAD.config.disabledEvents = []; //Disabled events
JOBAD.config.cleanModuleNamespace = false;//if set to true this.loadedModule instances will not allow additional functions/* end   <JOBAD.core.events.js> */
/* start <JOBAD.ui.js> */
/*
	JOBAD 3 UI Functions
	JOBAD.ui.js
	
	requires: 
		JOBAD.core.js
*/

//Mouse coordinates
var mouseCoords = [0, 0];


JOBAD.refs.$(document).on("mousemove.JOBADListener", function(e){
	mouseCoords = [e.pageX-JOBAD.refs.$(window).scrollLeft(), e.pageY-JOBAD.refs.$(window).scrollTop()];
});

//UI Namespace. 
JOBAD.UI = {}

//Hover UI. 
JOBAD.UI.hover = {}

JOBAD.UI.hover.config = {
	"offsetX": 10, //offset from the mouse in X and Y
	"offsetY": 10,
	"hoverDelay": 1000 //Delay for showing tooltip after hovering. (in milliseconds)	
}

var hoverActive = false;
var hoverElement = undefined;

/*
	Activates the hover ui which follows the mouse. 
	@param html HTML to use as content
	@param CssClass The CSS class to apply to the hover. 
	@return true. 
*/
JOBAD.UI.hover.enable = function(html, CssClass){
	hoverActive = true;
	hoverElement = JOBAD.refs.$("<div class='JOBAD'>").addClass(CssClass).html(html);
	hoverElement.appendTo(JOBAD.refs.$("body"));

	JOBAD.refs.$(document).on("mousemove.JOBAD.UI.hover", function(){
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
	JOBAD.refs.$(document).off("mousemove.JOBAD.UI.hover");
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
		var targetElement = JOBAD.refs.$(e.target);
		var elementOrg = JOBAD.refs.$(e.target);
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
		
		JOBAD.refs.$(document).trigger('JOBADContextMenuUnbind'); //close all other menus

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
		.appendTo(JOBAD.refs.$("body"));
		
		

		menuBuild
		.css("top", Math.min(mouseCoords[1], window.innerHeight-menuBuild.outerHeight(true)-JOBAD.UI.ContextMenu.config.margin))
		.css("left", Math.min(mouseCoords[0], window.innerWidth-menuBuild.outerWidth(true)-JOBAD.UI.ContextMenu.config.margin))
		var closeHandler = function(e){
			menuBuild
			.remove();
			onDisable(element);
		};

		JOBAD.refs.$(document).on('JOBADContextMenuUnbind', function(){
				closeHandler();
				JOBAD.refs.$(document).unbind('mousedown.UI.ContextMenu.Unbind JOBADContextMenuUnbind');
		});

		JOBAD.refs.$(document).on('mousedown.UI.ContextMenu.Unbind', function(){
			JOBAD.refs.$(document).trigger('JOBADContextMenuUnbind');
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
	var $ul = JOBAD.refs.$("<ul class='JOBAD JOBAD_Contextmenu'>");
	for(var i=0;i<items.length;i++){
		var item = items[i];
		var $a = JOBAD.refs.$("<a href='#'>");
		$li = JOBAD.refs.$("<li>")
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
					JOBAD.refs.$(document).trigger('JOBADContextMenuUnbind');
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
	"width": 100, //Sidebar Width
	"iconDistance": 15, //Minimal Icon distance
	"icons": { //Class Icons 
		/* All icons are public domain taken from http://openiconlibrary.sourceforge.net/ */
		"warning": "data:image/png;base64,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",
		"info": "data:image/png;base64,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", 
		"error": "data:image/png;base64,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", 
		"multiple_open": "data:image/png;base64,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",
		"multiple_close": "data:image/png;base64,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"
	}
};

/*
	Wraps an element to create a sidebar UI. 
	@param element The element to wrap. 
	@returns the original element, now wrapped. 
*/
JOBAD.UI.Sidebar.wrap = function(element){
	var org = $(element);

	var orgWrapper = JOBAD.refs.$("<div>").css({"overflow": "hidden"});

	var sideBarElement = JOBAD.refs.$("<div class='JOBAD JOBAD_Sidebar JOBAD_Sidebar_Container'>").css({
		"width": JOBAD.UI.Sidebar.config.width
	});

	var container = JOBAD.refs.$("<div class='JOBAD JOBAD_Sidebar JOBAD_Sidebar_Wrapper'>");

	org.wrap(orgWrapper);

	orgWrapper = org.parent();

	orgWrapper.wrap(container);

	container = orgWrapper.parent();

	container.prepend(sideBarElement);
	
	JOBAD.refs.$(window).on("resize.JOBAD.UI.Sidebar", function(){
		var children = sideBarElement.find("*").filter(function(){
			var me = JOBAD.refs.$(this);
			return me.data("JOBAD.UI.Sidebar.isElement")===true;
		});
		var cs = [];
		children.each(function(i, e){
			var e = JOBAD.refs.$(e).detach().appendTo(sideBarElement).addClass("JOBAD_Sidebar_Single");
			var offset = e.data("JOBAD.UI.Sidebar.orgElement").offset().top - sideBarElement.offset().top; //offset
			e.css({"top": offset, "right": 0});
		});
		
		//Sort the children in some way
		children = JOBAD.refs.$(children.sort(function(a, b){
			a = JOBAD.refs.$(a);
			b = JOBAD.refs.$(b);
			if(parseInt(a.css("top")) < parseInt(b.css("top"))){
				return -1;
			} else if(parseInt(a.css("top")) > parseInt(b.css("top"))){
				return 1;
			} else {
				return 0;
			}
		}));
		var prev = children.eq(0);
		var me = children.eq(1);
		var groups = [[prev]];
		for(var i=1;i<children.length;i++){
			var prevTop = parseInt(prev.css("top"));
			var myTop = parseInt(me.css("top"));
			if(myTop-(prevTop+prev.height()) < JOBAD.UI.Sidebar.config.iconDistance){
				groups[groups.length-1].push(me); //push me to the last group; we're to close
				me = children.eq(i+1);
			} else {
				groups.push([me]);
				prev = me;
				me = children.eq(i+1);
			}
		}

		JOBAD.refs.$(children).detach();
		
		sideBarElement.find("*").remove(); //clear the sidebar now
		
		for(var i=0;i<groups.length;i++){
			//Group them together
			if(groups[i].length > 1){
				(function(){
					var group = JOBAD.refs.$(groups[i]);
					var top = parseInt(JOBAD.refs.$(group[0]).css("top"));
					var par = JOBAD.refs.$("<div class='JOBAD JOBAD_Sidebar JOBAD_Sidebar_group'><img src='"+JOBAD.UI.Sidebar.config.icons.multiple_open+"' width='16' height='16'></div>")
					.css("top", top).appendTo(sideBarElement);
					var img = par.find("img");
					var state = false;
					par.click(function(){
						if(state){
							for(var j=0;j<group.length;j++){
								group[j].hide();
							}
							img.attr("src", JOBAD.UI.Sidebar.config.icons.multiple_open);
						} else {
							for(var j=0;j<group.length;j++){
								group[j].show();
							}
							img.attr("src", JOBAD.UI.Sidebar.config.icons.multiple_close);
						}
						state = !state;
						
					});
					for(var j=0;j<group.length;j++){
						JOBAD.refs.$(group[j]).appendTo(par).hide().removeClass("JOBAD_Sidebar_Single").addClass("JOBAD_Sidebar_group_element").css({
							"right": 20,
							"top": -16
						});
					}
				})();
			} else {
				sideBarElement.append(JOBAD.refs.$(groups[i][0]).removeClass("JOBAD_Sidebar_group_element").show());
			}
			
		}
	})
	
	$(window).on("resize", function(){
		JOBAD.UI.Sidebar.forceNotificationUpdate();
	});

	org.data("JOBAD.UI.Sidebar.active", true);
	return org;
};

/*
	Unwraps an element, destroying the sidebar. 
	@param The element which has a sidebar. 
	@returns the original element unwrapped. 
*/
JOBAD.UI.Sidebar.unwrap = function(element){
	var org = JOBAD.refs.$(element);
	org
	.unwrap()
	.parent()
	.find("div")
	.first().remove();

	org.removeData("JOBAD.UI.Sidebar.active");

	return org.unwrap();
};

/*
	Adds a new notification to the sidebar. (It must already exist)
	@param sidebar The element which has a sidebar. 
	@param element The element to bind the notification to. 
	@param config The configuration. 
			config.class:	Notificaton class. Default: none. 
			config.icon:	Icon (Default: Based on notification class. 
			config.text:	Text
			config.menu:	Context Menu
			config.trace:	Trace the original element on hover?
			config.click:	Callback on click
			config.menuThis: This for menu callbacks
	@returns an empty new notification element. 
*/
JOBAD.UI.Sidebar.addNotification = function(sidebar, element, config){
	var config = (typeof config == 'undefined')?{}:config;
	
	var sbar = JOBAD.refs.$(sidebar);
	var child = JOBAD.refs.$(element);
	var offset = child.offset().top - sbar.offset().top; //offset
	sbar = sbar.parent().parent().find("div").first();

	var newGuy =  JOBAD.refs.$("<div class='JOBAD JOBAD_Sidebar JOBAD_Sidebar_Notification'>").css({"top": offset}).appendTo(sbar);
	newGuy.data("JOBAD.UI.Sidebar.orgElement", element);
	newGuy.data("JOBAD.UI.Sidebar.isElement", true);
	
	//config
	if(typeof config.menu != 'undefined'){
		var entries = JOBAD.util.fullWrap(config.menu, function(org, args){
			return org.apply(newGuy, [element, config.menuThis]);
		});
		JOBAD.UI.ContextMenu.enable(newGuy, function(){return entries;});
	}
	
	
	if(config.hasOwnProperty("text")){
		newGuy.text(config.text);
	}
	

	if(config.trace){
		//highlight the element
		newGuy.hover(
		function(){
			JOBAD.UI.highlight(element);
		},
		function(){
			JOBAD.UI.unhighlight(element);
		});
	}

	if(typeof config.click == "function"){
		newGuy.click(config.click);
	}

	var icon = false;
	if(typeof config["class"] == 'string'){
		var notClass = config["class"];
		
		if(JOBAD.UI.Sidebar.config.icons.hasOwnProperty(notClass)){
			icon = JOBAD.UI.Sidebar.config.icons[notClass];
		}						
	}
	
	if(typeof config.icon == 'string'){
		icon = config.icon;
	}
	if(typeof icon == 'string'){
		newGuy.html("<img src='"+icon+"' width='16px' height='16px'>")
		.hover(function(){
			JOBAD.UI.hover.enable(JOBAD.refs.$("<div>").text(config.text).html(), "JOBAD_Sidebar_Hover JOBAD_Sidebar "+((typeof config["class"] == 'string')?" JOBAD_Notification_"+config["class"]:""));
		}, function(){
			JOBAD.UI.hover.disable();
		});
	} else {
		newGuy.addClass("JOBAD_Notification_"+notClass);
	}
	
	return newGuy;
};

/*
	Forces a sidebar notification position update. 
	@returns nothing. 
*/
JOBAD.UI.Sidebar.forceNotificationUpdate = function(){
	JOBAD.refs.$(window).trigger("resize.JOBAD.UI.Sidebar");
};

/*
	Removes a notification
	@param notification The notification element. 
	@returns nothing. 
*/
JOBAD.UI.Sidebar.removeNotification = function(notification){
	notification.remove();
};


//highlighting
/*
	highlights an element
*/
JOBAD.UI.highlight = function(element){
	var element = JOBAD.refs.$(element);
	var col;
	if(typeof element.data("JOBAD.UI.highlight.orgColor") == 'string'){
		col = element.data("JOBAD.UI.highlight.orgColor");
	} else {
		col = element.css("backgroundColor");
	}
	
	element
	.stop().data("JOBAD.UI.highlight.orgColor", col)
	.animate({ backgroundColor: "#FFFF9C"}, 1000);	
};
/*
	unhighlights an element.	
*/		
JOBAD.UI.unhighlight = function(element){
	var element = JOBAD.refs.$(element);
	element
	.stop()
	.animate({
		backgroundColor: element.data("JOBAD.UI.highlight.orgColor"),
		finish: function(){
			element.removeData("JOBAD.UI.highlight.orgColor");
		}
	}, 1000);
		
};/* end   <JOBAD.ui.js> */
/* start <JOBAD.events.js> */
/*
	JOBAD 3 Events
	depends:
		JOBAD.core.modules.js
		JOBAD.core.events.js
		JOABD.core.js
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
				var element = JOBAD.refs.$(event.target); //The base element.  
				switch (event.which) {
					case 1:
						/* left mouse button => left click */
						me.Event.leftClick.trigger(element);
						event.stopPropagation(); //Not for the parent. 
						break;
					default:
						/* nothing */
				}
				root.trigger('JOBAD.Event', ['leftClick', element]);
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
			var evt = this.Event.leftClick.getResult(target);
			return evt;
		}
	}
};

/* onEvent */
JOBAD.Events.onEvent = 
{
	'default': function(){},
	'Setup': {
		'enable': function(root){
			var me = this;
			root.on('JOBAD.Event', function(jqe, event, args){
				me.Event.onEvent.trigger(event, args);
			});
		},
		'disable': function(root){
			root.off('JOBAD.Event');
		}
	},
	'namespace': 
	{
		
		'getResult': function(event, element){
			return this.modules.iterateAnd(function(module){
				module.onEvent.call(module, event, element, module.getJOBAD());
				return true;
			});
		},
		'trigger': function(event, element){
			return this.Event.onEvent.getResult(event, element);
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
				var res = me.Event.contextMenuEntries.getResult(target);
				root.trigger('JOBAD.Event', ['contextMenuEntries', target]);
				return res;
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
				return (JOBAD.refs._.isArray(entries))?entries:JOBAD.util.generateMenuList(entries);
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
	Wraps a menu function
	@param menu Menu to generate. 
	@returns the new representation. 
*/
JOBAD.util.fullWrap = function(menu, wrapper){
	var menu = (JOBAD.refs._.isArray(menu))?menu:JOBAD.util.generateMenuList(menu);
	var menu2 = [];
	for(var i=0;i<menu.length;i++){
		if(typeof menu[i][1] == 'function'){
			(function(){
				var org = menu[i][1];
				menu2.push([menu[i][0], function(){
					return wrapper(org, arguments)
				}]);
			})();
		} else {
			menu2.push([menu[i][0], JOBAD.util.fullWrap(menu[i][1])]);
		}
		
	}
	return menu2;
};

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
				var $element = JOBAD.refs.$(this);
				var res = me.Event.hoverText.trigger($element);
				if(res){//something happened here: dont trigger on parent
					event.stopPropagation();
				} else if(!$element.is(root)){ //I have nothing => trigger the parent
					JOBAD.refs.$(this).parent().trigger('mouseenter.JOBAD.hoverText', event); //Trigger parent if i'm not root. 	
				}
				root.trigger('JOBAD.Event', ['hoverText', $element]);
				return false;
			};


			var untrigger = function(event){
				return me.Event.hoverText.untrigger(JOBAD.refs.$(this));	
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
						res = JOBAD.refs.$("<p>").text(hoverText)			
					} else if(typeof hoverText != "boolean"){
						try{
							res = JOBAD.refs.$(hoverText);
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

			if(this.Event.hoverText.activeHoverElement instanceof JOBAD.refs.$)//something already active
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
				JOBAD.UI.hover.enable(EventResult.html(), "JOBAD_Hover");
			}, JOBAD.UI.hover.config.hoverDelay);

			source.data('JOBAD.hover.timerId', tid);//save timeout id
			return true;
						
		},
		'untrigger': function(source){
			if(typeof source == 'undefined'){
				if(this.Event.hoverText.activeHoverElement instanceof JOBAD.refs.$){
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

/* sidebar: onSideBarUpdate Event */
JOBAD.Events.onSideBarUpdate = 
{
	'default': function(){
		//Does nothing
	},
	'Setup': {
		'init': {
			/* Sidebar namespace */
			'Sidebar': {
				/*
					Redraws the sidebar. 
				*/
				'redraw': function(){
					if(typeof this.Sidebar.Elements == 'undefined'){
						this.Sidebar.Elements = {};
					}
					if(JOBAD.refs._.keys(this.Sidebar.Elements).length == 0){
						if(this.element.data("JOBAD.UI.Sidebar.active")){
							JOBAD.UI.Sidebar.unwrap(this.element);
						}	
					} else {
						if(!this.element.data("JOBAD.UI.Sidebar.active")){
							JOBAD.UI.Sidebar.wrap(this.element);
						}
						for(var id in this.Sidebar.Elements){
							var element = this.Sidebar.Elements[id];
							if(!element.data("JOBAD.Events.Sidebar.id")){
								this.Sidebar.Elements[id] = JOBAD.UI.Sidebar.addNotification(this.element, this.Sidebar.Elements[id], element.data("JOBAD.Events.Sidebar.config"));
							}
						}
					}
					JOBAD.UI.Sidebar.forceNotificationUpdate();
					this.Event.onSideBarUpdate.trigger();
				},
				/*
					Registers a new notification. 
					@param element Element to register notification on. 
					@param config
							config.class:	Notificaton class. Default: none. 
							config.icon:	Icon (Default: Based on notification class. 
							config.text:	Text
							config.menu:	Context Menu
							config.trace:	Trace the original element on hover?
							config.click:	Callback on click
					@return jQuery element used as identification. 
							
				*/
				'registerNotification': function(element, config){
					var me = this;
					if(typeof this.Sidebar.Elements == 'undefined'){
						this.Sidebar.Elements = {};
					}
					var element = JOBAD.refs.$(element);
					var id = (new Date()).getTime().toString();
					var config = (typeof config == 'undefined')?{}:config;
					config.menuThis = me;
					this.Sidebar.Elements[id] = element.data("JOBAD.Events.Sidebar.config", config);
					
					this.Sidebar.redraw();
					
					var sidebar_element = this.Sidebar.Elements[id].data("JOBAD.Events.Sidebar.id", id);

					sidebar_element.data("JOBAD.Events.Sidebar.element", element)					
					
					return sidebar_element;
				}, 
				/*
					removes a notification. 
					@param	item	Notification to remove. 
				*/
				'removeNotification': function(item){
					if(item instanceof JOBAD.refs.$){
						var id = item.data("JOBAD.Events.Sidebar.id");
						JOBAD.UI.Sidebar.removeNotification(item);
						delete this.Sidebar.Elements[id];
						this.Sidebar.redraw();
					} else {
						JOBAD.error("JOBAD Sidebar Error: Tried to remove invalid Element. ");
					}
				}	
			}
		},
		'enable': function(root){
			this.Event.onSideBarUpdate.enabled = true;
			
		},
		'disable': function(root){
			this.Event.onSideBarUpdate.enabled = undefined;
		}
	},
	'namespace': 
	{
		
		'getResult': function(){
			if(this.Event.onSideBarUpdate.enabled){
				this.modules.iterateAnd(function(module){
					module.onSideBarUpdate.call(module, module.getJOBAD());
					return true;
				});
			}
		},
		'trigger': function(){
			this.Event.onSideBarUpdate.getResult();
		}
	}
};

for(var key in JOBAD.Events){
	JOBAD.modules.cleanProperties.push(key);
}
/* end   <JOBAD.events.js> */
/* start <JOBAD.config.js> */
/*
	JOBAD Configuration
*/
JOBAD.storageBackend = {
	"getKey": function(key, def){
		var res = JOBAD.storageBackend.engines[JOBAD.config.storageBackend][0](key);
		if(typeof res == "string"){
			return JSON.parse(res);
		} else {
			return def;
		}
	}, 
	"setKey": function(key, value){return JOBAD.storageBackend.engines[JOBAD.config.storageBackend][1](key, JSON.stringify(value));}
}

JOBAD.storageBackend.engines = {
	"none": [function(key){}, function(key, value){}]
}

var configCache = {};

JOBAD.config.storageBackend = "none";

JOBAD.util.validateConfigSetting = function(obj, key, val){
	if(!obj.hasOwnProperty(key)){
		JOBAD.console.warn("Undefined user setting: "+key);
		return false;
	}
	var type = obj[key][0];
	var validator = obj[key][1];
	switch(type){
		case "string":
			if(typeof val != "string"){
				return false;
			}
			if(JOBAD.refs._.isRegExp(validator)){
				return validator.test(val);
			} else if(typeof validator == 'function') {
				return validator(val);
			} else {
				return true;
			}
			break;
		case "bool":
			return (typeof val == "boolean");
			break;
		case "integer":
			if(typeof val != "number" || val % 1 != 0){
				return false;
			}
			if (typeof validator == "function"){
				return validator(val);
			} else if(JOBAD.refs._.isArray(validator)){
				return (val >= validator[0] && val <= validator[1]);
			} else {
				return true;
			}
			break;
		case "number":
			if(typeof val != "number"){
				return false;
			}
			if (typeof validator == "function"){
				return validator(val);
			} else if(JOBAD.refs._.isArray(validator)){
				return (val >= validator[0] && val <= validator[1]);
			} else {
				return true;
			}
			break;
		case "list":
			return validator.indexOf(val) != -1;
			break;
		default:
			JOBAD.console.warn("Unknown configuration type '"+type+"' for user setting '"+key+"'");
			break;
	}
};

JOBAD.util.getDefaultConfigSetting = function(obj, key){
	if(!obj.hasOwnProperty(key)){
		JOBAD.console.warn("Undefined user setting: "+key);
		return;
	}
	var val = obj[key][2];
	if(JOBAD.util.validateConfigSetting(obj, key, val)){
		return val;
	} else {
		JOBAD.console.warn("Undefined user setting: "+obj);
	}
};

JOBAD.modules.extensions.config = {
	"required": false, //not required
	
	"validate": function(prop){return true; }, //anything is ok
	
	"init": function(available, value, originalObject, properObject){
		return available ? value : {};
	},
	
	"onLoad": function(value, properObject, loadedModule){
		var id = properObject.info.identifier;
		
		this.UserConfig = {};
		
		this.UserConfig.set = function(prop, val){
			if(this.UserConfig.canSet(prop, val)){
				configCache[id][prop] = val;
			} else {
				JOBAD.console.warn("Can not set user config '"+prop+"': Validation failure. ");
			}
			JOBAD.storageBackend.setKey(id, configCache[id]);
		};
		
		this.UserConfig.canSet = function(prop, val){
			return JOBAD.util.validateConfigSetting(value, prop, val);
		};
		
		this.UserConfig.get = function(prop){
			var res = configCache[id][prop];
			if(JOBAD.util.validateConfigSetting(value, prop, res)){
				return res;
			} else{
				JOBAD.console.log("Failed to access user setting '"+prop+"'");
			}
		};
		
		this.UserConfig.reset = function(prop){
			configCache[id] = JOBAD.storageBackend.getKey(id);
			if(typeof configCache[id] == "undefined"){
				configCache[id] = {};
				for(var key in value){
					configCache[id][key] = JOBAD.util.getDefaultConfigSetting(value, key);
				}
			}
		};
		
		if(!configCache.hasOwnProperty(id)){//not yet loaded by some other JOBAD
			this.UserConfig.reset();
		}
	}
}/* end   <JOBAD.config.js> */
/* start <JOBAD.wrap.js> */
/*
	JOBAD.wrap.js
	
	Included at the end of the build to register all ifaces..
*/
for(var key in JOBAD.modules.extensions){
	JOBAD.modules.cleanProperties.push(key);
}/* end   <JOBAD.wrap.js> */
return JOBAD;
})();
