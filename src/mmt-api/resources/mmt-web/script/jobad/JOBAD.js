/*
	JOBAD v3
	Development version
	built: Sun Feb 19 17:49:38 CET 2017

	
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

var JOBAD = (function(){
/* start <core/JOBAD.core.js> */
/*
	JOBAD 3 Core
	
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.

	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

/*
	JOBAD 3 Main Function
	Creates a new JOBAD instance on a specefied DOM element.
	@param element Element to link this element to. May be a DOM Element or a jQuery Object.
	@param config Configuration for this JOBAD Instance.

*/
var JOBAD = function(element){

	//create new instance of JOBAD
	if(!(this instanceof JOBAD)){
		return new JOBAD(element);
	}

	var me = this;

	this.ID = JOBAD.util.UID(); //assign an id to this JOBAD

	//Add init arguments
	this.args = [];
	for(var i=0;i<arguments.length;i++){
		this.args.push(arguments[i]);
	}

	//The element the current JOBAD instance works on.
	this.element = JOBAD.refs.$(element);

	if(this.element.length == 0){
		JOBAD.error("Can't create JOBADInstance: Element Collection seems to be empty. ");
		return;
	}

	if(this.element.length > 1){
		JOBAD.console.warn("Warning: More than one element specefied for JOBADInstance. This may cause problems with some modules. ");
	}

	if(JOBAD.util.isMarkedHidden(element)){
		JOBAD.error("Can't create JOBADInstance: Element marked as hidden. ");
		return;
	}

	/*
		Checks if this JOBAD Instance contains an element.
		@param	el	Element to check.
	*/
	this.contains = function(el){
		return JOBAD.util.containsAll(me.element, el, true);
	}

	//IFace extensions
	for(var i=0; i < JOBAD.ifaces.length; i++){
		var mod = JOBAD.ifaces[i];
		if(typeof mod == 'function'){
			mod.call(this, this, this.args);
		}
	}
};



JOBAD.ifaces = []; //JOBAD interfaces

/* JOBAD Version */
JOBAD.version = "3.2.3";

/*
	JOBAD.toString
*/
JOBAD.toString = function(){
	return "function(/* JOBAD "+JOBAD.version+" */){ [non-native non-trivial code] }";
};

JOBAD.toString.toString = JOBAD.toString; //self-reference!

/* JOBAD Global config */
JOBAD.config =
{
	    'debug': true, //Debugging enabled? (Logs etc)
	    'BootstrapScope': undefined //Scope for Bootstrap CSS
};

/*
	JOBAD.console: Mimics  or wraps the native console object if available and debugging is enabled.
*/
if(typeof console != "undefined"){//Console available

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

JOBAD.noConflict = function(){
	JOBAD.refs.$ = JOBAD.refs.$.noConflict();
	return JOBAD.refs.$;
}; //No conflict mode
/* end   <core/JOBAD.core.js> */
/* start <bundled/jquery-color/jquery.color.plus-names-2.1.2.js> */
/*!
 * jQuery Color Animations v2.1.2
 * https://github.com/jquery/jquery-color
 *
 * Copyright 2013 jQuery Foundation and other contributors
 * Released under the MIT license.
 * http://jquery.org/license
 *
 * Date: Wed Jan 16 08:47:09 2013 -0600
 */
(function( jQuery, undefined ) {

	var stepHooks = "backgroundColor borderBottomColor borderLeftColor borderRightColor borderTopColor color columnRuleColor outlineColor textDecorationColor textEmphasisColor",

	// plusequals test for += 100 -= 100
	rplusequals = /^([\-+])=\s*(\d+\.?\d*)/,
	// a set of RE's that can match strings and generate color tuples.
	stringParsers = [{
			re: /rgba?\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*(\d{1,3})\s*(?:,\s*(\d?(?:\.\d+)?)\s*)?\)/,
			parse: function( execResult ) {
				return [
					execResult[ 1 ],
					execResult[ 2 ],
					execResult[ 3 ],
					execResult[ 4 ]
				];
			}
		}, {
			re: /rgba?\(\s*(\d+(?:\.\d+)?)\%\s*,\s*(\d+(?:\.\d+)?)\%\s*,\s*(\d+(?:\.\d+)?)\%\s*(?:,\s*(\d?(?:\.\d+)?)\s*)?\)/,
			parse: function( execResult ) {
				return [
					execResult[ 1 ] * 2.55,
					execResult[ 2 ] * 2.55,
					execResult[ 3 ] * 2.55,
					execResult[ 4 ]
				];
			}
		}, {
			// this regex ignores A-F because it's compared against an already lowercased string
			re: /#([a-f0-9]{2})([a-f0-9]{2})([a-f0-9]{2})/,
			parse: function( execResult ) {
				return [
					parseInt( execResult[ 1 ], 16 ),
					parseInt( execResult[ 2 ], 16 ),
					parseInt( execResult[ 3 ], 16 )
				];
			}
		}, {
			// this regex ignores A-F because it's compared against an already lowercased string
			re: /#([a-f0-9])([a-f0-9])([a-f0-9])/,
			parse: function( execResult ) {
				return [
					parseInt( execResult[ 1 ] + execResult[ 1 ], 16 ),
					parseInt( execResult[ 2 ] + execResult[ 2 ], 16 ),
					parseInt( execResult[ 3 ] + execResult[ 3 ], 16 )
				];
			}
		}, {
			re: /hsla?\(\s*(\d+(?:\.\d+)?)\s*,\s*(\d+(?:\.\d+)?)\%\s*,\s*(\d+(?:\.\d+)?)\%\s*(?:,\s*(\d?(?:\.\d+)?)\s*)?\)/,
			space: "hsla",
			parse: function( execResult ) {
				return [
					execResult[ 1 ],
					execResult[ 2 ] / 100,
					execResult[ 3 ] / 100,
					execResult[ 4 ]
				];
			}
		}],

	// jQuery.Color( )
	color = jQuery.Color = function( color, green, blue, alpha ) {
		return new jQuery.Color.fn.parse( color, green, blue, alpha );
	},
	spaces = {
		rgba: {
			props: {
				red: {
					idx: 0,
					type: "byte"
				},
				green: {
					idx: 1,
					type: "byte"
				},
				blue: {
					idx: 2,
					type: "byte"
				}
			}
		},

		hsla: {
			props: {
				hue: {
					idx: 0,
					type: "degrees"
				},
				saturation: {
					idx: 1,
					type: "percent"
				},
				lightness: {
					idx: 2,
					type: "percent"
				}
			}
		}
	},
	propTypes = {
		"byte": {
			floor: true,
			max: 255
		},
		"percent": {
			max: 1
		},
		"degrees": {
			mod: 360,
			floor: true
		}
	},
	support = color.support = {},

	// element for support tests
	supportElem = jQuery( "<p>" )[ 0 ],

	// colors = jQuery.Color.names
	colors,

	// local aliases of functions called often
	each = jQuery.each;

// determine rgba support immediately
supportElem.style.cssText = "background-color:rgba(1,1,1,.5)";
support.rgba = supportElem.style.backgroundColor.indexOf( "rgba" ) > -1;

// define cache name and alpha properties
// for rgba and hsla spaces
each( spaces, function( spaceName, space ) {
	space.cache = "_" + spaceName;
	space.props.alpha = {
		idx: 3,
		type: "percent",
		def: 1
	};
});

function clamp( value, prop, allowEmpty ) {
	var type = propTypes[ prop.type ] || {};

	if ( value == null ) {
		return (allowEmpty || !prop.def) ? null : prop.def;
	}

	// ~~ is an short way of doing floor for positive numbers
	value = type.floor ? ~~value : parseFloat( value );

	// IE will pass in empty strings as value for alpha,
	// which will hit this case
	if ( isNaN( value ) ) {
		return prop.def;
	}

	if ( type.mod ) {
		// we add mod before modding to make sure that negatives values
		// get converted properly: -10 -> 350
		return (value + type.mod) % type.mod;
	}

	// for now all property types without mod have min and max
	return 0 > value ? 0 : type.max < value ? type.max : value;
}

function stringParse( string ) {
	var inst = color(),
		rgba = inst._rgba = [];

	string = string.toLowerCase();

	each( stringParsers, function( i, parser ) {
		var parsed,
			match = parser.re.exec( string ),
			values = match && parser.parse( match ),
			spaceName = parser.space || "rgba";

		if ( values ) {
			parsed = inst[ spaceName ]( values );

			// if this was an rgba parse the assignment might happen twice
			// oh well....
			inst[ spaces[ spaceName ].cache ] = parsed[ spaces[ spaceName ].cache ];
			rgba = inst._rgba = parsed._rgba;

			// exit each( stringParsers ) here because we matched
			return false;
		}
	});

	// Found a stringParser that handled it
	if ( rgba.length ) {

		// if this came from a parsed string, force "transparent" when alpha is 0
		// chrome, (and maybe others) return "transparent" as rgba(0,0,0,0)
		if ( rgba.join() === "0,0,0,0" ) {
			jQuery.extend( rgba, colors.transparent );
		}
		return inst;
	}

	// named colors
	return colors[ string ];
}

color.fn = jQuery.extend( color.prototype, {
	parse: function( red, green, blue, alpha ) {
		if ( red === undefined ) {
			this._rgba = [ null, null, null, null ];
			return this;
		}
		if ( red.jquery || red.nodeType ) {
			red = jQuery( red ).css( green );
			green = undefined;
		}

		var inst = this,
			type = jQuery.type( red ),
			rgba = this._rgba = [];

		// more than 1 argument specified - assume ( red, green, blue, alpha )
		if ( green !== undefined ) {
			red = [ red, green, blue, alpha ];
			type = "array";
		}

		if ( type === "string" ) {
			return this.parse( stringParse( red ) || colors._default );
		}

		if ( type === "array" ) {
			each( spaces.rgba.props, function( key, prop ) {
				rgba[ prop.idx ] = clamp( red[ prop.idx ], prop );
			});
			return this;
		}

		if ( type === "object" ) {
			if ( red instanceof color ) {
				each( spaces, function( spaceName, space ) {
					if ( red[ space.cache ] ) {
						inst[ space.cache ] = red[ space.cache ].slice();
					}
				});
			} else {
				each( spaces, function( spaceName, space ) {
					var cache = space.cache;
					each( space.props, function( key, prop ) {

						// if the cache doesn't exist, and we know how to convert
						if ( !inst[ cache ] && space.to ) {

							// if the value was null, we don't need to copy it
							// if the key was alpha, we don't need to copy it either
							if ( key === "alpha" || red[ key ] == null ) {
								return;
							}
							inst[ cache ] = space.to( inst._rgba );
						}

						// this is the only case where we allow nulls for ALL properties.
						// call clamp with alwaysAllowEmpty
						inst[ cache ][ prop.idx ] = clamp( red[ key ], prop, true );
					});

					// everything defined but alpha?
					if ( inst[ cache ] && jQuery.inArray( null, inst[ cache ].slice( 0, 3 ) ) < 0 ) {
						// use the default of 1
						inst[ cache ][ 3 ] = 1;
						if ( space.from ) {
							inst._rgba = space.from( inst[ cache ] );
						}
					}
				});
			}
			return this;
		}
	},
	is: function( compare ) {
		var is = color( compare ),
			same = true,
			inst = this;

		each( spaces, function( _, space ) {
			var localCache,
				isCache = is[ space.cache ];
			if (isCache) {
				localCache = inst[ space.cache ] || space.to && space.to( inst._rgba ) || [];
				each( space.props, function( _, prop ) {
					if ( isCache[ prop.idx ] != null ) {
						same = ( isCache[ prop.idx ] === localCache[ prop.idx ] );
						return same;
					}
				});
			}
			return same;
		});
		return same;
	},
	_space: function() {
		var used = [],
			inst = this;
		each( spaces, function( spaceName, space ) {
			if ( inst[ space.cache ] ) {
				used.push( spaceName );
			}
		});
		return used.pop();
	},
	transition: function( other, distance ) {
		var end = color( other ),
			spaceName = end._space(),
			space = spaces[ spaceName ],
			startColor = this.alpha() === 0 ? color( "transparent" ) : this,
			start = startColor[ space.cache ] || space.to( startColor._rgba ),
			result = start.slice();

		end = end[ space.cache ];
		each( space.props, function( key, prop ) {
			var index = prop.idx,
				startValue = start[ index ],
				endValue = end[ index ],
				type = propTypes[ prop.type ] || {};

			// if null, don't override start value
			if ( endValue === null ) {
				return;
			}
			// if null - use end
			if ( startValue === null ) {
				result[ index ] = endValue;
			} else {
				if ( type.mod ) {
					if ( endValue - startValue > type.mod / 2 ) {
						startValue += type.mod;
					} else if ( startValue - endValue > type.mod / 2 ) {
						startValue -= type.mod;
					}
				}
				result[ index ] = clamp( ( endValue - startValue ) * distance + startValue, prop );
			}
		});
		return this[ spaceName ]( result );
	},
	blend: function( opaque ) {
		// if we are already opaque - return ourself
		if ( this._rgba[ 3 ] === 1 ) {
			return this;
		}

		var rgb = this._rgba.slice(),
			a = rgb.pop(),
			blend = color( opaque )._rgba;

		return color( jQuery.map( rgb, function( v, i ) {
			return ( 1 - a ) * blend[ i ] + a * v;
		}));
	},
	toRgbaString: function() {
		var prefix = "rgba(",
			rgba = jQuery.map( this._rgba, function( v, i ) {
				return v == null ? ( i > 2 ? 1 : 0 ) : v;
			});

		if ( rgba[ 3 ] === 1 ) {
			rgba.pop();
			prefix = "rgb(";
		}

		return prefix + rgba.join() + ")";
	},
	toHslaString: function() {
		var prefix = "hsla(",
			hsla = jQuery.map( this.hsla(), function( v, i ) {
				if ( v == null ) {
					v = i > 2 ? 1 : 0;
				}

				// catch 1 and 2
				if ( i && i < 3 ) {
					v = Math.round( v * 100 ) + "%";
				}
				return v;
			});

		if ( hsla[ 3 ] === 1 ) {
			hsla.pop();
			prefix = "hsl(";
		}
		return prefix + hsla.join() + ")";
	},
	toHexString: function( includeAlpha ) {
		var rgba = this._rgba.slice(),
			alpha = rgba.pop();

		if ( includeAlpha ) {
			rgba.push( ~~( alpha * 255 ) );
		}

		return "#" + jQuery.map( rgba, function( v ) {

			// default to 0 when nulls exist
			v = ( v || 0 ).toString( 16 );
			return v.length === 1 ? "0" + v : v;
		}).join("");
	},
	toString: function() {
		return this._rgba[ 3 ] === 0 ? "transparent" : this.toRgbaString();
	}
});
color.fn.parse.prototype = color.fn;

// hsla conversions adapted from:
// https://code.google.com/p/maashaack/source/browse/packages/graphics/trunk/src/graphics/colors/HUE2RGB.as?r=5021

function hue2rgb( p, q, h ) {
	h = ( h + 1 ) % 1;
	if ( h * 6 < 1 ) {
		return p + (q - p) * h * 6;
	}
	if ( h * 2 < 1) {
		return q;
	}
	if ( h * 3 < 2 ) {
		return p + (q - p) * ((2/3) - h) * 6;
	}
	return p;
}

spaces.hsla.to = function ( rgba ) {
	if ( rgba[ 0 ] == null || rgba[ 1 ] == null || rgba[ 2 ] == null ) {
		return [ null, null, null, rgba[ 3 ] ];
	}
	var r = rgba[ 0 ] / 255,
		g = rgba[ 1 ] / 255,
		b = rgba[ 2 ] / 255,
		a = rgba[ 3 ],
		max = Math.max( r, g, b ),
		min = Math.min( r, g, b ),
		diff = max - min,
		add = max + min,
		l = add * 0.5,
		h, s;

	if ( min === max ) {
		h = 0;
	} else if ( r === max ) {
		h = ( 60 * ( g - b ) / diff ) + 360;
	} else if ( g === max ) {
		h = ( 60 * ( b - r ) / diff ) + 120;
	} else {
		h = ( 60 * ( r - g ) / diff ) + 240;
	}

	// chroma (diff) == 0 means greyscale which, by definition, saturation = 0%
	// otherwise, saturation is based on the ratio of chroma (diff) to lightness (add)
	if ( diff === 0 ) {
		s = 0;
	} else if ( l <= 0.5 ) {
		s = diff / add;
	} else {
		s = diff / ( 2 - add );
	}
	return [ Math.round(h) % 360, s, l, a == null ? 1 : a ];
};

spaces.hsla.from = function ( hsla ) {
	if ( hsla[ 0 ] == null || hsla[ 1 ] == null || hsla[ 2 ] == null ) {
		return [ null, null, null, hsla[ 3 ] ];
	}
	var h = hsla[ 0 ] / 360,
		s = hsla[ 1 ],
		l = hsla[ 2 ],
		a = hsla[ 3 ],
		q = l <= 0.5 ? l * ( 1 + s ) : l + s - l * s,
		p = 2 * l - q;

	return [
		Math.round( hue2rgb( p, q, h + ( 1 / 3 ) ) * 255 ),
		Math.round( hue2rgb( p, q, h ) * 255 ),
		Math.round( hue2rgb( p, q, h - ( 1 / 3 ) ) * 255 ),
		a
	];
};


each( spaces, function( spaceName, space ) {
	var props = space.props,
		cache = space.cache,
		to = space.to,
		from = space.from;

	// makes rgba() and hsla()
	color.fn[ spaceName ] = function( value ) {

		// generate a cache for this space if it doesn't exist
		if ( to && !this[ cache ] ) {
			this[ cache ] = to( this._rgba );
		}
		if ( value === undefined ) {
			return this[ cache ].slice();
		}

		var ret,
			type = jQuery.type( value ),
			arr = ( type === "array" || type === "object" ) ? value : arguments,
			local = this[ cache ].slice();

		each( props, function( key, prop ) {
			var val = arr[ type === "object" ? key : prop.idx ];
			if ( val == null ) {
				val = local[ prop.idx ];
			}
			local[ prop.idx ] = clamp( val, prop );
		});

		if ( from ) {
			ret = color( from( local ) );
			ret[ cache ] = local;
			return ret;
		} else {
			return color( local );
		}
	};

	// makes red() green() blue() alpha() hue() saturation() lightness()
	each( props, function( key, prop ) {
		// alpha is included in more than one space
		if ( color.fn[ key ] ) {
			return;
		}
		color.fn[ key ] = function( value ) {
			var vtype = jQuery.type( value ),
				fn = ( key === "alpha" ? ( this._hsla ? "hsla" : "rgba" ) : spaceName ),
				local = this[ fn ](),
				cur = local[ prop.idx ],
				match;

			if ( vtype === "undefined" ) {
				return cur;
			}

			if ( vtype === "function" ) {
				value = value.call( this, cur );
				vtype = jQuery.type( value );
			}
			if ( value == null && prop.empty ) {
				return this;
			}
			if ( vtype === "string" ) {
				match = rplusequals.exec( value );
				if ( match ) {
					value = cur + parseFloat( match[ 2 ] ) * ( match[ 1 ] === "+" ? 1 : -1 );
				}
			}
			local[ prop.idx ] = value;
			return this[ fn ]( local );
		};
	});
});

// add cssHook and .fx.step function for each named hook.
// accept a space separated string of properties
color.hook = function( hook ) {
	var hooks = hook.split( " " );
	each( hooks, function( i, hook ) {
		jQuery.cssHooks[ hook ] = {
			set: function( elem, value ) {
				var parsed, curElem,
					backgroundColor = "";

				if ( value !== "transparent" && ( jQuery.type( value ) !== "string" || ( parsed = stringParse( value ) ) ) ) {
					value = color( parsed || value );
					if ( !support.rgba && value._rgba[ 3 ] !== 1 ) {
						curElem = hook === "backgroundColor" ? elem.parentNode : elem;
						while (
							(backgroundColor === "" || backgroundColor === "transparent") &&
							curElem && curElem.style
						) {
							try {
								backgroundColor = jQuery.css( curElem, "backgroundColor" );
								curElem = curElem.parentNode;
							} catch ( e ) {
							}
						}

						value = value.blend( backgroundColor && backgroundColor !== "transparent" ?
							backgroundColor :
							"_default" );
					}

					value = value.toRgbaString();
				}
				try {
					elem.style[ hook ] = value;
				} catch( e ) {
					// wrapped to prevent IE from throwing errors on "invalid" values like 'auto' or 'inherit'
				}
			}
		};
		jQuery.fx.step[ hook ] = function( fx ) {
			if ( !fx.colorInit ) {
				fx.start = color( fx.elem, hook );
				fx.end = color( fx.end );
				fx.colorInit = true;
			}
			jQuery.cssHooks[ hook ].set( fx.elem, fx.start.transition( fx.end, fx.pos ) );
		};
	});

};

color.hook( stepHooks );

jQuery.cssHooks.borderColor = {
	expand: function( value ) {
		var expanded = {};

		each( [ "Top", "Right", "Bottom", "Left" ], function( i, part ) {
			expanded[ "border" + part + "Color" ] = value;
		});
		return expanded;
	}
};

// Basic color names only.
// Usage of any of the other color names requires adding yourself or including
// jquery.color.svg-names.js.
colors = jQuery.Color.names = {
	// 4.1. Basic color keywords
	aqua: "#00ffff",
	black: "#000000",
	blue: "#0000ff",
	fuchsia: "#ff00ff",
	gray: "#808080",
	green: "#008000",
	lime: "#00ff00",
	maroon: "#800000",
	navy: "#000080",
	olive: "#808000",
	purple: "#800080",
	red: "#ff0000",
	silver: "#c0c0c0",
	teal: "#008080",
	white: "#ffffff",
	yellow: "#ffff00",

	// 4.2.3. "transparent" color keyword
	transparent: [ null, null, null, 0 ],

	_default: "#ffffff"
};

})( jQuery );

/*!
 * jQuery Color Animations v2.1.2 - SVG Color Names
 * https://github.com/jquery/jquery-color
 *
 * Remaining HTML/CSS color names per W3C's CSS Color Module Level 3.
 * http://www.w3.org/TR/css3-color/#svg-color
 *
 * Copyright 2013 jQuery Foundation and other contributors
 * Released under the MIT license.
 * http://jquery.org/license
 *
 * Date: Wed Jan 16 08:47:09 2013 -0600
 */
jQuery.extend( jQuery.Color.names, {
	// 4.3. Extended color keywords (minus the basic ones in core color plugin)
	aliceblue: "#f0f8ff",
	antiquewhite: "#faebd7",
	aquamarine: "#7fffd4",
	azure: "#f0ffff",
	beige: "#f5f5dc",
	bisque: "#ffe4c4",
	blanchedalmond: "#ffebcd",
	blueviolet: "#8a2be2",
	brown: "#a52a2a",
	burlywood: "#deb887",
	cadetblue: "#5f9ea0",
	chartreuse: "#7fff00",
	chocolate: "#d2691e",
	coral: "#ff7f50",
	cornflowerblue: "#6495ed",
	cornsilk: "#fff8dc",
	crimson: "#dc143c",
	cyan: "#00ffff",
	darkblue: "#00008b",
	darkcyan: "#008b8b",
	darkgoldenrod: "#b8860b",
	darkgray: "#a9a9a9",
	darkgreen: "#006400",
	darkgrey: "#a9a9a9",
	darkkhaki: "#bdb76b",
	darkmagenta: "#8b008b",
	darkolivegreen: "#556b2f",
	darkorange: "#ff8c00",
	darkorchid: "#9932cc",
	darkred: "#8b0000",
	darksalmon: "#e9967a",
	darkseagreen: "#8fbc8f",
	darkslateblue: "#483d8b",
	darkslategray: "#2f4f4f",
	darkslategrey: "#2f4f4f",
	darkturquoise: "#00ced1",
	darkviolet: "#9400d3",
	deeppink: "#ff1493",
	deepskyblue: "#00bfff",
	dimgray: "#696969",
	dimgrey: "#696969",
	dodgerblue: "#1e90ff",
	firebrick: "#b22222",
	floralwhite: "#fffaf0",
	forestgreen: "#228b22",
	gainsboro: "#dcdcdc",
	ghostwhite: "#f8f8ff",
	gold: "#ffd700",
	goldenrod: "#daa520",
	greenyellow: "#adff2f",
	grey: "#808080",
	honeydew: "#f0fff0",
	hotpink: "#ff69b4",
	indianred: "#cd5c5c",
	indigo: "#4b0082",
	ivory: "#fffff0",
	khaki: "#f0e68c",
	lavender: "#e6e6fa",
	lavenderblush: "#fff0f5",
	lawngreen: "#7cfc00",
	lemonchiffon: "#fffacd",
	lightblue: "#add8e6",
	lightcoral: "#f08080",
	lightcyan: "#e0ffff",
	lightgoldenrodyellow: "#fafad2",
	lightgray: "#d3d3d3",
	lightgreen: "#90ee90",
	lightgrey: "#d3d3d3",
	lightpink: "#ffb6c1",
	lightsalmon: "#ffa07a",
	lightseagreen: "#20b2aa",
	lightskyblue: "#87cefa",
	lightslategray: "#778899",
	lightslategrey: "#778899",
	lightsteelblue: "#b0c4de",
	lightyellow: "#ffffe0",
	limegreen: "#32cd32",
	linen: "#faf0e6",
	mediumaquamarine: "#66cdaa",
	mediumblue: "#0000cd",
	mediumorchid: "#ba55d3",
	mediumpurple: "#9370db",
	mediumseagreen: "#3cb371",
	mediumslateblue: "#7b68ee",
	mediumspringgreen: "#00fa9a",
	mediumturquoise: "#48d1cc",
	mediumvioletred: "#c71585",
	midnightblue: "#191970",
	mintcream: "#f5fffa",
	mistyrose: "#ffe4e1",
	moccasin: "#ffe4b5",
	navajowhite: "#ffdead",
	oldlace: "#fdf5e6",
	olivedrab: "#6b8e23",
	orange: "#ffa500",
	orangered: "#ff4500",
	orchid: "#da70d6",
	palegoldenrod: "#eee8aa",
	palegreen: "#98fb98",
	paleturquoise: "#afeeee",
	palevioletred: "#db7093",
	papayawhip: "#ffefd5",
	peachpuff: "#ffdab9",
	peru: "#cd853f",
	pink: "#ffc0cb",
	plum: "#dda0dd",
	powderblue: "#b0e0e6",
	rosybrown: "#bc8f8f",
	royalblue: "#4169e1",
	saddlebrown: "#8b4513",
	salmon: "#fa8072",
	sandybrown: "#f4a460",
	seagreen: "#2e8b57",
	seashell: "#fff5ee",
	sienna: "#a0522d",
	skyblue: "#87ceeb",
	slateblue: "#6a5acd",
	slategray: "#708090",
	slategrey: "#708090",
	snow: "#fffafa",
	springgreen: "#00ff7f",
	steelblue: "#4682b4",
	tan: "#d2b48c",
	thistle: "#d8bfd8",
	tomato: "#ff6347",
	turquoise: "#40e0d0",
	violet: "#ee82ee",
	wheat: "#f5deb3",
	whitesmoke: "#f5f5f5",
	yellowgreen: "#9acd32"
});
/* end   <bundled/jquery-color/jquery.color.plus-names-2.1.2.js> */
/* start <bundled/underscore/underscore.js> */
//     Underscore.js 1.5.1
//     http://underscorejs.org
//     (c) 2009-2013 Jeremy Ashkenas, DocumentCloud and Investigative Reporters & Editors
//     Underscore may be freely distributed under the MIT license.

(function() {

  // Baseline setup
  // --------------

  // Establish the root object, `window` in the browser, or `global` on the server.
  var root = this;

  // Save the previous value of the `_` variable.
  var previousUnderscore = root._;

  // Establish the object that gets returned to break out of a loop iteration.
  var breaker = {};

  // Save bytes in the minified (but not gzipped) version:
  var ArrayProto = Array.prototype, ObjProto = Object.prototype, FuncProto = Function.prototype;

  // Create quick reference variables for speed access to core prototypes.
  var
    push             = ArrayProto.push,
    slice            = ArrayProto.slice,
    concat           = ArrayProto.concat,
    toString         = ObjProto.toString,
    hasOwnProperty   = ObjProto.hasOwnProperty;

  // All **ECMAScript 5** native function implementations that we hope to use
  // are declared here.
  var
    nativeForEach      = ArrayProto.forEach,
    nativeMap          = ArrayProto.map,
    nativeReduce       = ArrayProto.reduce,
    nativeReduceRight  = ArrayProto.reduceRight,
    nativeFilter       = ArrayProto.filter,
    nativeEvery        = ArrayProto.every,
    nativeSome         = ArrayProto.some,
    nativeIndexOf      = ArrayProto.indexOf,
    nativeLastIndexOf  = ArrayProto.lastIndexOf,
    nativeIsArray      = Array.isArray,
    nativeKeys         = Object.keys,
    nativeBind         = FuncProto.bind;

  // Create a safe reference to the Underscore object for use below.
  var _ = function(obj) {
    if (obj instanceof _) return obj;
    if (!(this instanceof _)) return new _(obj);
    this._wrapped = obj;
  };

  // Export the Underscore object for **Node.js**, with
  // backwards-compatibility for the old `require()` API. If we're in
  // the browser, add `_` as a global object via a string identifier,
  // for Closure Compiler "advanced" mode.
  if (typeof exports !== 'undefined') {
    if (typeof module !== 'undefined' && module.exports) {
      exports = module.exports = _;
    }
    exports._ = _;
  } else {
    root._ = _;
  }

  // Current version.
  _.VERSION = '1.5.1';

  // Collection Functions
  // --------------------

  // The cornerstone, an `each` implementation, aka `forEach`.
  // Handles objects with the built-in `forEach`, arrays, and raw objects.
  // Delegates to **ECMAScript 5**'s native `forEach` if available.
  var each = _.each = _.forEach = function(obj, iterator, context) {
    if (obj == null) return;
    if (nativeForEach && obj.forEach === nativeForEach) {
      obj.forEach(iterator, context);
    } else if (obj.length === +obj.length) {
      for (var i = 0, l = obj.length; i < l; i++) {
        if (iterator.call(context, obj[i], i, obj) === breaker) return;
      }
    } else {
      for (var key in obj) {
        if (_.has(obj, key)) {
          if (iterator.call(context, obj[key], key, obj) === breaker) return;
        }
      }
    }
  };

  // Return the results of applying the iterator to each element.
  // Delegates to **ECMAScript 5**'s native `map` if available.
  _.map = _.collect = function(obj, iterator, context) {
    var results = [];
    if (obj == null) return results;
    if (nativeMap && obj.map === nativeMap) return obj.map(iterator, context);
    each(obj, function(value, index, list) {
      results.push(iterator.call(context, value, index, list));
    });
    return results;
  };

  var reduceError = 'Reduce of empty array with no initial value';

  // **Reduce** builds up a single result from a list of values, aka `inject`,
  // or `foldl`. Delegates to **ECMAScript 5**'s native `reduce` if available.
  _.reduce = _.foldl = _.inject = function(obj, iterator, memo, context) {
    var initial = arguments.length > 2;
    if (obj == null) obj = [];
    if (nativeReduce && obj.reduce === nativeReduce) {
      if (context) iterator = _.bind(iterator, context);
      return initial ? obj.reduce(iterator, memo) : obj.reduce(iterator);
    }
    each(obj, function(value, index, list) {
      if (!initial) {
        memo = value;
        initial = true;
      } else {
        memo = iterator.call(context, memo, value, index, list);
      }
    });
    if (!initial) throw new TypeError(reduceError);
    return memo;
  };

  // The right-associative version of reduce, also known as `foldr`.
  // Delegates to **ECMAScript 5**'s native `reduceRight` if available.
  _.reduceRight = _.foldr = function(obj, iterator, memo, context) {
    var initial = arguments.length > 2;
    if (obj == null) obj = [];
    if (nativeReduceRight && obj.reduceRight === nativeReduceRight) {
      if (context) iterator = _.bind(iterator, context);
      return initial ? obj.reduceRight(iterator, memo) : obj.reduceRight(iterator);
    }
    var length = obj.length;
    if (length !== +length) {
      var keys = _.keys(obj);
      length = keys.length;
    }
    each(obj, function(value, index, list) {
      index = keys ? keys[--length] : --length;
      if (!initial) {
        memo = obj[index];
        initial = true;
      } else {
        memo = iterator.call(context, memo, obj[index], index, list);
      }
    });
    if (!initial) throw new TypeError(reduceError);
    return memo;
  };

  // Return the first value which passes a truth test. Aliased as `detect`.
  _.find = _.detect = function(obj, iterator, context) {
    var result;
    any(obj, function(value, index, list) {
      if (iterator.call(context, value, index, list)) {
        result = value;
        return true;
      }
    });
    return result;
  };

  // Return all the elements that pass a truth test.
  // Delegates to **ECMAScript 5**'s native `filter` if available.
  // Aliased as `select`.
  _.filter = _.select = function(obj, iterator, context) {
    var results = [];
    if (obj == null) return results;
    if (nativeFilter && obj.filter === nativeFilter) return obj.filter(iterator, context);
    each(obj, function(value, index, list) {
      if (iterator.call(context, value, index, list)) results.push(value);
    });
    return results;
  };

  // Return all the elements for which a truth test fails.
  _.reject = function(obj, iterator, context) {
    return _.filter(obj, function(value, index, list) {
      return !iterator.call(context, value, index, list);
    }, context);
  };

  // Determine whether all of the elements match a truth test.
  // Delegates to **ECMAScript 5**'s native `every` if available.
  // Aliased as `all`.
  _.every = _.all = function(obj, iterator, context) {
    iterator || (iterator = _.identity);
    var result = true;
    if (obj == null) return result;
    if (nativeEvery && obj.every === nativeEvery) return obj.every(iterator, context);
    each(obj, function(value, index, list) {
      if (!(result = result && iterator.call(context, value, index, list))) return breaker;
    });
    return !!result;
  };

  // Determine if at least one element in the object matches a truth test.
  // Delegates to **ECMAScript 5**'s native `some` if available.
  // Aliased as `any`.
  var any = _.some = _.any = function(obj, iterator, context) {
    iterator || (iterator = _.identity);
    var result = false;
    if (obj == null) return result;
    if (nativeSome && obj.some === nativeSome) return obj.some(iterator, context);
    each(obj, function(value, index, list) {
      if (result || (result = iterator.call(context, value, index, list))) return breaker;
    });
    return !!result;
  };

  // Determine if the array or object contains a given value (using `===`).
  // Aliased as `include`.
  _.contains = _.include = function(obj, target) {
    if (obj == null) return false;
    if (nativeIndexOf && obj.indexOf === nativeIndexOf) return obj.indexOf(target) != -1;
    return any(obj, function(value) {
      return value === target;
    });
  };

  // Invoke a method (with arguments) on every item in a collection.
  _.invoke = function(obj, method) {
    var args = slice.call(arguments, 2);
    var isFunc = _.isFunction(method);
    return _.map(obj, function(value) {
      return (isFunc ? method : value[method]).apply(value, args);
    });
  };

  // Convenience version of a common use case of `map`: fetching a property.
  _.pluck = function(obj, key) {
    return _.map(obj, function(value){ return value[key]; });
  };

  // Convenience version of a common use case of `filter`: selecting only objects
  // containing specific `key:value` pairs.
  _.where = function(obj, attrs, first) {
    if (_.isEmpty(attrs)) return first ? void 0 : [];
    return _[first ? 'find' : 'filter'](obj, function(value) {
      for (var key in attrs) {
        if (attrs[key] !== value[key]) return false;
      }
      return true;
    });
  };

  // Convenience version of a common use case of `find`: getting the first object
  // containing specific `key:value` pairs.
  _.findWhere = function(obj, attrs) {
    return _.where(obj, attrs, true);
  };

  // Return the maximum element or (element-based computation).
  // Can't optimize arrays of integers longer than 65,535 elements.
  // See [WebKit Bug 80797](https://bugs.webkit.org/show_bug.cgi?id=80797)
  _.max = function(obj, iterator, context) {
    if (!iterator && _.isArray(obj) && obj[0] === +obj[0] && obj.length < 65535) {
      return Math.max.apply(Math, obj);
    }
    if (!iterator && _.isEmpty(obj)) return -Infinity;
    var result = {computed : -Infinity, value: -Infinity};
    each(obj, function(value, index, list) {
      var computed = iterator ? iterator.call(context, value, index, list) : value;
      computed > result.computed && (result = {value : value, computed : computed});
    });
    return result.value;
  };

  // Return the minimum element (or element-based computation).
  _.min = function(obj, iterator, context) {
    if (!iterator && _.isArray(obj) && obj[0] === +obj[0] && obj.length < 65535) {
      return Math.min.apply(Math, obj);
    }
    if (!iterator && _.isEmpty(obj)) return Infinity;
    var result = {computed : Infinity, value: Infinity};
    each(obj, function(value, index, list) {
      var computed = iterator ? iterator.call(context, value, index, list) : value;
      computed < result.computed && (result = {value : value, computed : computed});
    });
    return result.value;
  };

  // Shuffle an array.
  _.shuffle = function(obj) {
    var rand;
    var index = 0;
    var shuffled = [];
    each(obj, function(value) {
      rand = _.random(index++);
      shuffled[index - 1] = shuffled[rand];
      shuffled[rand] = value;
    });
    return shuffled;
  };

  // An internal function to generate lookup iterators.
  var lookupIterator = function(value) {
    return _.isFunction(value) ? value : function(obj){ return obj[value]; };
  };

  // Sort the object's values by a criterion produced by an iterator.
  _.sortBy = function(obj, value, context) {
    var iterator = lookupIterator(value);
    return _.pluck(_.map(obj, function(value, index, list) {
      return {
        value : value,
        index : index,
        criteria : iterator.call(context, value, index, list)
      };
    }).sort(function(left, right) {
      var a = left.criteria;
      var b = right.criteria;
      if (a !== b) {
        if (a > b || a === void 0) return 1;
        if (a < b || b === void 0) return -1;
      }
      return left.index < right.index ? -1 : 1;
    }), 'value');
  };

  // An internal function used for aggregate "group by" operations.
  var group = function(obj, value, context, behavior) {
    var result = {};
    var iterator = lookupIterator(value == null ? _.identity : value);
    each(obj, function(value, index) {
      var key = iterator.call(context, value, index, obj);
      behavior(result, key, value);
    });
    return result;
  };

  // Groups the object's values by a criterion. Pass either a string attribute
  // to group by, or a function that returns the criterion.
  _.groupBy = function(obj, value, context) {
    return group(obj, value, context, function(result, key, value) {
      (_.has(result, key) ? result[key] : (result[key] = [])).push(value);
    });
  };

  // Counts instances of an object that group by a certain criterion. Pass
  // either a string attribute to count by, or a function that returns the
  // criterion.
  _.countBy = function(obj, value, context) {
    return group(obj, value, context, function(result, key) {
      if (!_.has(result, key)) result[key] = 0;
      result[key]++;
    });
  };

  // Use a comparator function to figure out the smallest index at which
  // an object should be inserted so as to maintain order. Uses binary search.
  _.sortedIndex = function(array, obj, iterator, context) {
    iterator = iterator == null ? _.identity : lookupIterator(iterator);
    var value = iterator.call(context, obj);
    var low = 0, high = array.length;
    while (low < high) {
      var mid = (low + high) >>> 1;
      iterator.call(context, array[mid]) < value ? low = mid + 1 : high = mid;
    }
    return low;
  };

  // Safely create a real, live array from anything iterable.
  _.toArray = function(obj) {
    if (!obj) return [];
    if (_.isArray(obj)) return slice.call(obj);
    if (obj.length === +obj.length) return _.map(obj, _.identity);
    return _.values(obj);
  };

  // Return the number of elements in an object.
  _.size = function(obj) {
    if (obj == null) return 0;
    return (obj.length === +obj.length) ? obj.length : _.keys(obj).length;
  };

  // Array Functions
  // ---------------

  // Get the first element of an array. Passing **n** will return the first N
  // values in the array. Aliased as `head` and `take`. The **guard** check
  // allows it to work with `_.map`.
  _.first = _.head = _.take = function(array, n, guard) {
    if (array == null) return void 0;
    return (n != null) && !guard ? slice.call(array, 0, n) : array[0];
  };

  // Returns everything but the last entry of the array. Especially useful on
  // the arguments object. Passing **n** will return all the values in
  // the array, excluding the last N. The **guard** check allows it to work with
  // `_.map`.
  _.initial = function(array, n, guard) {
    return slice.call(array, 0, array.length - ((n == null) || guard ? 1 : n));
  };

  // Get the last element of an array. Passing **n** will return the last N
  // values in the array. The **guard** check allows it to work with `_.map`.
  _.last = function(array, n, guard) {
    if (array == null) return void 0;
    if ((n != null) && !guard) {
      return slice.call(array, Math.max(array.length - n, 0));
    } else {
      return array[array.length - 1];
    }
  };

  // Returns everything but the first entry of the array. Aliased as `tail` and `drop`.
  // Especially useful on the arguments object. Passing an **n** will return
  // the rest N values in the array. The **guard**
  // check allows it to work with `_.map`.
  _.rest = _.tail = _.drop = function(array, n, guard) {
    return slice.call(array, (n == null) || guard ? 1 : n);
  };

  // Trim out all falsy values from an array.
  _.compact = function(array) {
    return _.filter(array, _.identity);
  };

  // Internal implementation of a recursive `flatten` function.
  var flatten = function(input, shallow, output) {
    if (shallow && _.every(input, _.isArray)) {
      return concat.apply(output, input);
    }
    each(input, function(value) {
      if (_.isArray(value) || _.isArguments(value)) {
        shallow ? push.apply(output, value) : flatten(value, shallow, output);
      } else {
        output.push(value);
      }
    });
    return output;
  };

  // Return a completely flattened version of an array.
  _.flatten = function(array, shallow) {
    return flatten(array, shallow, []);
  };

  // Return a version of the array that does not contain the specified value(s).
  _.without = function(array) {
    return _.difference(array, slice.call(arguments, 1));
  };

  // Produce a duplicate-free version of the array. If the array has already
  // been sorted, you have the option of using a faster algorithm.
  // Aliased as `unique`.
  _.uniq = _.unique = function(array, isSorted, iterator, context) {
    if (_.isFunction(isSorted)) {
      context = iterator;
      iterator = isSorted;
      isSorted = false;
    }
    var initial = iterator ? _.map(array, iterator, context) : array;
    var results = [];
    var seen = [];
    each(initial, function(value, index) {
      if (isSorted ? (!index || seen[seen.length - 1] !== value) : !_.contains(seen, value)) {
        seen.push(value);
        results.push(array[index]);
      }
    });
    return results;
  };

  // Produce an array that contains the union: each distinct element from all of
  // the passed-in arrays.
  _.union = function() {
    return _.uniq(_.flatten(arguments, true));
  };

  // Produce an array that contains every item shared between all the
  // passed-in arrays.
  _.intersection = function(array) {
    var rest = slice.call(arguments, 1);
    return _.filter(_.uniq(array), function(item) {
      return _.every(rest, function(other) {
        return _.indexOf(other, item) >= 0;
      });
    });
  };

  // Take the difference between one array and a number of other arrays.
  // Only the elements present in just the first array will remain.
  _.difference = function(array) {
    var rest = concat.apply(ArrayProto, slice.call(arguments, 1));
    return _.filter(array, function(value){ return !_.contains(rest, value); });
  };

  // Zip together multiple lists into a single array -- elements that share
  // an index go together.
  _.zip = function() {
    var length = _.max(_.pluck(arguments, "length").concat(0));
    var results = new Array(length);
    for (var i = 0; i < length; i++) {
      results[i] = _.pluck(arguments, '' + i);
    }
    return results;
  };

  // Converts lists into objects. Pass either a single array of `[key, value]`
  // pairs, or two parallel arrays of the same length -- one of keys, and one of
  // the corresponding values.
  _.object = function(list, values) {
    if (list == null) return {};
    var result = {};
    for (var i = 0, l = list.length; i < l; i++) {
      if (values) {
        result[list[i]] = values[i];
      } else {
        result[list[i][0]] = list[i][1];
      }
    }
    return result;
  };

  // If the browser doesn't supply us with indexOf (I'm looking at you, **MSIE**),
  // we need this function. Return the position of the first occurrence of an
  // item in an array, or -1 if the item is not included in the array.
  // Delegates to **ECMAScript 5**'s native `indexOf` if available.
  // If the array is large and already in sort order, pass `true`
  // for **isSorted** to use binary search.
  _.indexOf = function(array, item, isSorted) {
    if (array == null) return -1;
    var i = 0, l = array.length;
    if (isSorted) {
      if (typeof isSorted == 'number') {
        i = (isSorted < 0 ? Math.max(0, l + isSorted) : isSorted);
      } else {
        i = _.sortedIndex(array, item);
        return array[i] === item ? i : -1;
      }
    }
    if (nativeIndexOf && array.indexOf === nativeIndexOf) return array.indexOf(item, isSorted);
    for (; i < l; i++) if (array[i] === item) return i;
    return -1;
  };

  // Delegates to **ECMAScript 5**'s native `lastIndexOf` if available.
  _.lastIndexOf = function(array, item, from) {
    if (array == null) return -1;
    var hasIndex = from != null;
    if (nativeLastIndexOf && array.lastIndexOf === nativeLastIndexOf) {
      return hasIndex ? array.lastIndexOf(item, from) : array.lastIndexOf(item);
    }
    var i = (hasIndex ? from : array.length);
    while (i--) if (array[i] === item) return i;
    return -1;
  };

  // Generate an integer Array containing an arithmetic progression. A port of
  // the native Python `range()` function. See
  // [the Python documentation](http://docs.python.org/library/functions.html#range).
  _.range = function(start, stop, step) {
    if (arguments.length <= 1) {
      stop = start || 0;
      start = 0;
    }
    step = arguments[2] || 1;

    var len = Math.max(Math.ceil((stop - start) / step), 0);
    var idx = 0;
    var range = new Array(len);

    while(idx < len) {
      range[idx++] = start;
      start += step;
    }

    return range;
  };

  // Function (ahem) Functions
  // ------------------

  // Reusable constructor function for prototype setting.
  var ctor = function(){};

  // Create a function bound to a given object (assigning `this`, and arguments,
  // optionally). Delegates to **ECMAScript 5**'s native `Function.bind` if
  // available.
  _.bind = function(func, context) {
    var args, bound;
    if (nativeBind && func.bind === nativeBind) return nativeBind.apply(func, slice.call(arguments, 1));
    if (!_.isFunction(func)) throw new TypeError;
    args = slice.call(arguments, 2);
    return bound = function() {
      if (!(this instanceof bound)) return func.apply(context, args.concat(slice.call(arguments)));
      ctor.prototype = func.prototype;
      var self = new ctor;
      ctor.prototype = null;
      var result = func.apply(self, args.concat(slice.call(arguments)));
      if (Object(result) === result) return result;
      return self;
    };
  };

  // Partially apply a function by creating a version that has had some of its
  // arguments pre-filled, without changing its dynamic `this` context.
  _.partial = function(func) {
    var args = slice.call(arguments, 1);
    return function() {
      return func.apply(this, args.concat(slice.call(arguments)));
    };
  };

  // Bind all of an object's methods to that object. Useful for ensuring that
  // all callbacks defined on an object belong to it.
  _.bindAll = function(obj) {
    var funcs = slice.call(arguments, 1);
    if (funcs.length === 0) throw new Error("bindAll must be passed function names");
    each(funcs, function(f) { obj[f] = _.bind(obj[f], obj); });
    return obj;
  };

  // Memoize an expensive function by storing its results.
  _.memoize = function(func, hasher) {
    var memo = {};
    hasher || (hasher = _.identity);
    return function() {
      var key = hasher.apply(this, arguments);
      return _.has(memo, key) ? memo[key] : (memo[key] = func.apply(this, arguments));
    };
  };

  // Delays a function for the given number of milliseconds, and then calls
  // it with the arguments supplied.
  _.delay = function(func, wait) {
    var args = slice.call(arguments, 2);
    return setTimeout(function(){ return func.apply(null, args); }, wait);
  };

  // Defers a function, scheduling it to run after the current call stack has
  // cleared.
  _.defer = function(func) {
    return _.delay.apply(_, [func, 1].concat(slice.call(arguments, 1)));
  };

  // Returns a function, that, when invoked, will only be triggered at most once
  // during a given window of time. Normally, the throttled function will run
  // as much as it can, without ever going more than once per `wait` duration;
  // but if you'd like to disable the execution on the leading edge, pass
  // `{leading: false}`. To disable execution on the trailing edge, ditto.
  _.throttle = function(func, wait, options) {
    var context, args, result;
    var timeout = null;
    var previous = 0;
    options || (options = {});
    var later = function() {
      previous = options.leading === false ? 0 : new Date;
      timeout = null;
      result = func.apply(context, args);
    };
    return function() {
      var now = new Date;
      if (!previous && options.leading === false) previous = now;
      var remaining = wait - (now - previous);
      context = this;
      args = arguments;
      if (remaining <= 0) {
        clearTimeout(timeout);
        timeout = null;
        previous = now;
        result = func.apply(context, args);
      } else if (!timeout && options.trailing !== false) {
        timeout = setTimeout(later, remaining);
      }
      return result;
    };
  };

  // Returns a function, that, as long as it continues to be invoked, will not
  // be triggered. The function will be called after it stops being called for
  // N milliseconds. If `immediate` is passed, trigger the function on the
  // leading edge, instead of the trailing.
  _.debounce = function(func, wait, immediate) {
    var result;
    var timeout = null;
    return function() {
      var context = this, args = arguments;
      var later = function() {
        timeout = null;
        if (!immediate) result = func.apply(context, args);
      };
      var callNow = immediate && !timeout;
      clearTimeout(timeout);
      timeout = setTimeout(later, wait);
      if (callNow) result = func.apply(context, args);
      return result;
    };
  };

  // Returns a function that will be executed at most one time, no matter how
  // often you call it. Useful for lazy initialization.
  _.once = function(func) {
    var ran = false, memo;
    return function() {
      if (ran) return memo;
      ran = true;
      memo = func.apply(this, arguments);
      func = null;
      return memo;
    };
  };

  // Returns the first function passed as an argument to the second,
  // allowing you to adjust arguments, run code before and after, and
  // conditionally execute the original function.
  _.wrap = function(func, wrapper) {
    return function() {
      var args = [func];
      push.apply(args, arguments);
      return wrapper.apply(this, args);
    };
  };

  // Returns a function that is the composition of a list of functions, each
  // consuming the return value of the function that follows.
  _.compose = function() {
    var funcs = arguments;
    return function() {
      var args = arguments;
      for (var i = funcs.length - 1; i >= 0; i--) {
        args = [funcs[i].apply(this, args)];
      }
      return args[0];
    };
  };

  // Returns a function that will only be executed after being called N times.
  _.after = function(times, func) {
    return function() {
      if (--times < 1) {
        return func.apply(this, arguments);
      }
    };
  };

  // Object Functions
  // ----------------

  // Retrieve the names of an object's properties.
  // Delegates to **ECMAScript 5**'s native `Object.keys`
  _.keys = nativeKeys || function(obj) {
    if (obj !== Object(obj)) throw new TypeError('Invalid object');
    var keys = [];
    for (var key in obj) if (_.has(obj, key)) keys.push(key);
    return keys;
  };

  // Retrieve the values of an object's properties.
  _.values = function(obj) {
    var values = [];
    for (var key in obj) if (_.has(obj, key)) values.push(obj[key]);
    return values;
  };

  // Convert an object into a list of `[key, value]` pairs.
  _.pairs = function(obj) {
    var pairs = [];
    for (var key in obj) if (_.has(obj, key)) pairs.push([key, obj[key]]);
    return pairs;
  };

  // Invert the keys and values of an object. The values must be serializable.
  _.invert = function(obj) {
    var result = {};
    for (var key in obj) if (_.has(obj, key)) result[obj[key]] = key;
    return result;
  };

  // Return a sorted list of the function names available on the object.
  // Aliased as `methods`
  _.functions = _.methods = function(obj) {
    var names = [];
    for (var key in obj) {
      if (_.isFunction(obj[key])) names.push(key);
    }
    return names.sort();
  };

  // Extend a given object with all the properties in passed-in object(s).
  _.extend = function(obj) {
    each(slice.call(arguments, 1), function(source) {
      if (source) {
        for (var prop in source) {
          obj[prop] = source[prop];
        }
      }
    });
    return obj;
  };

  // Return a copy of the object only containing the whitelisted properties.
  _.pick = function(obj) {
    var copy = {};
    var keys = concat.apply(ArrayProto, slice.call(arguments, 1));
    each(keys, function(key) {
      if (key in obj) copy[key] = obj[key];
    });
    return copy;
  };

   // Return a copy of the object without the blacklisted properties.
  _.omit = function(obj) {
    var copy = {};
    var keys = concat.apply(ArrayProto, slice.call(arguments, 1));
    for (var key in obj) {
      if (!_.contains(keys, key)) copy[key] = obj[key];
    }
    return copy;
  };

  // Fill in a given object with default properties.
  _.defaults = function(obj) {
    each(slice.call(arguments, 1), function(source) {
      if (source) {
        for (var prop in source) {
          if (obj[prop] === void 0) obj[prop] = source[prop];
        }
      }
    });
    return obj;
  };

  // Create a (shallow-cloned) duplicate of an object.
  _.clone = function(obj) {
    if (!_.isObject(obj)) return obj;
    return _.isArray(obj) ? obj.slice() : _.extend({}, obj);
  };

  // Invokes interceptor with the obj, and then returns obj.
  // The primary purpose of this method is to "tap into" a method chain, in
  // order to perform operations on intermediate results within the chain.
  _.tap = function(obj, interceptor) {
    interceptor(obj);
    return obj;
  };

  // Internal recursive comparison function for `isEqual`.
  var eq = function(a, b, aStack, bStack) {
    // Identical objects are equal. `0 === -0`, but they aren't identical.
    // See the [Harmony `egal` proposal](http://wiki.ecmascript.org/doku.php?id=harmony:egal).
    if (a === b) return a !== 0 || 1 / a == 1 / b;
    // A strict comparison is necessary because `null == undefined`.
    if (a == null || b == null) return a === b;
    // Unwrap any wrapped objects.
    if (a instanceof _) a = a._wrapped;
    if (b instanceof _) b = b._wrapped;
    // Compare `[[Class]]` names.
    var className = toString.call(a);
    if (className != toString.call(b)) return false;
    switch (className) {
      // Strings, numbers, dates, and booleans are compared by value.
      case '[object String]':
        // Primitives and their corresponding object wrappers are equivalent; thus, `"5"` is
        // equivalent to `new String("5")`.
        return a == String(b);
      case '[object Number]':
        // `NaN`s are equivalent, but non-reflexive. An `egal` comparison is performed for
        // other numeric values.
        return a != +a ? b != +b : (a == 0 ? 1 / a == 1 / b : a == +b);
      case '[object Date]':
      case '[object Boolean]':
        // Coerce dates and booleans to numeric primitive values. Dates are compared by their
        // millisecond representations. Note that invalid dates with millisecond representations
        // of `NaN` are not equivalent.
        return +a == +b;
      // RegExps are compared by their source patterns and flags.
      case '[object RegExp]':
        return a.source == b.source &&
               a.global == b.global &&
               a.multiline == b.multiline &&
               a.ignoreCase == b.ignoreCase;
    }
    if (typeof a != 'object' || typeof b != 'object') return false;
    // Assume equality for cyclic structures. The algorithm for detecting cyclic
    // structures is adapted from ES 5.1 section 15.12.3, abstract operation `JO`.
    var length = aStack.length;
    while (length--) {
      // Linear search. Performance is inversely proportional to the number of
      // unique nested structures.
      if (aStack[length] == a) return bStack[length] == b;
    }
    // Objects with different constructors are not equivalent, but `Object`s
    // from different frames are.
    var aCtor = a.constructor, bCtor = b.constructor;
    if (aCtor !== bCtor && !(_.isFunction(aCtor) && (aCtor instanceof aCtor) &&
                             _.isFunction(bCtor) && (bCtor instanceof bCtor))) {
      return false;
    }
    // Add the first object to the stack of traversed objects.
    aStack.push(a);
    bStack.push(b);
    var size = 0, result = true;
    // Recursively compare objects and arrays.
    if (className == '[object Array]') {
      // Compare array lengths to determine if a deep comparison is necessary.
      size = a.length;
      result = size == b.length;
      if (result) {
        // Deep compare the contents, ignoring non-numeric properties.
        while (size--) {
          if (!(result = eq(a[size], b[size], aStack, bStack))) break;
        }
      }
    } else {
      // Deep compare objects.
      for (var key in a) {
        if (_.has(a, key)) {
          // Count the expected number of properties.
          size++;
          // Deep compare each member.
          if (!(result = _.has(b, key) && eq(a[key], b[key], aStack, bStack))) break;
        }
      }
      // Ensure that both objects contain the same number of properties.
      if (result) {
        for (key in b) {
          if (_.has(b, key) && !(size--)) break;
        }
        result = !size;
      }
    }
    // Remove the first object from the stack of traversed objects.
    aStack.pop();
    bStack.pop();
    return result;
  };

  // Perform a deep comparison to check if two objects are equal.
  _.isEqual = function(a, b) {
    return eq(a, b, [], []);
  };

  // Is a given array, string, or object empty?
  // An "empty" object has no enumerable own-properties.
  _.isEmpty = function(obj) {
    if (obj == null) return true;
    if (_.isArray(obj) || _.isString(obj)) return obj.length === 0;
    for (var key in obj) if (_.has(obj, key)) return false;
    return true;
  };

  // Is a given value a DOM element?
  _.isElement = function(obj) {
    return !!(obj && obj.nodeType === 1);
  };

  // Is a given value an array?
  // Delegates to ECMA5's native Array.isArray
  _.isArray = nativeIsArray || function(obj) {
    return toString.call(obj) == '[object Array]';
  };

  // Is a given variable an object?
  _.isObject = function(obj) {
    return obj === Object(obj);
  };

  // Add some isType methods: isArguments, isFunction, isString, isNumber, isDate, isRegExp.
  each(['Arguments', 'Function', 'String', 'Number', 'Date', 'RegExp'], function(name) {
    _['is' + name] = function(obj) {
      return toString.call(obj) == '[object ' + name + ']';
    };
  });

  // Define a fallback version of the method in browsers (ahem, IE), where
  // there isn't any inspectable "Arguments" type.
  if (!_.isArguments(arguments)) {
    _.isArguments = function(obj) {
      return !!(obj && _.has(obj, 'callee'));
    };
  }

  // Optimize `isFunction` if appropriate.
  if (typeof (/./) !== 'function') {
    _.isFunction = function(obj) {
      return typeof obj === 'function';
    };
  }

  // Is a given object a finite number?
  _.isFinite = function(obj) {
    return isFinite(obj) && !isNaN(parseFloat(obj));
  };

  // Is the given value `NaN`? (NaN is the only number which does not equal itself).
  _.isNaN = function(obj) {
    return _.isNumber(obj) && obj != +obj;
  };

  // Is a given value a boolean?
  _.isBoolean = function(obj) {
    return obj === true || obj === false || toString.call(obj) == '[object Boolean]';
  };

  // Is a given value equal to null?
  _.isNull = function(obj) {
    return obj === null;
  };

  // Is a given variable undefined?
  _.isUndefined = function(obj) {
    return obj === void 0;
  };

  // Shortcut function for checking if an object has a given property directly
  // on itself (in other words, not on a prototype).
  _.has = function(obj, key) {
    return hasOwnProperty.call(obj, key);
  };

  // Utility Functions
  // -----------------

  // Run Underscore.js in *noConflict* mode, returning the `_` variable to its
  // previous owner. Returns a reference to the Underscore object.
  _.noConflict = function() {
    root._ = previousUnderscore;
    return this;
  };

  // Keep the identity function around for default iterators.
  _.identity = function(value) {
    return value;
  };

  // Run a function **n** times.
  _.times = function(n, iterator, context) {
    var accum = Array(Math.max(0, n));
    for (var i = 0; i < n; i++) accum[i] = iterator.call(context, i);
    return accum;
  };

  // Return a random integer between min and max (inclusive).
  _.random = function(min, max) {
    if (max == null) {
      max = min;
      min = 0;
    }
    return min + Math.floor(Math.random() * (max - min + 1));
  };

  // List of HTML entities for escaping.
  var entityMap = {
    escape: {
      '&': '&amp;',
      '<': '&lt;',
      '>': '&gt;',
      '"': '&quot;',
      "'": '&#x27;',
      '/': '&#x2F;'
    }
  };
  entityMap.unescape = _.invert(entityMap.escape);

  // Regexes containing the keys and values listed immediately above.
  var entityRegexes = {
    escape:   new RegExp('[' + _.keys(entityMap.escape).join('') + ']', 'g'),
    unescape: new RegExp('(' + _.keys(entityMap.unescape).join('|') + ')', 'g')
  };

  // Functions for escaping and unescaping strings to/from HTML interpolation.
  _.each(['escape', 'unescape'], function(method) {
    _[method] = function(string) {
      if (string == null) return '';
      return ('' + string).replace(entityRegexes[method], function(match) {
        return entityMap[method][match];
      });
    };
  });

  // If the value of the named `property` is a function then invoke it with the
  // `object` as context; otherwise, return it.
  _.result = function(object, property) {
    if (object == null) return void 0;
    var value = object[property];
    return _.isFunction(value) ? value.call(object) : value;
  };

  // Add your own custom functions to the Underscore object.
  _.mixin = function(obj) {
    each(_.functions(obj), function(name){
      var func = _[name] = obj[name];
      _.prototype[name] = function() {
        var args = [this._wrapped];
        push.apply(args, arguments);
        return result.call(this, func.apply(_, args));
      };
    });
  };

  // Generate a unique integer id (unique within the entire client session).
  // Useful for temporary DOM ids.
  var idCounter = 0;
  _.uniqueId = function(prefix) {
    var id = ++idCounter + '';
    return prefix ? prefix + id : id;
  };

  // By default, Underscore uses ERB-style template delimiters, change the
  // following template settings to use alternative delimiters.
  _.templateSettings = {
    evaluate    : /<%([\s\S]+?)%>/g,
    interpolate : /<%=([\s\S]+?)%>/g,
    escape      : /<%-([\s\S]+?)%>/g
  };

  // When customizing `templateSettings`, if you don't want to define an
  // interpolation, evaluation or escaping regex, we need one that is
  // guaranteed not to match.
  var noMatch = /(.)^/;

  // Certain characters need to be escaped so that they can be put into a
  // string literal.
  var escapes = {
    "'":      "'",
    '\\':     '\\',
    '\r':     'r',
    '\n':     'n',
    '\t':     't',
    '\u2028': 'u2028',
    '\u2029': 'u2029'
  };

  var escaper = /\\|'|\r|\n|\t|\u2028|\u2029/g;

  // JavaScript micro-templating, similar to John Resig's implementation.
  // Underscore templating handles arbitrary delimiters, preserves whitespace,
  // and correctly escapes quotes within interpolated code.
  _.template = function(text, data, settings) {
    var render;
    settings = _.defaults({}, settings, _.templateSettings);

    // Combine delimiters into one regular expression via alternation.
    var matcher = new RegExp([
      (settings.escape || noMatch).source,
      (settings.interpolate || noMatch).source,
      (settings.evaluate || noMatch).source
    ].join('|') + '|$', 'g');

    // Compile the template source, escaping string literals appropriately.
    var index = 0;
    var source = "__p+='";
    text.replace(matcher, function(match, escape, interpolate, evaluate, offset) {
      source += text.slice(index, offset)
        .replace(escaper, function(match) { return '\\' + escapes[match]; });

      if (escape) {
        source += "'+\n((__t=(" + escape + "))==null?'':_.escape(__t))+\n'";
      }
      if (interpolate) {
        source += "'+\n((__t=(" + interpolate + "))==null?'':__t)+\n'";
      }
      if (evaluate) {
        source += "';\n" + evaluate + "\n__p+='";
      }
      index = offset + match.length;
      return match;
    });
    source += "';\n";

    // If a variable is not specified, place data values in local scope.
    if (!settings.variable) source = 'with(obj||{}){\n' + source + '}\n';

    source = "var __t,__p='',__j=Array.prototype.join," +
      "print=function(){__p+=__j.call(arguments,'');};\n" +
      source + "return __p;\n";

    try {
      render = new Function(settings.variable || 'obj', '_', source);
    } catch (e) {
      e.source = source;
      throw e;
    }

    if (data) return render(data, _);
    var template = function(data) {
      return render.call(this, data, _);
    };

    // Provide the compiled function source as a convenience for precompilation.
    template.source = 'function(' + (settings.variable || 'obj') + '){\n' + source + '}';

    return template;
  };

  // Add a "chain" function, which will delegate to the wrapper.
  _.chain = function(obj) {
    return _(obj).chain();
  };

  // OOP
  // ---------------
  // If Underscore is called as a function, it returns a wrapped object that
  // can be used OO-style. This wrapper holds altered versions of all the
  // underscore functions. Wrapped objects may be chained.

  // Helper function to continue chaining intermediate results.
  var result = function(obj) {
    return this._chain ? _(obj).chain() : obj;
  };

  // Add all of the Underscore functions to the wrapper object.
  _.mixin(_);

  // Add all mutator Array functions to the wrapper.
  each(['pop', 'push', 'reverse', 'shift', 'sort', 'splice', 'unshift'], function(name) {
    var method = ArrayProto[name];
    _.prototype[name] = function() {
      var obj = this._wrapped;
      method.apply(obj, arguments);
      if ((name == 'shift' || name == 'splice') && obj.length === 0) delete obj[0];
      return result.call(this, obj);
    };
  });

  // Add all accessor Array functions to the wrapper.
  each(['concat', 'join', 'slice'], function(name) {
    var method = ArrayProto[name];
    _.prototype[name] = function() {
      return result.call(this, method.apply(this._wrapped, arguments));
    };
  });

  _.extend(_.prototype, {

    // Start chaining a wrapped Underscore object.
    chain: function() {
      this._chain = true;
      return this;
    },

    // Extracts the result from a wrapped and chained object.
    value: function() {
      return this._wrapped;
    }

  });

}).call(this);/* end   <bundled/underscore/underscore.js> */
/* start <util/JOBAD.util.js> */
/*
	JOBAD utility functions
	
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

/* various utility functions */
JOBAD.util = {};

/*
	Binds every function within an object recursively. 
	@param obj Object to bind. 
	@param thisObj 'this' inside functions. 
*/
JOBAD.util.bindEverything = function(obj, thisObj){
	if(JOBAD.util.isObject(obj) && typeof obj != 'function' ){
		var ret = {};
		for(var key in obj){
			ret[key] = JOBAD.util.bindEverything(obj[key], thisObj);
		}
		return ret;
	} else if(typeof obj == 'function'){
		return JOBAD.util.bind(obj, thisObj);
	} else {
		return JOBAD.util.clone(obj);
	}
};

/*
	Creates a unique ID
	@param	prefix	Optional. A prefix to use for the UID. 
*/
JOBAD.util.UID = function(prefix){
	var prefix = (typeof prefix == "string")?prefix+"_":"";
	var time = (new Date()).getTime();
	var id1 = Math.floor(Math.random()*1000);
	var id2 = Math.floor(Math.random()*1000);
	return ""+prefix+time+"_"+id1+"_"+id2;
};

/*
	Creates a dropdown (<select>) control. 
	@param values	Values to use. 
	@param texts	Texts to use. 
	@param start	Initially selected id. 
*/
JOBAD.util.createDropDown = function(values, texts, start){
	var select = JOBAD.refs.$("<select>"); 

	for(var i=0;i<texts.length;i++){
		select.append(
			JOBAD.refs.$("<option>")
			.attr("value", values[i])
			.text(texts[i])
		);
	}

	select.find("option").eq((typeof start == "number")?start:0).prop('selected', true); 

	return select; 
}

/*
	Creates a radio button for use with Bootsrap
	@param texts	Texts to use. 
	@param start	Initial selection
*/
JOBAD.util.createRadio = function(texts, start){
	var id = JOBAD.util.UID(); //Id for the radio buttons
	var selfChange = false; 

	if(typeof start !== 'number'){
		start = 0;
	}

	var Div = JOBAD.refs.$('<div>').addClass("btn-group");
	var Div2 = JOBAD.refs.$('<div>').hide(); 
					
	for(var i=0;i<texts.length;i++){
		Div.append(
			JOBAD.refs.$("<button>").addClass("btn").text(texts[i])
		);
		Div2.append(
			JOBAD.refs.$("<input type='radio' name='"+id+"' value='"+JOBAD.util.UID()+"'>")
		);
	}


	var Buttons = Div.find("button"); 
	var Inputs = Div2.find("input"); 

	Buttons.on("click", function(){
		var radio = Inputs.eq(Buttons.index(this)); 
		radio[0].checked = true; 
		Inputs.change(); 
	})

	Inputs
	.change(function(e){
		Buttons.removeClass("active"); 

		Inputs.each(function(i){
			var me = JOBAD.refs.$(this); 
			if(me.is(":checked")){
				Buttons.eq(i).addClass("active"); 
			}
		})
		e.stopPropagation(); 
	});
	
	Inputs.eq(start)[0].checked = true;
	Inputs.change(); 

	
	return JOBAD.refs.$("<div>").append(Div, Div2); 
};

/*
	Creates tab data compatible with Bootstrap. 
	@param names	Texts to use. 
	@param divs	Divs to use as content. 
	@param config Configuration. Optional. 
		@param config.tabParams	Params for Tab creation. 
		@param config.type Type of tabs to use. CSS class. 
		@param config.select Select Hook. function(tabName, tabDiv) To be called on selection of a div. 
		@param config.unselect Deselect Hook. function(tabName, tabDiv) Top be called on the deselection of a div. 
*/
JOBAD.util.createTabs = function(names, divs, config){
	var config = JOBAD.util.defined(config); 

	var options = JOBAD.util.defined(config.tabParams); 
	var tabtype = (typeof config.type == "string")?config.type:"";
	var enableHook = (typeof config.select == "function")?config.select:function(){}; 
	var disableHook = (typeof config.unselect == "function")?config.unselect:function(){}; 

	var ids = []; 

	var div = JOBAD.refs.$("<div>").addClass("tabbable "+tabtype);
	var ul = JOBAD.refs.$("<ul>").appendTo(div).addClass("nav nav-tabs");
	var cdiv = JOBAD.refs.$("<div>").addClass("tab-content").appendTo(div);
	for(var i=0;i<names.length;i++){
		var id = JOBAD.util.UID();
		ids.push("#"+id); 
		ul.append(
			JOBAD.refs.$("<li>").append(JOBAD.refs.$("<a>").attr("data-toggle", "tab").attr("href", "#"+id).text(names[i]))
		);
		
		JOBAD.refs.$("<div>").append(divs[i]).attr("id", id).addClass("tab-pane").appendTo(cdiv);
	}
	cdiv.children().eq(0).addClass("active"); 

	JOBAD.refs.$('a[data-toggle="tab"]', ul).on("shown", function(e){
		if(typeof e.relatedTarget != "undefined"){
			var relatedTarget = JOBAD.refs.$(e.relatedTarget); 
			var tabId = ids.indexOf(relatedTarget.attr("href")); 

			disableHook(relatedTarget.text(), JOBAD.refs.$(divs[tabId])); 
		}

		var Target = JOBAD.refs.$(e.target); 
		var tabId = ids.indexOf(Target.attr("href")); 
		enableHook(Target.text(), JOBAD.refs.$(divs[tabId]));
	}); 

	JOBAD.refs.$('a[data-toggle="tab"]', ul).eq(0).tab("show"); 

	return div; 
};

/*
	Applies a function to the arguments of a function every time it is called. 
	@param func Function to wrap
	@param wrap Wrapper function. 
*/

JOBAD.util.argWrap = function(func, wrapper){
	return function(){
		var new_args = [];
		for(var i=0;i<arguments.length;i++){
			new_args.push(arguments[i]);
		}
		
		return func.apply(this, wrapper(new_args));
	};
};


/*
	Applies Array.slice to the arguments of a function every time it is called. 
	@param func Function to wrap
	@param to First parameter for args
	@param from Second Parameter for slice
*/

JOBAD.util.argSlice = function(func, from, to){
	return JOBAD.util.argWrap(func, function(args){
		return args.slice(from, to);
	});
};

/*
	Checks if 2 objects are equal. Does not accept functions. 
	@param a Object A
	@param b Object B
	@returns boolean
*/
JOBAD.util.objectEquals = function(a, b){
	try{
		return JSON.stringify(a) == JSON.stringify(b);
	} catch(e){
		return a==b;
	}
};

/*
	Similary to jQuery's .closest() but also accepts functions. 
*/
JOBAD.util.closest = function(element, selector){
	var element = JOBAD.refs.$(element);
	if(typeof selector == "function"){
		while(element.length > 0){
			if(selector.call(element[0], element)){
				break; //we are matching
			}
			element = element.parent(); //go up
		}
		return element;
	} else {
		return element.closest(selector);
	}
};

/* Element marking */
/*
	Marks an element as hidden. 
	@param	element	Element to mark as hidden. 
*/
JOBAD.util.markHidden = function(element){
	return JOBAD.util.markDefault(element).addClass("JOBAD_Ignore");
};

/*
	Marks an element as visible.
	@param	element	Element to mark as visible. 
*/
JOBAD.util.markVisible = function(element){
	return JOBAD.util.markDefault(element).addClass("JOBAD_Notice");
};

/*
	Removes a marking from an element. Everything is treated as normal. 
	@param	element	Element to remove Marking from. 
*/
JOBAD.util.markDefault = function(element){
	return JOBAD.refs.$(element).removeClass("JOBAD_Ignore").removeClass("JOBAD_Notice");
};

/*
	Checks if an element is marked as hidden. 
	@param	element	Element to check. 
*/
JOBAD.util.isMarkedHidden = function(element){
	return (JOBAD.util.closest(element, function(e){
		//find the closest hidden one. 
		return e.hasClass("JOBAD_Ignore");
	}).length > 0);
};

/*
	Checks if an element is marked as visible. 
	@param	element	Element to check. 
*/
JOBAD.util.isMarkedVisible = function(element){
	return JOBAD.refs.$(element).hasClass("JOBAD_Notice");;
};

/*
	Checks if an element is hidden (either in reality or marked) . 
	@param	element	Element to check. 
*/
JOBAD.util.isHidden = function(element){
	var element = JOBAD.refs.$(element);
	if(JOBAD.util.isMarkedVisible(element)){
		return false;
	} else if(JOBAD.util.isMarkedHidden(element)){
		return true;
	} else if(element.is("map") || element.is("area")){//in response to issue #14 
		return false; 
	} else {
		return element.is(":hidden");
	}
};

/* Other utility functions */

/*
	Checks if object is defined and return obj, otherwise an empty Object. 
	@param	obj	Object to check. 
*/
JOBAD.util.defined = function(obj){
	return (typeof obj == "undefined")?{}:obj;
};

/*
	Forces obj to be a boolean. 
	@param obj	Object to check. 
	@param def	Optional. Default to use instead. 
*/
JOBAD.util.forceBool = function(obj, def){
	if(typeof def == "undefined"){
		def = obj; 
	}
	return (typeof obj == "boolean"?obj:(def?true:false));
};

/*
	Forces an object to be an array. 
*/
JOBAD.util.forceArray = function(obj, def){
	var def = def; 
	if(typeof def == "undefined"){
		if(typeof obj == "undefined"){
			def =  []; 
		} else {
			def = [obj];
		}
	}
	if(!JOBAD.util.isArray(def)){
		def = [def]; 
	}
	return JOBAD.util.isArray(obj)?obj:def; 
}

/*
	Forces obj to be a function. 
	@param func	Function to check. 
	@param def	Optional. Default to use instead. 
*/
JOBAD.util.forceFunction = function(func, def){
	//local References
	var def = def;
	var func = func;
	if(typeof func == "function"){
		return func;
	} else if(typeof def == "undefined"){
		return function(){return func; }
	} else if(typeof def == "function"){
		return def;
	} else {
		return function(){return def; }
	}
}

/*
	If obj is of type type, return obj else def. 
*/
JOBAD.util.ifType = function(obj, type, def){
	return (obj instanceof type)?obj:def;
}

/*
	Checks if two strings are equal, ignoring upper and lower case. 
*/
JOBAD.util.equalsIgnoreCase = function(a, b){
	var a = String(a);
	var b = String(b);

	return (a.toLowerCase() == b.toLowerCase())
};

/*
	Orders a jQuery collection by their tree depth. 
	@param element Collection to sort. 
*/
JOBAD.util.orderTree = function(element){
	var element = JOBAD.refs.$(element);
	return JOBAD.refs.$(element.get().sort(function(a, b){
		var a = JOBAD.refs.$(a).parents().filter(element).length;
		var b = JOBAD.refs.$(b).parents().filter(element).length;

		if(a<b){
			return -1;
		} else if(a > b){
			return 1;
		} else {
			return 0;
		}
	}));
};

/*
	Checks if a string is a URL. 
	@param str	String to check. 
	@returns boolean. 
*/
JOBAD.util.isUrl = function(str){
	var str = String(str);
	var expression = /[-a-zA-Z0-9@:%_\+.~#?&//=]{2,256}\.[a-z]{2,4}\b(\/[-a-zA-Z0-9@:%_\+.~#?&//=]*)?/gi;
	var matches = str.match(new RegExp(expression));
	if(JOBAD.util.isArray(matches)){
		return matches.length > 0;
	} else {
		return JOBAD.util.startsWith(str, "data:"); 
	}
};

/*
	Checks if the string str starts with the string start. 
*/
JOBAD.util.startsWith = function(str, start){
	var str = String(str);
	var start = String(start);
	return (str.substring(0, start.length) == start); 
}

/*
	logical or
*/
JOBAD.util.lOr = function(){
	var args = [];
	for(var i=0;i<arguments.length;i++){
		args.push(arguments[i]);
	}
	args = JOBAD.util.map(JOBAD.util.flatten(args), JOBAD.util.forceBool);
	return (JOBAD.util.indexOf(args, true)!= -1);

};

/*
	logical and
*/
JOBAD.util.lAnd = function(){
	var args = [];
	for(var i=0;i<arguments.length;i++){
		args.push(arguments[i]);
	}
	args = JOBAD.util.map(JOBAD.util.flatten(args), JOBAD.util.forceBool);
	return (JOBAD.util.indexOf(args, false)== -1);
};

/*
	Checks if a jQuery element container contains all of contained. 
	Similar to jQuery.contains
	@param	container	Container element. 
	@param	contained	Contained elements. 
	@param	includeSelf	Should container itself be included in the search
*/
JOBAD.util.containsAll = function(container, contained, includeSelf){
	var container = JOBAD.refs.$(container); 
	var includeSelf = JOBAD.util.forceBool(includeSelf, false); 
	return JOBAD.util.lAnd(
		JOBAD.refs.$(contained).map(function(){
			return container.is(contained) || (includeSelf && container.find(contained).length > 0); 
		}).get()
	);
};

/*
	Loads an external javascript file. 
	@param url	Url(s) of script(s) to load. 
	@param	callback	Callback of script to load. 
	@param	scope	Scope of callback. 
	@param preLoadHack. Function to call before laoding a specific file. 
*/
JOBAD.util.loadExternalJS = function(url, callback, scope, preLoadHack){
	var TIMEOUT_CONST = 15000; //timeout for bad links
	var has_called = false; 
	var preLoadHack = JOBAD.util.forceFunction(preLoadHack, function(){}); 

	var do_call = function(suc){
		if(has_called){
			return;
		}
		has_called = true;

		var func = JOBAD.util.forceFunction(callback, function(){});
		var scope = (typeof scope == "undefined")?window:scope;

		func.call(scope, url, suc);
		
	}

	
	if(JOBAD.util.isArray(url)){
		var i=0;
		var next = function(urls, suc){
			if(i>=url.length || !suc){
				window.setTimeout(function(){
					do_call(suc);
				}, 0);
			} else {
				JOBAD.util.loadExternalJS(url[i], function(urls, suc){
					i++;
					next(urls, suc);
				}, scope, preLoadHack);
			}
		}

		window.setTimeout(function(){
			next("", true);
		}, 0);

		return url.length;
	} else {
		//adapted from: http://www.nczonline.net/blog/2009/07/28/the-best-way-to-load-external-javascript/
		var script = document.createElement("script")
	    script.type = "text/javascript";

	    if (script.readyState){  //IE
	        script.onreadystatechange = function(){
	            if (script.readyState == "loaded" ||
	                    script.readyState == "complete"){
	                script.onreadystatechange = null;
	                window.setTimeout(function(){
						do_call(true);
					}, 0);
	            }
	        };
	    } else {  //Others
	        script.onload = function(){
	            window.setTimeout(function(){
					do_call(true);
				}, 0);
	        };
	    }

	    script.src = JOBAD.util.resolve(url);
	    preLoadHack(url); 
	    document.getElementsByTagName("head")[0].appendChild(script);

	    window.setTimeout(function(){
	    	do_call(false);
	    }, TIMEOUT_CONST);
	    return 1;
	}
    
}

/*
	Loads an external css file. 
	@param url	Url(s) of css to load. 
	@param	callback	Callback of css to load. 
	@param	scope	Scope of callback. 
	@param preLoadHack. Function to call before laoding a specific file. 
*/
JOBAD.util.loadExternalCSS = function(url, callback, scope, preLoadHack){
	var TIMEOUT_CONST = 15000; //timeout for bad links
	var has_called = false; 
	var interval_id, timeout_id; 
	var preLoadHack = JOBAD.util.forceFunction(preLoadHack, function(){}); 

	var do_call = function(suc){
		if(has_called){
			return;
		}
		has_called = true;
		try{

		} catch(e){
			clearInterval(interval_id); 
			clearTimeout(timeout_id);
		}

		var func = JOBAD.util.forceFunction(callback, function(){});
		var scope = (typeof scope == "undefined")?window:scope;

		func.call(scope, url, suc);
		
	}

	
	if(JOBAD.util.isArray(url)){
		var i=0;
		var next = function(urls, suc){
			if(i>=url.length || !suc){
				window.setTimeout(function(){
					do_call(suc);
				}, 0);
			} else {
				JOBAD.util.loadExternalCSS(url[i], function(urls, suc){
					i++;
					next(urls, suc);
				}, scope, preLoadHack);
			}
		}

		window.setTimeout(function(){
			next("", true);
		}, 0);

		return url.length;
	} else {
		//adapted from: http://stackoverflow.com/questions/5537622/dynamically-loading-css-file-using-javascript-with-callback-without-jquery
		var head = document.getElementsByTagName('head')[0]; 
		var link = document.createElement('link');
		link.setAttribute( 'href', url );
		link.setAttribute( 'rel', 'stylesheet' );
		link.setAttribute( 'type', 'text/css' ); 
		var sheet, cssRules;

		interval_id = setInterval(function(){
			try{
				if("sheet" in link){
					if(link.sheet && link.sheet.cssRules.length){
						clearInterval(interval_id); 
						clearTimeout(timeout_id); 
						do_call(true); 
					}
				} else {
					if(link.styleSheet && link.styleSheet.rules.length > 0){
						clearInterval(interval_id); 
						clearTimeout(timeout_id); 
						do_call(true); 
					}
				}

				if(link[sheet] && link[sheet][cssRules].length > 0){
					clearInterval(interval_id); 
					clearTimeout(timeout_id); 

					do_call(true); 
				}
			}catch(e){}
		}, 1000);

		timeout_id = setTimeout(function(){
			clearInterval(interval_id); 
			do_call(false);
		}, TIMEOUT_CONST);


		link.onload = function () {
			do_call(true); 
		}
		if (link.addEventListener) {
			link.addEventListener('load', function() {
			do_call(true); 
			}, false);
		}

	  link.onreadystatechange = function() {
	    var state = link.readyState;
	    if (state === 'loaded' || state === 'complete') {
	      link.onreadystatechange = null;
	      do_call(true); 
	    }
	  };

	  	preLoadHack(url);
		head.appendChild(link); 
		return 1;
	}
    
}

/*
	escapes a string for HTML
	@param	str	String to escape
*/
JOBAD.util.escapeHTML = function(s){
	return s.split('&').join('&amp;').split('<').join('&lt;').split('"').join('&quot;');
}

/*
	Resolves a relative url
	@param url	Url to resolve
	@param base	Optional. Base url to use. 
	@param isDir	Optional. If set to true, will return a directory name ending with a slash
*/
JOBAD.util.resolve = function(url, base, isDir){

	var resolveWithBase = false; 
	var baseUrl, oldBase, newBase; 

	if(typeof base == "string"){
		resolveWithBase = true; 
		baseUrl = JOBAD.util.resolve(base, true); 
		oldBase = JOBAD.refs.$("base").detach(); 
		newBase = JOBAD.refs.$("<base>").attr("href", baseUrl).appendTo("head"); 
	}
	
    var el= document.createElement('div');
    el.innerHTML= '<a href="'+JOBAD.util.escapeHTML(url)+'">x</a>';
    var url = el.firstChild.href;
   
    if(resolveWithBase){
    	newBase.remove(); 
    	oldBase.appendTo("head"); 
	}

	if( (base === true || isDir === true ) && url[url.length - 1] != "/"){url = url + "/"; }
    return url; 
}


/*
	Adds an event listener to a query. 
	@param	query A jQuery element to use as as query. 
	@param	event Event to register trigger for. 
	@param	handler	Handler to add
	@returns an id for the added handler. 
*/
JOBAD.util.on = function(query, event, handler){
	var query = JOBAD.refs.$(query);
	var id = JOBAD.util.UID(); 
	var handler = JOBAD.util.forceFunction(handler, function(){});
	handler = JOBAD.util.argSlice(handler, 1); 

	query.on(event+".core."+id, function(ev){
		var result = JOBAD.util.forceArray(ev.result);

		result.push(handler.apply(this, arguments));

		return result; 
	});
	return event+".core."+id;
}

/*
	Adds a one-time event listener to a query. 
	@param	query A jQuery element to use as as query. 
	@param	event Event to register trigger for. 
	@param	handler	Handler to add
	@returns an id for the added handler. 
*/
JOBAD.util.once = function(query, event, handler){
	var id;

	id = JOBAD.util.on(query, event, function(){
		var result = handler.apply(this, arguments); 
		JOBAD.util.off(query, id); 
	});
}

/*
	Removes an event handler from a query. 
	@param	query A jQuery element to use as as query. 
	@param	id	Id of handler to remove. 
*/
JOBAD.util.off = function(query, id){
	var query = JOBAD.refs.$(query);
	query.off(id); 
}

/*
	Turns a keyup event into a string. 
*/
JOBAD.util.toKeyString = function(e){
	var res = ((e.ctrlKey || e.keyCode == 17 )? 'ctrl+' : '') +
        ((e.altKey || e.keyCode == 18 ) ? 'alt+' : '') +
        ((e.shiftKey || e.keyCode == 16 ) ? 'shift+' : ''); 

      var specialKeys = {
			8: "backspace", 9: "tab", 10: "return", 13: "return", 19: "pause",
			20: "capslock", 27: "esc", 32: "space", 33: "pageup", 34: "pagedown", 35: "end", 36: "home",
			37: "left", 38: "up", 39: "right", 40: "down", 45: "insert", 46: "del", 
			96: "0", 97: "1", 98: "2", 99: "3", 100: "4", 101: "5", 102: "6", 103: "7",
			104: "8", 105: "9", 106: "*", 107: "+", 109: "-", 110: ".", 111 : "/", 
			112: "f1", 113: "f2", 114: "f3", 115: "f4", 116: "f5", 117: "f6", 118: "f7", 119: "f8", 
			120: "f9", 121: "f10", 122: "f11", 123: "f12", 144: "numlock", 145: "scroll", 186: ";", 191: "/",
			220: "\\", 222: "'", 224: "meta"
		}

    if(!e.charCode && specialKeys[e.keyCode]){
    	res += specialKeys[e.keyCode]; 
    } else {
    	if(res == "" && event.type == "keypress"){
    		return false; 
    	} else {
    		res +=String.fromCharCode(e.charCode || e.keyCode).toLowerCase();
    	}
    }

    if(res[res.length-1] == "+"){
    	return res.substring(0, res.length - 1); 
    } else {
    	return res; 
    }
};

JOBAD.util.onKey = function(cb){
	var uuid = JOBAD.util.UID(); 
	JOBAD.refs.$(document).on("keydown."+uuid+" keypress."+uuid, function(evt){
		var key = JOBAD.util.toKeyString(evt); 
		if(!key){
			return; 
		}
		var res = cb.call(undefined, key, evt);

		if(res === false){
			//stop propagnation etc
			evt.preventDefault();
			evt.stopPropagation(); 
			return false; 
		}
	}); 

	return "keydown."+uuid+" keypress."+uuid; 
}

/*
	Triggers an event on a query. 
	@param	query A jQuery element to use as as query. 
	@param	event Event to trigger. 
	@param	params	Parameters to give to the event. 
*/
JOBAD.util.trigger = function(query, event, params){

	var query = JOBAD.refs.$(query);

	var result; 

	var params = JOBAD.util.forceArray(params).slice(0);
	params.unshift(event); 

	var id = JOBAD.util.UID(); 

	query.on(event+"."+id, function(ev){
		result = ev.result; 
	})

	query.trigger.apply(query, params);

	query.off(event+"."+id);

	return result; 

}

/*
	Creates a new Event Handler
*/
JOBAD.util.EventHandler = function(me){
	var handler = {}; 
	var EventHandler = JOBAD.refs.$("<div>"); 

	handler.on = function(event, handler){
		return JOBAD.util.on(EventHandler, event, handler.bind(me));
	};

	handler.once = function(event, handler){
		return JOBAD.util.once(EventHandler, event, handler.bind(me));
	};

	handler.off = function(handler){
		return JOBAD.util.off(EventHandler, handler);
	};

	handler.trigger = function(event, params){
		return JOBAD.util.trigger(EventHandler, event, params);
	}

	return handler;
}

/*
	Gets the current script origin. 
*/
JOBAD.util.getCurrentOrigin = function(){

	var scripts = JOBAD.refs.$('script'); 
	var src = scripts[scripts.length-1].src;

	//if we have an empty src or jQuery is ready, return the location.href
	return (src == "" || jQuery.isReady || !src)?location.href:src; 
}

/*
	Permute to members of an array. 
	@param arr	Array to permute. 
	@param a	Index of first element. 
	@param b	Index of second element. 
*/
JOBAD.util.permuteArray = function(arr, a, b){

	var arr = JOBAD.refs.$.makeArray(arr); 
	
	if(!JOBAD.util.isArray(arr)){
		return arr; 
	}

	var a = JOBAD.util.limit(a, 0, arr.length); 
	var b = JOBAD.util.limit(b, 0, arr.length); 

	var arr = arr.slice(0); 

	var old = arr[a];
	arr[a] = arr[b]; 
	arr[b] = old; 

	return arr; 
}

/*
	Limit the number x to be between a and b. 

*/
JOBAD.util.limit = function(x, a, b){
	if(a >= b){
		return (x<b)?b:((x>a)?a:x); 
	} else {
		// b > a
		return JOBAD.util.limit(x, b, a); 
	}
}



//Merge underscore and JOBAD.util namespace
_.mixin(JOBAD.util);
JOBAD.util = _.noConflict(); //destroy the original underscore instance. /* end   <util/JOBAD.util.js> */
/* start <JOBAD.resources.js> */
/*
	JOBAD String Resoucres
	JOBAD.resources.js
	
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

//Contains license texts etc. 
JOBAD.resources = {};

/*
	Gets an icon resource. 
	@param	icon	Icon to get. 
	@param	mappers	Optional. Map of icon => real_icon mappings. 
*/
JOBAD.resources.getIconResource = function(icon, mappers){
	var mappers = JOBAD.util.defined(mappers);
	if(mappers.hasOwnProperty(icon)){
		icon = mappers[icon];
	}
	if(JOBAD.resources.icon.hasOwnProperty(icon)){
		return JOBAD.resources.icon[icon];
	} else if(JOBAD.util.isUrl(icon)){
		return icon;
	} else {
		JOBAD.console.warn("Can't find icon resource: '"+icon+"' does not match any resource name. ")
		return JOBAD.resources.icon["error"];
	}
}

/*
	Gets a text resource. 
	@param	text	Text Resource to get. 
*/
JOBAD.resources.getTextResource = function(text){
	if(JOBAD.resources.text.hasOwnProperty(text)){
		return JOBAD.resources.text[text];
	} else {
		JOBAD.console.warn("Can't find text resource: '"+text+"' does not match any resource name. ")
		return "Missing resource: '"+text+"'";
	}
}

/*
	Provide text or icon resources. 
	@param	type	Type of resource to provide. 
	@param	name	Name of Resource to provide or a resource map.  
	@param	value	Resource to provide. 
*/
JOBAD.resources.provide = function(type, name, value){
	if(type != "text" && type != "icon"){
		JOABD.console.warn("First paramater JOBAD.resources.provide must be either 'text' or 'icon'. ");
		return;
	}

	var data = {};

	if(JOBAD.util.isObject(name)){
		data = name;
	} else {
		data[name] = value;
	}

	JOBAD.resources[type] = JOBAD.util.extend(JOBAD.util.defined(JOBAD.resources[type]), data);
}

JOBAD.resources.available = function(type, name){
	if(type != "text" && type != "icon"){
		JOABD.console.warn("First paramater JOBAD.resources.available must be either 'text' or 'icon'. ");
		return;
	}
	return JOBAD.util.defined(JOBAD.resources[type]).hasOwnProperty(name);
}

/* Standard Resources */

// Text Resources
JOBAD.resources.provide("text", {
	"gpl_v3_text": "                    GNU GENERAL PUBLIC LICENSE\n                       Version 3, 29 June 2007\n\n Copyright (C) 2007 Free Software Foundation, Inc. \x3Chttp:\x2F\x2Ffsf.org\x2F\x3E\n Everyone is permitted to copy and distribute verbatim copies\n of this license document, but changing it is not allowed.\n\n                            Preamble\n\n  The GNU General Public License is a free, copyleft license for\nsoftware and other kinds of works.\n\n  The licenses for most software and other practical works are designed\nto take away your freedom to share and change the works.  By contrast,\nthe GNU General Public License is intended to guarantee your freedom to\nshare and change all versions of a program--to make sure it remains free\nsoftware for all its users.  We, the Free Software Foundation, use the\nGNU General Public License for most of our software; it applies also to\nany other work released this way by its authors.  You can apply it to\nyour programs, too.\n\n  When we speak of free software, we are referring to freedom, not\nprice.  Our General Public Licenses are designed to make sure that you\nhave the freedom to distribute copies of free software (and charge for\nthem if you wish), that you receive source code or can get it if you\nwant it, that you can change the software or use pieces of it in new\nfree programs, and that you know you can do these things.\n\n  To protect your rights, we need to prevent others from denying you\nthese rights or asking you to surrender the rights.  Therefore, you have\ncertain responsibilities if you distribute copies of the software, or if\nyou modify it: responsibilities to respect the freedom of others.\n\n  For example, if you distribute copies of such a program, whether\ngratis or for a fee, you must pass on to the recipients the same\nfreedoms that you received.  You must make sure that they, too, receive\nor can get the source code.  And you must show them these terms so they\nknow their rights.\n\n  Developers that use the GNU GPL protect your rights with two steps:\n(1) assert copyright on the software, and (2) offer you this License\ngiving you legal permission to copy, distribute and\x2For modify it.\n\n  For the developers\' and authors\' protection, the GPL clearly explains\nthat there is no warranty for this free software.  For both users\' and\nauthors\' sake, the GPL requires that modified versions be marked as\nchanged, so that their problems will not be attributed erroneously to\nauthors of previous versions.\n\n  Some devices are designed to deny users access to install or run\nmodified versions of the software inside them, although the manufacturer\ncan do so.  This is fundamentally incompatible with the aim of\nprotecting users\' freedom to change the software.  The systematic\npattern of such abuse occurs in the area of products for individuals to\nuse, which is precisely where it is most unacceptable.  Therefore, we\nhave designed this version of the GPL to prohibit the practice for those\nproducts.  If such problems arise substantially in other domains, we\nstand ready to extend this provision to those domains in future versions\nof the GPL, as needed to protect the freedom of users.\n\n  Finally, every program is threatened constantly by software patents.\nStates should not allow patents to restrict development and use of\nsoftware on general-purpose computers, but in those that do, we wish to\navoid the special danger that patents applied to a free program could\nmake it effectively proprietary.  To prevent this, the GPL assures that\npatents cannot be used to render the program non-free.\n\n  The precise terms and conditions for copying, distribution and\nmodification follow.\n\n                       TERMS AND CONDITIONS\n\n  0. Definitions.\n\n  \"This License\" refers to version 3 of the GNU General Public License.\n\n  \"Copyright\" also means copyright-like laws that apply to other kinds of\nworks, such as semiconductor masks.\n\n  \"The Program\" refers to any copyrightable work licensed under this\nLicense.  Each licensee is addressed as \"you\".  \"Licensees\" and\n\"recipients\" may be individuals or organizations.\n\n  To \"modify\" a work means to copy from or adapt all or part of the work\nin a fashion requiring copyright permission, other than the making of an\nexact copy.  The resulting work is called a \"modified version\" of the\nearlier work or a work \"based on\" the earlier work.\n\n  A \"covered work\" means either the unmodified Program or a work based\non the Program.\n\n  To \"propagate\" a work means to do anything with it that, without\npermission, would make you directly or secondarily liable for\ninfringement under applicable copyright law, except executing it on a\ncomputer or modifying a private copy.  Propagation includes copying,\ndistribution (with or without modification), making available to the\npublic, and in some countries other activities as well.\n\n  To \"convey\" a work means any kind of propagation that enables other\nparties to make or receive copies.  Mere interaction with a user through\na computer network, with no transfer of a copy, is not conveying.\n\n  An interactive user interface displays \"Appropriate Legal Notices\"\nto the extent that it includes a convenient and prominently visible\nfeature that (1) displays an appropriate copyright notice, and (2)\ntells the user that there is no warranty for the work (except to the\nextent that warranties are provided), that licensees may convey the\nwork under this License, and how to view a copy of this License.  If\nthe interface presents a list of user commands or options, such as a\nmenu, a prominent item in the list meets this criterion.\n\n  1. Source Code.\n\n  The \"source code\" for a work means the preferred form of the work\nfor making modifications to it.  \"Object code\" means any non-source\nform of a work.\n\n  A \"Standard Interface\" means an interface that either is an official\nstandard defined by a recognized standards body, or, in the case of\ninterfaces specified for a particular programming language, one that\nis widely used among developers working in that language.\n\n  The \"System Libraries\" of an executable work include anything, other\nthan the work as a whole, that (a) is included in the normal form of\npackaging a Major Component, but which is not part of that Major\nComponent, and (b) serves only to enable use of the work with that\nMajor Component, or to implement a Standard Interface for which an\nimplementation is available to the public in source code form.  A\n\"Major Component\", in this context, means a major essential component\n(kernel, window system, and so on) of the specific operating system\n(if any) on which the executable work runs, or a compiler used to\nproduce the work, or an object code interpreter used to run it.\n\n  The \"Corresponding Source\" for a work in object code form means all\nthe source code needed to generate, install, and (for an executable\nwork) run the object code and to modify the work, including scripts to\ncontrol those activities.  However, it does not include the work\'s\nSystem Libraries, or general-purpose tools or generally available free\nprograms which are used unmodified in performing those activities but\nwhich are not part of the work.  For example, Corresponding Source\nincludes interface definition files associated with source files for\nthe work, and the source code for shared libraries and dynamically\nlinked subprograms that the work is specifically designed to require,\nsuch as by intimate data communication or control flow between those\nsubprograms and other parts of the work.\n\n  The Corresponding Source need not include anything that users\ncan regenerate automatically from other parts of the Corresponding\nSource.\n\n  The Corresponding Source for a work in source code form is that\nsame work.\n\n  2. Basic Permissions.\n\n  All rights granted under this License are granted for the term of\ncopyright on the Program, and are irrevocable provided the stated\nconditions are met.  This License explicitly affirms your unlimited\npermission to run the unmodified Program.  The output from running a\ncovered work is covered by this License only if the output, given its\ncontent, constitutes a covered work.  This License acknowledges your\nrights of fair use or other equivalent, as provided by copyright law.\n\n  You may make, run and propagate covered works that you do not\nconvey, without conditions so long as your license otherwise remains\nin force.  You may convey covered works to others for the sole purpose\nof having them make modifications exclusively for you, or provide you\nwith facilities for running those works, provided that you comply with\nthe terms of this License in conveying all material for which you do\nnot control copyright.  Those thus making or running the covered works\nfor you must do so exclusively on your behalf, under your direction\nand control, on terms that prohibit them from making any copies of\nyour copyrighted material outside their relationship with you.\n\n  Conveying under any other circumstances is permitted solely under\nthe conditions stated below.  Sublicensing is not allowed; section 10\nmakes it unnecessary.\n\n  3. Protecting Users\' Legal Rights From Anti-Circumvention Law.\n\n  No covered work shall be deemed part of an effective technological\nmeasure under any applicable law fulfilling obligations under article\n11 of the WIPO copyright treaty adopted on 20 December 1996, or\nsimilar laws prohibiting or restricting circumvention of such\nmeasures.\n\n  When you convey a covered work, you waive any legal power to forbid\ncircumvention of technological measures to the extent such circumvention\nis effected by exercising rights under this License with respect to\nthe covered work, and you disclaim any intention to limit operation or\nmodification of the work as a means of enforcing, against the work\'s\nusers, your or third parties\' legal rights to forbid circumvention of\ntechnological measures.\n\n  4. Conveying Verbatim Copies.\n\n  You may convey verbatim copies of the Program\'s source code as you\nreceive it, in any medium, provided that you conspicuously and\nappropriately publish on each copy an appropriate copyright notice;\nkeep intact all notices stating that this License and any\nnon-permissive terms added in accord with section 7 apply to the code;\nkeep intact all notices of the absence of any warranty; and give all\nrecipients a copy of this License along with the Program.\n\n  You may charge any price or no price for each copy that you convey,\nand you may offer support or warranty protection for a fee.\n\n  5. Conveying Modified Source Versions.\n\n  You may convey a work based on the Program, or the modifications to\nproduce it from the Program, in the form of source code under the\nterms of section 4, provided that you also meet all of these conditions:\n\n    a) The work must carry prominent notices stating that you modified\n    it, and giving a relevant date.\n\n    b) The work must carry prominent notices stating that it is\n    released under this License and any conditions added under section\n    7.  This requirement modifies the requirement in section 4 to\n    \"keep intact all notices\".\n\n    c) You must license the entire work, as a whole, under this\n    License to anyone who comes into possession of a copy.  This\n    License will therefore apply, along with any applicable section 7\n    additional terms, to the whole of the work, and all its parts,\n    regardless of how they are packaged.  This License gives no\n    permission to license the work in any other way, but it does not\n    invalidate such permission if you have separately received it.\n\n    d) If the work has interactive user interfaces, each must display\n    Appropriate Legal Notices; however, if the Program has interactive\n    interfaces that do not display Appropriate Legal Notices, your\n    work need not make them do so.\n\n  A compilation of a covered work with other separate and independent\nworks, which are not by their nature extensions of the covered work,\nand which are not combined with it such as to form a larger program,\nin or on a volume of a storage or distribution medium, is called an\n\"aggregate\" if the compilation and its resulting copyright are not\nused to limit the access or legal rights of the compilation\'s users\nbeyond what the individual works permit.  Inclusion of a covered work\nin an aggregate does not cause this License to apply to the other\nparts of the aggregate.\n\n  6. Conveying Non-Source Forms.\n\n  You may convey a covered work in object code form under the terms\nof sections 4 and 5, provided that you also convey the\nmachine-readable Corresponding Source under the terms of this License,\nin one of these ways:\n\n    a) Convey the object code in, or embodied in, a physical product\n    (including a physical distribution medium), accompanied by the\n    Corresponding Source fixed on a durable physical medium\n    customarily used for software interchange.\n\n    b) Convey the object code in, or embodied in, a physical product\n    (including a physical distribution medium), accompanied by a\n    written offer, valid for at least three years and valid for as\n    long as you offer spare parts or customer support for that product\n    model, to give anyone who possesses the object code either (1) a\n    copy of the Corresponding Source for all the software in the\n    product that is covered by this License, on a durable physical\n    medium customarily used for software interchange, for a price no\n    more than your reasonable cost of physically performing this\n    conveying of source, or (2) access to copy the\n    Corresponding Source from a network server at no charge.\n\n    c) Convey individual copies of the object code with a copy of the\n    written offer to provide the Corresponding Source.  This\n    alternative is allowed only occasionally and noncommercially, and\n    only if you received the object code with such an offer, in accord\n    with subsection 6b.\n\n    d) Convey the object code by offering access from a designated\n    place (gratis or for a charge), and offer equivalent access to the\n    Corresponding Source in the same way through the same place at no\n    further charge.  You need not require recipients to copy the\n    Corresponding Source along with the object code.  If the place to\n    copy the object code is a network server, the Corresponding Source\n    may be on a different server (operated by you or a third party)\n    that supports equivalent copying facilities, provided you maintain\n    clear directions next to the object code saying where to find the\n    Corresponding Source.  Regardless of what server hosts the\n    Corresponding Source, you remain obligated to ensure that it is\n    available for as long as needed to satisfy these requirements.\n\n    e) Convey the object code using peer-to-peer transmission, provided\n    you inform other peers where the object code and Corresponding\n    Source of the work are being offered to the general public at no\n    charge under subsection 6d.\n\n  A separable portion of the object code, whose source code is excluded\nfrom the Corresponding Source as a System Library, need not be\nincluded in conveying the object code work.\n\n  A \"User Product\" is either (1) a \"consumer product\", which means any\ntangible personal property which is normally used for personal, family,\nor household purposes, or (2) anything designed or sold for incorporation\ninto a dwelling.  In determining whether a product is a consumer product,\ndoubtful cases shall be resolved in favor of coverage.  For a particular\nproduct received by a particular user, \"normally used\" refers to a\ntypical or common use of that class of product, regardless of the status\nof the particular user or of the way in which the particular user\nactually uses, or expects or is expected to use, the product.  A product\nis a consumer product regardless of whether the product has substantial\ncommercial, industrial or non-consumer uses, unless such uses represent\nthe only significant mode of use of the product.\n\n  \"Installation Information\" for a User Product means any methods,\nprocedures, authorization keys, or other information required to install\nand execute modified versions of a covered work in that User Product from\na modified version of its Corresponding Source.  The information must\nsuffice to ensure that the continued functioning of the modified object\ncode is in no case prevented or interfered with solely because\nmodification has been made.\n\n  If you convey an object code work under this section in, or with, or\nspecifically for use in, a User Product, and the conveying occurs as\npart of a transaction in which the right of possession and use of the\nUser Product is transferred to the recipient in perpetuity or for a\nfixed term (regardless of how the transaction is characterized), the\nCorresponding Source conveyed under this section must be accompanied\nby the Installation Information.  But this requirement does not apply\nif neither you nor any third party retains the ability to install\nmodified object code on the User Product (for example, the work has\nbeen installed in ROM).\n\n  The requirement to provide Installation Information does not include a\nrequirement to continue to provide support service, warranty, or updates\nfor a work that has been modified or installed by the recipient, or for\nthe User Product in which it has been modified or installed.  Access to a\nnetwork may be denied when the modification itself materially and\nadversely affects the operation of the network or violates the rules and\nprotocols for communication across the network.\n\n  Corresponding Source conveyed, and Installation Information provided,\nin accord with this section must be in a format that is publicly\ndocumented (and with an implementation available to the public in\nsource code form), and must require no special password or key for\nunpacking, reading or copying.\n\n  7. Additional Terms.\n\n  \"Additional permissions\" are terms that supplement the terms of this\nLicense by making exceptions from one or more of its conditions.\nAdditional permissions that are applicable to the entire Program shall\nbe treated as though they were included in this License, to the extent\nthat they are valid under applicable law.  If additional permissions\napply only to part of the Program, that part may be used separately\nunder those permissions, but the entire Program remains governed by\nthis License without regard to the additional permissions.\n\n  When you convey a copy of a covered work, you may at your option\nremove any additional permissions from that copy, or from any part of\nit.  (Additional permissions may be written to require their own\nremoval in certain cases when you modify the work.)  You may place\nadditional permissions on material, added by you to a covered work,\nfor which you have or can give appropriate copyright permission.\n\n  Notwithstanding any other provision of this License, for material you\nadd to a covered work, you may (if authorized by the copyright holders of\nthat material) supplement the terms of this License with terms:\n\n    a) Disclaiming warranty or limiting liability differently from the\n    terms of sections 15 and 16 of this License; or\n\n    b) Requiring preservation of specified reasonable legal notices or\n    author attributions in that material or in the Appropriate Legal\n    Notices displayed by works containing it; or\n\n    c) Prohibiting misrepresentation of the origin of that material, or\n    requiring that modified versions of such material be marked in\n    reasonable ways as different from the original version; or\n\n    d) Limiting the use for publicity purposes of names of licensors or\n    authors of the material; or\n\n    e) Declining to grant rights under trademark law for use of some\n    trade names, trademarks, or service marks; or\n\n    f) Requiring indemnification of licensors and authors of that\n    material by anyone who conveys the material (or modified versions of\n    it) with contractual assumptions of liability to the recipient, for\n    any liability that these contractual assumptions directly impose on\n    those licensors and authors.\n\n  All other non-permissive additional terms are considered \"further\nrestrictions\" within the meaning of section 10.  If the Program as you\nreceived it, or any part of it, contains a notice stating that it is\ngoverned by this License along with a term that is a further\nrestriction, you may remove that term.  If a license document contains\na further restriction but permits relicensing or conveying under this\nLicense, you may add to a covered work material governed by the terms\nof that license document, provided that the further restriction does\nnot survive such relicensing or conveying.\n\n  If you add terms to a covered work in accord with this section, you\nmust place, in the relevant source files, a statement of the\nadditional terms that apply to those files, or a notice indicating\nwhere to find the applicable terms.\n\n  Additional terms, permissive or non-permissive, may be stated in the\nform of a separately written license, or stated as exceptions;\nthe above requirements apply either way.\n\n  8. Termination.\n\n  You may not propagate or modify a covered work except as expressly\nprovided under this License.  Any attempt otherwise to propagate or\nmodify it is void, and will automatically terminate your rights under\nthis License (including any patent licenses granted under the third\nparagraph of section 11).\n\n  However, if you cease all violation of this License, then your\nlicense from a particular copyright holder is reinstated (a)\nprovisionally, unless and until the copyright holder explicitly and\nfinally terminates your license, and (b) permanently, if the copyright\nholder fails to notify you of the violation by some reasonable means\nprior to 60 days after the cessation.\n\n  Moreover, your license from a particular copyright holder is\nreinstated permanently if the copyright holder notifies you of the\nviolation by some reasonable means, this is the first time you have\nreceived notice of violation of this License (for any work) from that\ncopyright holder, and you cure the violation prior to 30 days after\nyour receipt of the notice.\n\n  Termination of your rights under this section does not terminate the\nlicenses of parties who have received copies or rights from you under\nthis License.  If your rights have been terminated and not permanently\nreinstated, you do not qualify to receive new licenses for the same\nmaterial under section 10.\n\n  9. Acceptance Not Required for Having Copies.\n\n  You are not required to accept this License in order to receive or\nrun a copy of the Program.  Ancillary propagation of a covered work\noccurring solely as a consequence of using peer-to-peer transmission\nto receive a copy likewise does not require acceptance.  However,\nnothing other than this License grants you permission to propagate or\nmodify any covered work.  These actions infringe copyright if you do\nnot accept this License.  Therefore, by modifying or propagating a\ncovered work, you indicate your acceptance of this License to do so.\n\n  10. Automatic Licensing of Downstream Recipients.\n\n  Each time you convey a covered work, the recipient automatically\nreceives a license from the original licensors, to run, modify and\npropagate that work, subject to this License.  You are not responsible\nfor enforcing compliance by third parties with this License.\n\n  An \"entity transaction\" is a transaction transferring control of an\norganization, or substantially all assets of one, or subdividing an\norganization, or merging organizations.  If propagation of a covered\nwork results from an entity transaction, each party to that\ntransaction who receives a copy of the work also receives whatever\nlicenses to the work the party\'s predecessor in interest had or could\ngive under the previous paragraph, plus a right to possession of the\nCorresponding Source of the work from the predecessor in interest, if\nthe predecessor has it or can get it with reasonable efforts.\n\n  You may not impose any further restrictions on the exercise of the\nrights granted or affirmed under this License.  For example, you may\nnot impose a license fee, royalty, or other charge for exercise of\nrights granted under this License, and you may not initiate litigation\n(including a cross-claim or counterclaim in a lawsuit) alleging that\nany patent claim is infringed by making, using, selling, offering for\nsale, or importing the Program or any portion of it.\n\n  11. Patents.\n\n  A \"contributor\" is a copyright holder who authorizes use under this\nLicense of the Program or a work on which the Program is based.  The\nwork thus licensed is called the contributor\'s \"contributor version\".\n\n  A contributor\'s \"essential patent claims\" are all patent claims\nowned or controlled by the contributor, whether already acquired or\nhereafter acquired, that would be infringed by some manner, permitted\nby this License, of making, using, or selling its contributor version,\nbut do not include claims that would be infringed only as a\nconsequence of further modification of the contributor version.  For\npurposes of this definition, \"control\" includes the right to grant\npatent sublicenses in a manner consistent with the requirements of\nthis License.\n\n  Each contributor grants you a non-exclusive, worldwide, royalty-free\npatent license under the contributor\'s essential patent claims, to\nmake, use, sell, offer for sale, import and otherwise run, modify and\npropagate the contents of its contributor version.\n\n  In the following three paragraphs, a \"patent license\" is any express\nagreement or commitment, however denominated, not to enforce a patent\n(such as an express permission to practice a patent or covenant not to\nsue for patent infringement).  To \"grant\" such a patent license to a\nparty means to make such an agreement or commitment not to enforce a\npatent against the party.\n\n  If you convey a covered work, knowingly relying on a patent license,\nand the Corresponding Source of the work is not available for anyone\nto copy, free of charge and under the terms of this License, through a\npublicly available network server or other readily accessible means,\nthen you must either (1) cause the Corresponding Source to be so\navailable, or (2) arrange to deprive yourself of the benefit of the\npatent license for this particular work, or (3) arrange, in a manner\nconsistent with the requirements of this License, to extend the patent\nlicense to downstream recipients.  \"Knowingly relying\" means you have\nactual knowledge that, but for the patent license, your conveying the\ncovered work in a country, or your recipient\'s use of the covered work\nin a country, would infringe one or more identifiable patents in that\ncountry that you have reason to believe are valid.\n\n  If, pursuant to or in connection with a single transaction or\narrangement, you convey, or propagate by procuring conveyance of, a\ncovered work, and grant a patent license to some of the parties\nreceiving the covered work authorizing them to use, propagate, modify\nor convey a specific copy of the covered work, then the patent license\nyou grant is automatically extended to all recipients of the covered\nwork and works based on it.\n\n  A patent license is \"discriminatory\" if it does not include within\nthe scope of its coverage, prohibits the exercise of, or is\nconditioned on the non-exercise of one or more of the rights that are\nspecifically granted under this License.  You may not convey a covered\nwork if you are a party to an arrangement with a third party that is\nin the business of distributing software, under which you make payment\nto the third party based on the extent of your activity of conveying\nthe work, and under which the third party grants, to any of the\nparties who would receive the covered work from you, a discriminatory\npatent license (a) in connection with copies of the covered work\nconveyed by you (or copies made from those copies), or (b) primarily\nfor and in connection with specific products or compilations that\ncontain the covered work, unless you entered into that arrangement,\nor that patent license was granted, prior to 28 March 2007.\n\n  Nothing in this License shall be construed as excluding or limiting\nany implied license or other defenses to infringement that may\notherwise be available to you under applicable patent law.\n\n  12. No Surrender of Others\' Freedom.\n\n  If conditions are imposed on you (whether by court order, agreement or\notherwise) that contradict the conditions of this License, they do not\nexcuse you from the conditions of this License.  If you cannot convey a\ncovered work so as to satisfy simultaneously your obligations under this\nLicense and any other pertinent obligations, then as a consequence you may\nnot convey it at all.  For example, if you agree to terms that obligate you\nto collect a royalty for further conveying from those to whom you convey\nthe Program, the only way you could satisfy both those terms and this\nLicense would be to refrain entirely from conveying the Program.\n\n  13. Use with the GNU Affero General Public License.\n\n  Notwithstanding any other provision of this License, you have\npermission to link or combine any covered work with a work licensed\nunder version 3 of the GNU Affero General Public License into a single\ncombined work, and to convey the resulting work.  The terms of this\nLicense will continue to apply to the part which is the covered work,\nbut the special requirements of the GNU Affero General Public License,\nsection 13, concerning interaction through a network will apply to the\ncombination as such.\n\n  14. Revised Versions of this License.\n\n  The Free Software Foundation may publish revised and\x2For new versions of\nthe GNU General Public License from time to time.  Such new versions will\nbe similar in spirit to the present version, but may differ in detail to\naddress new problems or concerns.\n\n  Each version is given a distinguishing version number.  If the\nProgram specifies that a certain numbered version of the GNU General\nPublic License \"or any later version\" applies to it, you have the\noption of following the terms and conditions either of that numbered\nversion or of any later version published by the Free Software\nFoundation.  If the Program does not specify a version number of the\nGNU General Public License, you may choose any version ever published\nby the Free Software Foundation.\n\n  If the Program specifies that a proxy can decide which future\nversions of the GNU General Public License can be used, that proxy\'s\npublic statement of acceptance of a version permanently authorizes you\nto choose that version for the Program.\n\n  Later license versions may give you additional or different\npermissions.  However, no additional obligations are imposed on any\nauthor or copyright holder as a result of your choosing to follow a\nlater version.\n\n  15. Disclaimer of Warranty.\n\n  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY\nAPPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT\nHOLDERS AND\x2FOR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY\nOF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,\nTHE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR\nPURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM\nIS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF\nALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\n  16. Limitation of Liability.\n\n  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING\nWILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND\x2FOR CONVEYS\nTHE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY\nGENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE\nUSE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF\nDATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD\nPARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),\nEVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF\nSUCH DAMAGES.\n\n  17. Interpretation of Sections 15 and 16.\n\n  If the disclaimer of warranty and limitation of liability provided\nabove cannot be given local legal effect according to their terms,\nreviewing courts shall apply local law that most closely approximates\nan absolute waiver of all civil liability in connection with the\nProgram, unless a warranty or assumption of liability accompanies a\ncopy of the Program in return for a fee.\n\n                     END OF TERMS AND CONDITIONS\n\n            How to Apply These Terms to Your New Programs\n\n  If you develop a new program, and you want it to be of the greatest\npossible use to the public, the best way to achieve this is to make it\nfree software which everyone can redistribute and change under these terms.\n\n  To do so, attach the following notices to the program.  It is safest\nto attach them to the start of each source file to most effectively\nstate the exclusion of warranty; and each file should have at least\nthe \"copyright\" line and a pointer to where the full notice is found.\n\n    \x3Cone line to give the program\'s name and a brief idea of what it does.\x3E\n    Copyright (C) \x3Cyear\x3E  \x3Cname of author\x3E\n\n    This program is free software: you can redistribute it and\x2For modify\n    it under the terms of the GNU General Public License as published by\n    the Free Software Foundation, either version 3 of the License, or\n    (at your option) any later version.\n\n    This program is distributed in the hope that it will be useful,\n    but WITHOUT ANY WARRANTY; without even the implied warranty of\n    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n    GNU General Public License for more details.\n\n    You should have received a copy of the GNU General Public License\n    along with this program.  If not, see \x3Chttp:\x2F\x2Fwww.gnu.org\x2Flicenses\x2F\x3E.\n\nAlso add information on how to contact you by electronic and paper mail.\n\n  If the program does terminal interaction, make it output a short\nnotice like this when it starts in an interactive mode:\n\n    \x3Cprogram\x3E  Copyright (C) \x3Cyear\x3E  \x3Cname of author\x3E\n    This program comes with ABSOLUTELY NO WARRANTY; for details type `show w\'.\n    This is free software, and you are welcome to redistribute it\n    under certain conditions; type `show c\' for details.\n\nThe hypothetical commands `show w\' and `show c\' should show the appropriate\nparts of the General Public License.  Of course, your program\'s commands\nmight be different; for a GUI interface, you would use an \"about box\".\n\n  You should also get your employer (if you work as a programmer) or school,\nif any, to sign a \"copyright disclaimer\" for the program, if necessary.\nFor more information on this, and how to apply and follow the GNU GPL, see\n\x3Chttp:\x2F\x2Fwww.gnu.org\x2Flicenses\x2F\x3E.\n\n  The GNU General Public License does not permit incorporating your program\ninto proprietary programs.  If your program is a subroutine library, you\nmay consider it more useful to permit linking proprietary applications with\nthe library.  If this is what you want to do, use the GNU Lesser General\nPublic License instead of this License.  But first, please read\n\x3Chttp:\x2F\x2Fwww.gnu.org\x2Fphilosophy\x2Fwhy-not-lgpl.html\x3E.",
	"jquery_license": "Copyright 2013 jQuery Foundation and other contributors\nhttp:\x2F\x2Fjquery.com\x2F\n\nPermission is hereby granted, free of charge, to any person obtaining\na copy of this software and associated documentation files (the\n\"Software\"), to deal in the Software without restriction, including\nwithout limitation the rights to use, copy, modify, merge, publish,\ndistribute, sublicense, and\x2For sell copies of the Software, and to\npermit persons to whom the Software is furnished to do so, subject to\nthe following conditions:\n\nThe above copyright notice and this permission notice shall be\nincluded in all copies or substantial portions of the Software.\n\nTHE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,\nEXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF\nMERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND\nNONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE\nLIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION\nOF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION\nWITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.",
	"underscore_license": "Copyright (c) 2009-2013 Jeremy Ashkenas, DocumentCloud\n\nPermission is hereby granted, free of charge, to any person\nobtaining a copy of this software and associated documentation\nfiles (the \"Software\"), to deal in the Software without\nrestriction, including without limitation the rights to use,\ncopy, modify, merge, publish, distribute, sublicense, and\x2For sell\ncopies of the Software, and to permit persons to whom the\nSoftware is furnished to do so, subject to the following\nconditions:\n\nThe above copyright notice and this permission notice shall be\nincluded in all copies or substantial portions of the Software.\n\nTHE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,\nEXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES\nOF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND\nNONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT\nHOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,\nWHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING\nFROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR\nOTHER DEALINGS IN THE SOFTWARE.",
	"jobad_license": "JOBAD - JavaScript API for OMDoc-based Active Documents\n\nCopyright (C) 2013-17 KWARC Group \x3Ckwarc.info\x3E\n\nJOBAD is free software: you can redistribute it and\x2For modify\nit under the terms of the GNU General Public License as published by\nthe Free Software Foundation, either version 3 of the License, or\n(at your option) any later version.\n\nJOBAD is distributed in the hope that it will be useful,\nbut WITHOUT ANY WARRANTY; without even the implied warranty of\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\nGNU General Public License for more details.\n\nYou should have received a copy of the GNU General Public License\nalong with JOBAD.  If not, see \x3Chttp:\x2F\x2Fwww.gnu.org\x2Flicenses\x2F\x3E.",
	"bootstrap_license": "                                 Apache License\r\n                           Version 2.0, January 2004\r\n                        http:\/\/www.apache.org\/licenses\/\r\n\r\n   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION\r\n\r\n   1. Definitions.\r\n\r\n      \"License\" shall mean the terms and conditions for use, reproduction,\r\n      and distribution as defined by Sections 1 through 9 of this document.\r\n\r\n      \"Licensor\" shall mean the copyright owner or entity authorized by\r\n      the copyright owner that is granting the License.\r\n\r\n      \"Legal Entity\" shall mean the union of the acting entity and all\r\n      other entities that control, are controlled by, or are under common\r\n      control with that entity. For the purposes of this definition,\r\n      \"control\" means (i) the power, direct or indirect, to cause the\r\n      direction or management of such entity, whether by contract or\r\n      otherwise, or (ii) ownership of fifty percent (50%) or more of the\r\n      outstanding shares, or (iii) beneficial ownership of such entity.\r\n\r\n      \"You\" (or \"Your\") shall mean an individual or Legal Entity\r\n      exercising permissions granted by this License.\r\n\r\n      \"Source\" form shall mean the preferred form for making modifications,\r\n      including but not limited to software source code, documentation\r\n      source, and configuration files.\r\n\r\n      \"Object\" form shall mean any form resulting from mechanical\r\n      transformation or translation of a Source form, including but\r\n      not limited to compiled object code, generated documentation,\r\n      and conversions to other media types.\r\n\r\n      \"Work\" shall mean the work of authorship, whether in Source or\r\n      Object form, made available under the License, as indicated by a\r\n      copyright notice that is included in or attached to the work\r\n      (an example is provided in the Appendix below).\r\n\r\n      \"Derivative Works\" shall mean any work, whether in Source or Object\r\n      form, that is based on (or derived from) the Work and for which the\r\n      editorial revisions, annotations, elaborations, or other modifications\r\n      represent, as a whole, an original work of authorship. For the purposes\r\n      of this License, Derivative Works shall not include works that remain\r\n      separable from, or merely link (or bind by name) to the interfaces of,\r\n      the Work and Derivative Works thereof.\r\n\r\n      \"Contribution\" shall mean any work of authorship, including\r\n      the original version of the Work and any modifications or additions\r\n      to that Work or Derivative Works thereof, that is intentionally\r\n      submitted to Licensor for inclusion in the Work by the copyright owner\r\n      or by an individual or Legal Entity authorized to submit on behalf of\r\n      the copyright owner. For the purposes of this definition, \"submitted\"\r\n      means any form of electronic, verbal, or written communication sent\r\n      to the Licensor or its representatives, including but not limited to\r\n      communication on electronic mailing lists, source code control systems,\r\n      and issue tracking systems that are managed by, or on behalf of, the\r\n      Licensor for the purpose of discussing and improving the Work, but\r\n      excluding communication that is conspicuously marked or otherwise\r\n      designated in writing by the copyright owner as \"Not a Contribution.\"\r\n\r\n      \"Contributor\" shall mean Licensor and any individual or Legal Entity\r\n      on behalf of whom a Contribution has been received by Licensor and\r\n      subsequently incorporated within the Work.\r\n\r\n   2. Grant of Copyright License. Subject to the terms and conditions of\r\n      this License, each Contributor hereby grants to You a perpetual,\r\n      worldwide, non-exclusive, no-charge, royalty-free, irrevocable\r\n      copyright license to reproduce, prepare Derivative Works of,\r\n      publicly display, publicly perform, sublicense, and distribute the\r\n      Work and such Derivative Works in Source or Object form.\r\n\r\n   3. Grant of Patent License. Subject to the terms and conditions of\r\n      this License, each Contributor hereby grants to You a perpetual,\r\n      worldwide, non-exclusive, no-charge, royalty-free, irrevocable\r\n      (except as stated in this section) patent license to make, have made,\r\n      use, offer to sell, sell, import, and otherwise transfer the Work,\r\n      where such license applies only to those patent claims licensable\r\n      by such Contributor that are necessarily infringed by their\r\n      Contribution(s) alone or by combination of their Contribution(s)\r\n      with the Work to which such Contribution(s) was submitted. If You\r\n      institute patent litigation against any entity (including a\r\n      cross-claim or counterclaim in a lawsuit) alleging that the Work\r\n      or a Contribution incorporated within the Work constitutes direct\r\n      or contributory patent infringement, then any patent licenses\r\n      granted to You under this License for that Work shall terminate\r\n      as of the date such litigation is filed.\r\n\r\n   4. Redistribution. You may reproduce and distribute copies of the\r\n      Work or Derivative Works thereof in any medium, with or without\r\n      modifications, and in Source or Object form, provided that You\r\n      meet the following conditions:\r\n\r\n      (a) You must give any other recipients of the Work or\r\n          Derivative Works a copy of this License; and\r\n\r\n      (b) You must cause any modified files to carry prominent notices\r\n          stating that You changed the files; and\r\n\r\n      (c) You must retain, in the Source form of any Derivative Works\r\n          that You distribute, all copyright, patent, trademark, and\r\n          attribution notices from the Source form of the Work,\r\n          excluding those notices that do not pertain to any part of\r\n          the Derivative Works; and\r\n\r\n      (d) If the Work includes a \"NOTICE\" text file as part of its\r\n          distribution, then any Derivative Works that You distribute must\r\n          include a readable copy of the attribution notices contained\r\n          within such NOTICE file, excluding those notices that do not\r\n          pertain to any part of the Derivative Works, in at least one\r\n          of the following places: within a NOTICE text file distributed\r\n          as part of the Derivative Works; within the Source form or\r\n          documentation, if provided along with the Derivative Works; or,\r\n          within a display generated by the Derivative Works, if and\r\n          wherever such third-party notices normally appear. The contents\r\n          of the NOTICE file are for informational purposes only and\r\n          do not modify the License. You may add Your own attribution\r\n          notices within Derivative Works that You distribute, alongside\r\n          or as an addendum to the NOTICE text from the Work, provided\r\n          that such additional attribution notices cannot be construed\r\n          as modifying the License.\r\n\r\n      You may add Your own copyright statement to Your modifications and\r\n      may provide additional or different license terms and conditions\r\n      for use, reproduction, or distribution of Your modifications, or\r\n      for any such Derivative Works as a whole, provided Your use,\r\n      reproduction, and distribution of the Work otherwise complies with\r\n      the conditions stated in this License.\r\n\r\n   5. Submission of Contributions. Unless You explicitly state otherwise,\r\n      any Contribution intentionally submitted for inclusion in the Work\r\n      by You to the Licensor shall be under the terms and conditions of\r\n      this License, without any additional terms or conditions.\r\n      Notwithstanding the above, nothing herein shall supersede or modify\r\n      the terms of any separate license agreement you may have executed\r\n      with Licensor regarding such Contributions.\r\n\r\n   6. Trademarks. This License does not grant permission to use the trade\r\n      names, trademarks, service marks, or product names of the Licensor,\r\n      except as required for reasonable and customary use in describing the\r\n      origin of the Work and reproducing the content of the NOTICE file.\r\n\r\n   7. Disclaimer of Warranty. Unless required by applicable law or\r\n      agreed to in writing, Licensor provides the Work (and each\r\n      Contributor provides its Contributions) on an \"AS IS\" BASIS,\r\n      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or\r\n      implied, including, without limitation, any warranties or conditions\r\n      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A\r\n      PARTICULAR PURPOSE. You are solely responsible for determining the\r\n      appropriateness of using or redistributing the Work and assume any\r\n      risks associated with Your exercise of permissions under this License.\r\n\r\n   8. Limitation of Liability. In no event and under no legal theory,\r\n      whether in tort (including negligence), contract, or otherwise,\r\n      unless required by applicable law (such as deliberate and grossly\r\n      negligent acts) or agreed to in writing, shall any Contributor be\r\n      liable to You for damages, including any direct, indirect, special,\r\n      incidental, or consequential damages of any character arising as a\r\n      result of this License or out of the use or inability to use the\r\n      Work (including but not limited to damages for loss of goodwill,\r\n      work stoppage, computer failure or malfunction, or any and all\r\n      other commercial damages or losses), even if such Contributor\r\n      has been advised of the possibility of such damages.\r\n\r\n   9. Accepting Warranty or Additional Liability. While redistributing\r\n      the Work or Derivative Works thereof, You may choose to offer,\r\n      and charge a fee for, acceptance of support, warranty, indemnity,\r\n      or other liability obligations and\/or rights consistent with this\r\n      License. However, in accepting such obligations, You may act only\r\n      on Your own behalf and on Your sole responsibility, not on behalf\r\n      of any other Contributor, and only if You agree to indemnify,\r\n      defend, and hold each Contributor harmless for any liability\r\n      incurred by, or claims asserted against, such Contributor by reason\r\n      of your accepting any such warranty or additional liability.\r\n\r\n   END OF TERMS AND CONDITIONS", 
	"jquery_color_license": "Copyright 2013 jQuery Foundation and other contributors,\nhttp:\x2F\x2Fjquery.com\n\nPermission is hereby granted, free of charge, to any person obtaining\na copy of this software and associated documentation files (the\n\"Software\"), to deal in the Software without restriction, including\nwithout limitation the rights to use, copy, modify, merge, publish,\ndistribute, sublicense, and\x2For sell copies of the Software, and to\npermit persons to whom the Software is furnished to do so, subject to\nthe following conditions:\n\nThe above copyright notice and this permission notice shall be\nincluded in all copies or substantial portions of the Software.\n\nTHE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,\nEXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF\nMERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND\nNONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE\nLIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION\nOF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION\nWITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. "
});

JOBAD.resources.provide("icon", {
	/* All icons are public domain taken from http://openiconlibrary.sourceforge.net/ */
	"warning": "data:image/png;base64,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",
	"info": "data:image/png;base64,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", 
	"error": "data:image/png;base64,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", 
	"open": "data:image/png;base64,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",
	"close": "data:image/png;base64,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"
});
/* end   <JOBAD.resources.js> */
/* start <JOBAD.repo.js> */
/*
	JOBAD.repo.js - Contains the JOBAD repo implementation
	
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

var JOBAD_Repo_Urls = {}; //urls per repo
var JOBAD_Repo_Mods = {}; //primary modules

/*
	Marks the current page as a repository page and generates a repository about page. 
*/
JOBAD.repo = function(){
	JOBAD.repo.buildPage("body", "./");
}

/*
	Builds an information page about a repository. 
	@param	element	Element to generate page info in. 
	@param	repo	Repository to build page about. 
	@param	callback	A Callback; 
	
*/
JOBAD.repo.buildPage = function(element, repo, callback){

	var callback = JOBAD.util.forceFunction(callback, function(){}); 

	var body = JOBAD.refs.$("<div class='JOBAD JOBAD_Repo JOBAD_Repo_Body'>").appendTo(JOBAD.refs.$(element).empty());

	var msgBox = JOBAD.refs.$("<div class='bar'>");

	msgBox.wrap("<div class='progress'>").parent().wrap("<div class='JOBAD JOBAD_Repo JOBAD_Repo_MsgBox'>").parent().appendTo(body); 

	var label = JOBAD.refs.$("<div class='progress-label'>").text("Loading Repository, please wait ...").appendTo(msgBox);
	
	msgBox.css({
		"width": 0
	});

	var baseUrl = JOBAD.util.resolve(repo);
	baseUrl = baseUrl.substring(0, baseUrl.length - 1); // no slash at the end

	JOBAD.repo.init(baseUrl, function(suc, cache){
		msgBox.css({
			"width": "25%",
		}); 

		if(!suc){
			label.text("Repository loading failed: "+cache);
			return; 
		}

		var title = cache.name; 
		var desc = cache.description; 

		body.append(
			JOBAD.refs.$("<h1 class='JOBAD JOBAD_Repo JOBAD_Repo_Title'>").text(title),
			JOBAD.refs.$("<div class='JOBAD JOBAD_Repo JOBAD_Repo_Desc'>").text(desc)
		)


		var table = JOBAD.refs.$("<table class='JOBAD JOBAD_Repo JOBAD_Repo_Table'>").appendTo(body);

		table.append(
			JOBAD.refs.$("<thead>").append(
				JOBAD.refs.$("<tr>")
				.append(
					JOBAD.refs.$("<th>").text("Identifier"),
					JOBAD.refs.$("<th>").text("Name"),
					JOBAD.refs.$("<th>").text("Author"),
					JOBAD.refs.$("<th>").text("Version"),
					JOBAD.refs.$("<th>").text("Homepage"),
					JOBAD.refs.$("<th>").text("Description"),
					JOBAD.refs.$("<th>").text("Module Dependencies"),
					JOBAD.refs.$("<th>").text("External JavaScript Dependencies"),
					JOBAD.refs.$("<th>").text("External CSS Dependencies")
				).children("th").click(function(){
					JOBAD.UI.sortTableBy(this, "rotate", function(i){
						this.parent().find("span").remove(); 
						if(i==1){
							this.append("<span class='JOBAD JOBAD_Repo JOBAD_Sort_Ascend'>");
						} else if(i==2){
							this.append("<span class='JOBAD JOBAD_Repo JOBAD_Sort_Descend'>");
						}
					}); 
					return false; 
				}).end() 
			)
		);

		//Init everything

		label.text("Loading module information ...")

		var modules = JOBAD.util.keys(JOBAD_Repo_Urls[baseUrl]);
		var count = modules.length; 

		var i=0;


		var next = function(){

			msgBox.css({
				"width": ""+((25+75*((i+1)/(modules.length)))*100)+"%"
			});

			var name = modules[i]; 

			if(i >= modules.length){
				label.text("Finished. ");
				msgBox.parent().fadeOut(1000);
				callback(body); 
				return;
			}

			label.text("Loading module information ["+(i+1)+"/"+count+"]: \""+name+"\"");

			JOBAD.repo.loadFrom(baseUrl, modules[i], function(suc){

				if(!suc){
					table.append(
						JOBAD.refs.$("<tr>")
						.append(
							JOBAD.refs.$("<td></td>").text(name),
							"<td colspan='7'>Loading failed: Timeout</td>"
						).css("color", "red")
					);
				} else if(typeof moduleList[name] == "undefined"){
					table.append(
						JOBAD.refs.$("<tr>")
						.append(
							JOBAD.refs.$("<td></td>").text(name),
							"<td colspan='7'>Loading failed: Module specification incorrect. </td>"
						).css("color", "red")
					);
				} else {
					var info = moduleList[name].info;

					var row = JOBAD.refs.$("<tr>").appendTo(table); 
					
					//id
					JOBAD.refs.$("<td></td>").text(info.identifier).appendTo(row);
					
					//name
					JOBAD.refs.$("<td></td>").text(info.title).appendTo(row);
					
					//author
					JOBAD.refs.$("<td></td>").text(info.author).appendTo(row);
					
					//version
					if(info.version !== ""){
						JOBAD.refs.$("<td></td>").text(info.version).appendTo(row);
					} else {
						JOBAD.refs.$("<td><span class='JOBAD JOBAD_Repo JOBAD_Repo_NA'></span></td>").appendTo(row); 
					}

					//homepage
					if(typeof info.url == "string"){
						JOBAD.refs.$("<td></td>").append(
							JOBAD.refs.$("<a>").text(info.url).attr("href", info.url).attr("target", "_blank").button()
						).appendTo(row); 
					} else {
						JOBAD.refs.$("<td><span class='JOBAD JOBAD_Repo JOBAD_Repo_NA'></span></td>").appendTo(row); 
					}

					//description
					JOBAD.refs.$("<td></td>").text(info.description).appendTo(row);


					//dependencies (internal)
					var deps = info.dependencies;

					if(deps.length == 0){
						JOBAD.refs.$("<td></td>").text("(None)").appendTo(row);
					} else {
						var cell = JOBAD.refs.$("<td></td>").appendTo(row); 
						for(var j=0;j<deps.length;j++){
							cell.append("\"");
							if(JOBAD.util.indexOf(modules, deps[j]) == -1){
								cell.append(
									JOBAD.refs.$("<span>").addClass("JOBAD JOBAD_Repo JOBAD_Repo_Dependency JOBAD_Repo_Dependency_NotFound")
									.text("\""+deps[j]+"\"")
								)
							} else {
								cell.append(
									JOBAD.refs.$("<span>").addClass("JOBAD JOBAD_Repo JOBAD_Repo_Dependency JOBAD_Repo_Dependency_Found")
									.text(deps[j])
								)
							}
							cell.append("\"");
							if(j != deps.length - 1 ){
								cell.append(" , "); 
							}
						}
					}

					var edeps = info.externals.js;

					if(edeps.length == 0){
						JOBAD.refs.$("<td></td>").text("(None)").appendTo(row);
					} else {
						var cell = JOBAD.refs.$("<td></td>").appendTo(row); 
						for(var j=0;j<edeps.length;j++){
							cell.append(
								"\"", 
								JOBAD.refs.$("<span>")
									.addClass("JOBAD JOBAD_Repo JOBAD_Repo_External_Dependency")
									.text(edeps[j]),
								"\""
							);

							if(j != edeps.length - 1 ){
								cell.append(" , "); 
							}
						}
					}

					var edeps = info.externals.css;

					if(edeps.length == 0){
						JOBAD.refs.$("<td></td>").text("(None)").appendTo(row);
					} else {
						var cell = JOBAD.refs.$("<td></td>").appendTo(row); 
						for(var j=0;j<edeps.length;j++){
							cell.append(
								"\"", 
								JOBAD.refs.$("<span>")
									.addClass("JOBAD JOBAD_Repo JOBAD_Repo_External_Dependency")
									.text(edeps[j]),
								"\""
							);

							if(j != edeps.length - 1 ){
								cell.append(" , "); 
							}
						}
					}

				}

				delete moduleList[name];
				i++;
				next(); 
			})
		};
		
		next(); 
	})
}

var repo_is_initing = false; 

/*
	Register a given set of repositories
	@param baseUrl	Base urls of the repositories
	@param callback	Callback on success / failure
*/
JOBAD.repo.init = function(baseUrl, callback){

	var callback = JOBAD.util.forceFunction(callback, function(){}); 

	if(JOBAD.util.isArray(baseUrl)){
		if(baseUrl.length == 0){
			return callback(true); 
		} else {
			var now = baseUrl[0];
			var next = baseUrl.slice(1);

			return JOBAD.repo.init(now, function(){
				JOBAD.repo.init(next, callback)
			})
		}
	}

	if(JOBAD.repo.hasInit(baseUrl)){
		return callback(true);  //we already have init
	}

	//we have a single item
	if(repo_is_initing){
		return false; 
	}
	repo_is_initing = true; 

	var baseUrl = JOBAD.util.resolve(baseUrl); 

	var repo_cache; 

	JOBAD.repo.config = function(obj){
		repo_cache = obj; //cache it
	}

	JOBAD.util.loadExternalJS(JOBAD.util.resolve("jobad_repo.js", baseUrl), function(s){

		delete JOBAD.repo.config; //delete the function again

		if(!s){
			callback(false, "Repository Main File Timeout"); 
			repo_is_initing = false; 
			return; 
		}

		//parse it

		if(!JOBAD.util.isObject(repo_cache)){
			callback(false, "Repository in wrong Format: Not an object"); 
			repo_is_initing = false; 
			return; 
		}

		if(!JOBAD.util.isArray(repo_cache.versions)){
			callback(false, "Repository Spec in wrong Format: Versions missing or not an array. "); 
			repo_is_initing = false; 
			return; 
		}


		if(JOBAD.util.indexOf(repo_cache.versions, JOBAD.version) == -1){
			callback(false, "Repository incompatible with this version of JOBAD. "); 
			repo_is_initing = false; 
			return; 
		}

		if(!JOBAD.util.isArray(repo_cache.provides)){
			callback(false, "Repository Spec in wrong Format: Modules missing or not an array. "); 
			repo_is_initing = false; 
			return; 
		}


		var overrides = {};

		if(JOBAD.util.isObject(repo_cache.at)){
			overrides = repo_cache.at; 
		}

		var modules = repo_cache.provides.slice(0); //available modules
		
		JOBAD_Repo_Urls[baseUrl] = {}; //Create a new cache object

		for(var i=0;i<modules.length;i++){
			var key = modules[i]; 
			//is the url set manually
			if(overrides.hasOwnProperty(key)){
				JOBAD_Repo_Urls[baseUrl][key] = JOBAD.util.resolve(overrides[key], baseUrl); 
			} else {
				JOBAD_Repo_Urls[baseUrl][key] = JOBAD.util.resolve(key+".js", baseUrl);
			}


			//register the repo with the modules stuff
			if(JOBAD_Repo_Mods.hasOwnProperty(key)){
				JOBAD_Repo_Mods[key].push(baseUrl);
			} else {
				JOBAD_Repo_Mods[key] = [baseUrl]; 
			}
		}

		repo_is_initing = false;  //we are done
		callback(true, repo_cache); 
	});

}

/*
	Checks if a repository has been initialised
	@param	baseUrl	Repository to check. 
*/
JOBAD.repo.hasInit = function(baseUrl){
	return JOBAD_Repo_Urls.hasOwnProperty(baseUrl); 
}

/*
	Loads modules from a repository. 
	@param	repo	Repository to load module from. 
	@param	modules	Module to load
	@param	callback	Callback once finished. 
*/
JOBAD.repo.loadFrom = function(repo, modules, callback){

	var callback = JOBAD.util.forceFunction(callback, function(){}); 
	var modules = JOBAD.util.forceArray(modules); 
	var repo = JOBAD.util.resolve(repo);

	JOBAD.repo.init(repo, function(res, msg){

		//we failed to initialise it
		if(!res){
			return callback(false, "Repo init failed: "+msg);
		}

		if(!JOBAD.repo.provides(repo, modules)){
			return callback(false, "Modules are not provided by repo. ");
		}

		//only load thigns that we don't already have
		modules = JOBAD.util.filter(modules, function(mod){
			return !JOBAD.modules.available(mod, false);
		});

		var m2 = JOBAD.util.map(modules, function(mod){
			return JOBAD_Repo_Urls[repo][mod]; 
		});

		JOBAD.repo.__currentFile = undefined; 
		JOBAD.repo.__currentLoad = m2; 
		JOBAD.repo.__currentRepo = repo; 

		JOBAD.util.loadExternalJS(m2, function(suc){
			delete JOBAD.repo.__currentFile; 
			delete JOBAD.repo.__currentLoad; 
			delete JOBAD.repo.__currentRepo;  
			if(suc){
				callback(true)
			} else {
				callback(false, "Failed to load one or more Modules: Timeout")
			}
		}, undefined, function(u){
			JOBAD.repo.__currentFile = u; 
		});
	});
}

JOBAD.repo.loadAllFrom = function(repo, callback){

	var callback = JOBAD.util.forceFunction(callback, function(){}); 
	var repo = JOBAD.util.resolve(repo);

	JOBAD.repo.init(repo, function(res, msg){

		//we failed to initialise it
		if(!res){
			return callback(false, "Repo init failed: "+msg);
		}

		JOBAD.repo.loadFrom(repo, JOBAD.util.keys(JOBAD_Repo_Urls[repo]), callback); 
	});
}

/*
	Check if repo provides module
	@param	repo	Repositorie(s) to look in. Optional. 
	@param	module	Module(s) to look for. 
*/
JOBAD.repo.provides = function(repo, module){

	var Repos = JOBAD.util.forceArray(repo); 
	var Modules = JOBAD.util.forceArray(module); 
	
	if(typeof module == "undefined"){
		Modules = Repos; 
		Repos = JOBAD.util.keys(JOBAD_Repo_Urls); 
	}

	return JOBAD.util.lAnd(JOBAD.util.map(Modules, function(mod){
		return JOBAD.util.lOr(JOBAD.util.map(Repos, function(rep){
			//check repo for module safely
			var rep = JOBAD.util.resolve(rep);

			return JOBAD.util.indexOf(JOBAD_Repo_Mods[mod], rep) != -1; 
		}));
	}));

}

/*
	Provide a module
	@param	modules	Modules to provide. 
	@param	repos	Additionally to repos already available, check these. Optional. 
	@param	callback	Callback to use. 
	@param	provideDependencies	Should we provide depndencies?
*/
JOBAD.repo.provide = function(modules, repos, callback, provideDependencies){
	if(typeof repos == "function"){
		provideDependencies = callback;
		callback = repos; 
		repos = []; 
	}

	var callback = JOBAD.util.forceFunction(callback, function(){}); 

	var modules = JOBAD.util.forceArray(modules);
	var i = 0;
	var repos = JOBAD.util.forceArray(repos);
	var provideDependencies = JOBAD.util.forceBool(provideDependencies, true);

	var load_next = function(){
		if(i >= modules.length){
			return callback(true); 
		}

		var mod = modules[i]; 

		var repo = JOBAD_Repo_Mods[mod][0]; //take the first provider
		JOBAD.repo.loadFrom(repo, mod, function(suc, msg){
			if(!suc){
				callback(false, msg); 
			} else {
				if(provideDependencies){
					var deps = moduleList[mod].info.dependencies;

					if(!JOBAD.repo.provides(deps)){
						return callback(false, "Dependencies for module '"+mod+"' are not provided by any repo. ")
					}

					modules = JOBAD.util.union(modules, deps); //add dependencies in the end
				}
				
				i++; 
				load_next(); 
			}
		})
	}

	JOBAD.repo.init(repos, function(suc, msg){
		if(!JOBAD.repo.provides(modules)){
			return callback(false, "Modules are not provided by any repo. ");
		}
		if(suc){
			load_next();
		} else {
			callback(false, msg); 
		}
	})
}/* end   <JOBAD.repo.js> */
/* start <core/JOBAD.core.modules.js> */
/*
	JOBAD Core Module logic
		
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/


JOBAD.ifaces.push(function(me, args){

	//modules namespace
	this.modules = {};

	//Event namespace
	this.Event = JOBAD.util.EventHandler(this); 

	/*
		Triggers a handable event. 
		@param	evt	Name of event to trigger. 
		@param	param	Parameter for event. 
	*/
	this.Event.handle = function(evt, param){
		me.Event.trigger("event.handlable", [evt, param]); 
	}

	/*
		Binds a custom Event handler in a module. 
		@param	evt	Name of event to biond to. 
		@param	module JOABd.modules.loadedModule Instance to enable binding on.
		@param handleName Name of handle function to use. Should be a parameter of the module.  
	*/
	this.Event.bind = function(evt, module, handleName){
		if(module instanceof JOBAD.modules.loadedModule){
			me.Event.on(evt, function(){
				if(module.isActive()){
					var args = [me];
					for(var i=0;i<arguments.length;i++){
						args.push(arguments[i]); 
					}
					module[handleName].apply(module, args);
				}
			})
		} else {
			JOBAD.console.error("Can't bind Event Handler for '"+evt+"': module is not a loadedModuleInstance. ")
		}
	}

	var InstanceModules = {}; //Modules loaded
	var disabledModules = []; //Modules disabled

	var loadQuenue = []; //the quenue of things to load
	var loadQuenueIndex = -1; //the loadQuenueIndex

	var loadQuenueOptions = {}; //options Array
	var loadQuenueAutoActivate = []; //auto activate
	var execQuenue = []; //quenue of exec callbacks

	var loadFail = []; //load failed

	var isQuenueRunning = false;
	
	/*
		runs the load Quenue
	*/
	var runLoadQuenue = function(){
		isQuenueRunning = true; //we are running

		//run the quenue
		loadQuenueIndex++;
		if(loadQuenue.length > loadQuenueIndex){ //do we have memebers
			var modName = loadQuenue[loadQuenueIndex];
			var options = loadQuenueOptions[modName];
			if(typeof options == "undefined"){
				options = [];
			}
			doLoad(modName, options, runFinishQuenue);
		} else {
			runFinishQuenue(function(){
				loadQuenueIndex--; //reset the index
				isQuenueRunning = false; //STOP
			});
		}
	};

	var runFinishQuenue = function(next){
		window.setTimeout(function(){
			//run the finish quenue
			execQuenue = execQuenue.filter(function(m){
				if(me.modules.loaded(m[0])){
					try{
						m[1].call(m[2], me.modules.loadedOK(m[0]), m[0]);
					} catch(e){}
					return false;
				} else {
					return true;
				}
			});

			window.setTimeout(next, 0); //run the load quenue again
		}, 0);
	}

	/*
		Loads a module
		@param	module	Module to load. 
		@param	options	Options to pass to the module
	*/
	var doLoad = function(module, options){
		var auto_activate = loadQuenueAutoActivate.indexOf(module) != -1;
		try{
			InstanceModules[module] = new JOBAD.modules.loadedModule(module, options, me, function(suc, msg){
				if(!suc){
					markLoadAsFailed(module, msg);
				} else {
					disabledModules.push(module); //we are disabled by default
					me.Event.trigger("module.load", [module, options]); 

					if(auto_activate){
						me.modules.activate(module);
					}
				}
				runFinishQuenue(runLoadQuenue);
				
			});
		} catch(e){
			markLoadAsFailed(module, String(e));
			runFinishQuenue(runLoadQuenue);
		}
	 };

	var markLoadAsFailed = function(module, message){
		loadFail.push(module);
		me.Event.trigger("module.fail", [module, message]); 
		try{
			delete InstanceModules[module];
		} catch(e){}

		JOBAD.console.error("Failed to load module '"+module+"': "+String(message));
	};

	var properLoadObj = function(obj){
		var res = [];

		if(JOBAD.util.isArray(obj)){
			for(var i=0;i<obj.length;i++){
				var proper = JOBAD.util.forceArray(obj[i]);

				if(typeof proper[1] == "boolean"){
					proper[2] = proper[1];
					proper[1] = [];
				}

				res.push(proper); 
			}
		} else {
			for(var key in obj){
				res.push([key, obj[key]]);
			}
		}

		return res;
	};

	/*
		Loads a module
		@param	modules	Modules to load
		@param config	Configuration. 
			config.ready	Callback when ready
			config.load		Callback on singular load
			config.activate	Should we automatically activate all modules? Default: True
	*/
	this.modules.load = function(modules, config, option){
		if(typeof modules == "string"){
			if(JOBAD.util.isArray(config)){
				return me.modules.load([[modules, config]], option);
			} else {
				return me.modules.load([modules], config, option);
			}
		}

		config = JOBAD.util.defined(config);
		config = (typeof config == "function")?{"ready": config}:config; 
		config = (typeof config == "booelan")?{"activate": config}:config; 

		var ready = JOBAD.util.forceFunction(config.ready, function(){});
		var load = JOBAD.util.forceFunction(config.load, function(){});

		var activate = JOBAD.util.forceBool(config.activate, true);

		var triggers = [];

		var everything = JOBAD.util.map(properLoadObj(modules), function(m){
			var mod = m[0];
			var opt = m[1];
			var act = m[2];

			triggers.push(mod); //add to the trigegrs

			if(typeof loadQuenueOptions[mod] == "undefined" && JOBAD.util.isArray(opt)){
				loadQuenueOptions[mod] = opt;
			}

			if(typeof act == "undefined"){
				act = activate; 
			}

			if(act){
				loadQuenueAutoActivate.push(mod); //auto activate
			}

			if(!JOBAD.modules.available(mod)){
				markLoadAsFailed(mod, "Module not available. (Did loading fail?)");
				return [];
			}

			if(!me.modules.inLoadProcess(mod)){
				var deps = JOBAD.modules.getDependencyList(mod);
				if(!deps){
					markLoadAsFailed(mod, "Failed to resolve dependencies (Is a dependent module missing? )");
					return [];
				} else {
					return deps;
				}
			} else {
				if(act && me.modules.loadedOK(mod) && !me.modules.isActive(mod)){
					me.modules.activate(mod); 
				}
				return []; //it's already in load process
			}
		});

		//clean up
		everything = JOBAD.util.flatten(everything);
		everything = JOBAD.util.union(everything);
		triggers = JOBAD.util.union(triggers)

		//register all the callbacks
		JOBAD.util.map(triggers, function(m){
			me.modules.once(m, load, me, false);
		});
		me.modules.once(triggers, ready, me, false);

		//add them to the quenue and run it
		loadQuenue = JOBAD.util.union(loadQuenue, everything);

		if(!isQuenueRunning){
			runLoadQuenue(); 
		}
	}

	 /*
	 	Defers a callback until the specefied modules are loaded. 
	 */
	this.modules.once = function(modules, cb, scope, run){
		execQuenue.push([modules, cb, (typeof scope == "undefined")?me:scope]);

		var run = JOBAD.util.forceBool(run, true);

		//if we are inactive run the quenue
		if(!isQuenueRunning && run){
			runLoadQuenue(); 
		}
	 };

	/*
		Checks if a module is loaded. 
		@param module Name of the module(s) to check. 
		@returns boolean
	*/
	this.modules.loaded = function(module){
		return (me.modules.loadedOK(module) || me.modules.loadedFail(module)); 
	}

	this.modules.loadedOK = function(module){
		if(JOBAD.util.isArray(module)){
			return JOBAD.util.lAnd(JOBAD.util.map(module, function(m){
				return me.modules.loadedOK(m);
			}));
		} else {
			return InstanceModules.hasOwnProperty(module);
		}
	}

	this.modules.loadedFail = function(module){
		if(JOBAD.util.isArray(module)){
			return JOBAD.util.lOr(JOBAD.util.map(module, function(m){
				return me.modules.loadedFail(m);
			}));
		} else {
			return (JOBAD.util.indexOf(loadFail, module) != -1);
		}
	}

	this.modules.inLoadProcess = function(module){
		if(JOBAD.util.isArray(module)){
			return JOBAD.util.lAnd(JOBAD.util.map(module, function(m){
				return me.modules.inLoadProcess(m);
			}));
		} else {
			return (JOBAD.util.indexOf(loadQuenue, module) != -1);
		}
	};

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
		me.Event.trigger("module.deactivate", [InstanceModules[module]]); 
		me.Event.handle("deactivate", module); 
	}

	/*
		Activates a module if it is not yet actiavted
		@param module Module to be activated. 
	*/
	this.modules.activate = function(module){
	
		if(me.modules.isActive(module)){
			return false; 	
		}

		var todo = function(){
			if(me.modules.isActive(module)){
				return; 	
			}

			disabledModules = JOBAD.util.without(disabledModules, module);

			var deps = JOBAD.modules.getDependencyList(module);		
			for(var i=0;i<deps.length-1;i++){
				me.modules.activate(deps[i]); // i am last
			}
			

			InstanceModules[module].onActivate(me);
			me.Event.trigger("module.activate", [InstanceModules[module]]); 
			me.Event.handle("activate", module); 
		}

		if(me.Setup.isEnabled()){
			todo();
		} else {
			me.Event.once("instance.enable", todo); 
		}

		return true; 
	};
	
	/*
		Checks if a module is active. 
		@param module Module to check. 
	*/
	this.modules.isActive = function(module){
		return (JOBAD.util.indexOf(disabledModules, module)==-1 && me.modules.loadedOK(module)); 
	};

	/*
		Checks if a module is inactive. 
		@param module Module to check. 
	*/
	this.modules.isInActive = function(module){
		return (JOBAD.util.indexOf(disabledModules, module)!=-1 && me.modules.loadedOK(module)); 
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
	
	
	var onDisable = function(){
		var cache = [];
		
		//cache all the modules
		me.modules.iterate(function(mod){
			var name = mod.info().identifier;
			cache.push(name);
			me.modules.deactivate(name);
			return true;
		});
		
		//reactivate all once setup is called again
		me.Event.once("instance.enable", function(){
			for(var i=0;i<cache.length;i++){
				var name = cache[i];
				if(!me.modules.isActive(name)){
					me.modules.activate(name);
				}
			}
			me.Event.once("instance.disable", onDisable); //reregister me
		});
	};
	

	this.Event.once("instance.disable", onDisable); 
	
	this.modules = JOBAD.util.bindEverything(this.modules, this);
});

JOBAD.modules = {};
JOBAD.modules.extensions = {}; //Extensions for modules
JOBAD.modules.ifaces = []; //JOABD Module ifaces

JOBAD.modules.cleanProperties = ["init", "activate", "deactivate", "globalinit", "info"];

var moduleList = {};
var moduleStorage = {};
var moduleOrigins = {}; 

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
		//set the origins
		if(JOBAD.repo.__currentFile){
			moduleOrigins[identifier] = [JOBAD.repo.__currentFile, JOBAD.repo.__currentLoad, JOBAD.repo.__currentRepo]; //The current origin
		} else {
			moduleOrigins[identifier] = [JOBAD.util.getCurrentOrigin()];
		}

		//resolving all the relative urls
		if(moduleObject.info.url){
			moduleObject.info.url = JOBAD.modules.resolveModuleResourceURL(identifier, moduleObject.info.url); 
		}

		moduleObject.info.externals.js = JOBAD.util.map(moduleObject.info.externals.js, function(e){
			return JOBAD.modules.resolveModuleResourceURL(identifier, e); 
		});

		moduleObject.info.externals.css = JOBAD.util.map(moduleObject.info.externals.css, function(e){
			return JOBAD.modules.resolveModuleResourceURL(identifier, e); 
		});

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
	if(!JOBAD.util.isObject(ModuleObject)){
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

		if(info.hasOwnProperty('externals')){
			if(JOBAD.util.isArray(info["externals"])){
				properObject.info.externals = {"js": info["externals"], "css": []};
			} else if(JOBAD.util.isObject(info["externals"])){
				properObject.info.externals = {}
				if(info["externals"].hasOwnProperty("css")){
					if(!JOBAD.util.isArray(info["externals"].css)){
						return false; 
					}
					properObject.info.externals.css = info["externals"].css; 
				}
				if(info["externals"].hasOwnProperty("js")){
					if(!JOBAD.util.isArray(info["externals"].js)){
						return false; 
					}
					properObject.info.externals.js = info["externals"].js; 
				}
			} else {
				return false;
			}
		} else {
			properObject.info.externals = {"js":[], "css": []};
		}

		if(info.hasOwnProperty('async')){
			properObject.info.async = JOBAD.util.forceBool(info.async);
		} else {
			properObject.info.async = false;
		}

		if(!properObject.info.async){
			var sync_init = properObject.globalinit;
			properObject.globalinit = function(next){
				sync_init(); 
				window.setTimeout(next, 0);
			}
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
			if(!JOBAD.util.isArray(arr)){
				return false;			
			}
			properObject.info['dependencies'] = arr;
		}

		if(info.hasOwnProperty('url')){
			if(!JOBAD.util.isUrl(info.url)){
				return false;			
			}
			properObject.info['url'] = info.url;
		} else {
			info.url = false; 
		}

		try{
			JOBAD.util.map(['identifier', 'title', 'author', 'description'], function(key){
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

		for(var key in ModuleObject){
			if(ModuleObject.hasOwnProperty(key) && JOBAD.util.indexOf(JOBAD.modules.cleanProperties, key) == -1){
				if(properObject.info.hasCleanNamespace){
					JOBAD.console.warn("Warning: Module '"+properObject.info.identifier+"' says its namespace is clean, but property '"+key+"' found. Check ModuleObject.info.hasCleanNamespace. ");	
				} else {
					properObject.namespace[key] = ModuleObject[key];
				}
			}
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

			if(typeof mod["onRegister"] == "function"){
				mod.onRegister(properObject[key], properObject, ModuleObject);
			}
		}
		
		for(var i=0;i<JOBAD.modules.ifaces.length;i++){
			var mod = JOBAD.modules.ifaces[i];
			properObject = mod[0].call(this, properObject, ModuleObject);
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
	Gets the origin of a module. 
	@param	name Name of module to get origin from. 
	@param  what what kind of orgin to get. (Optional, "file" or "group", otehrwise "repo"))
*/
JOBAD.modules.getOrigin = function(name, what){
	var origin = moduleOrigins[name]; 
	if(JOBAD.util.equalsIgnoreCase(what, "file")){
		return origin[0]; 
	} else if(JOBAD.util.equalsIgnoreCase(what, "group")){
		return origin[1] || [origin[0]]; 
	} else {
		return origin[2]; 
	}
}

/*
	Resolves a resource URL for the specefied module. 
	@param	mod	Name of module to resolve url for. 
	@param	url Url to resolve. 
*/
JOBAD.modules.resolveModuleResourceURL = function(mod, url){
	var origin = JOBAD.modules.getOrigin(mod, "file");
	origin = origin.substring(0, origin.lastIndexOf('/'));
	return JOBAD.util.resolve(url, origin); 
}

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
		depArray = JOBAD.util.union(depArray, JOBAD.modules.getDependencyList(deps[i]));
	}

	depArray.reverse(); //reverse it

	return depArray;
};

/*
	Storage shared accross all module instances. 
*/
JOBAD.modules.globalStore = 
{
	"get": function(name, prop){
		if(JOBAD.util.isObject(prop) && !JOBAD.util.isArray(prop)){
			var prop = JOBAD.util.keys(prop);
		}

		if(JOBAD.util.isArray(prop)){
			var res = {};

			JOBAD.util.map(prop, function(key){
				res[key] = JOBAD.modules.globalStore.get(name, key); 
			});

			return res;
		}

		return  moduleStorage[name][prop+"_"];
	},
	"set": function(name, prop, val){
		if(JOBAD.util.isObject(prop) && !JOBAD.util.isArray(prop)){
			var prop = JOBAD.util.keys(prop);
		}

		if(JOBAD.util.isArray(prop)){

			return JOBAD.util.map(prop, function(key){
				return JOBAD.modules.globalStore.set(name, key, prop[key]); 
			});
		}

		moduleStorage[name][prop+"_"] = val;

		return prop;
	},
	"delete": function(name, prop){
		delete moduleStorage[name][prop+"_"];
	},
	"keys": function(name){
		var keys = [];
		for(var key in moduleStorage[name]){
			if(moduleStorage[name].hasOwnProperty(key) && key[key.length-1] == "_"){
				keys.push(key.substr(0, key.length-1));
			}
		}
		return keys;
	},
	"getFor": function(name){
		return {
			"get": function(prop){
				return JOBAD.modules.globalStore.get(name, prop);	
			},
			"set": function(prop, val){
				return JOBAD.modules.globalStore.set(name, prop, val);	
			},
			"delete": function(prop){
				return JOBAD.modules.globalStore["delete"](name, prop);	
			},
			"keys": function(){
				JOBAD.modules.globalStore.keys(name);	
			}
		};
	}
};

/*
	Loads a module, assuming the dependencies are already available. 
	@param name Module to loads
	@param args Arguments to pass to the module. 
	@returns new JOBAD.modules.loadedModule instance. 
*/
JOBAD.modules.loadedModule = function(name, args, JOBADInstance, next){

	var me = this;
	var next = JOBAD.util.forceFunction(next, function(){});

	if(!JOBAD.modules.available(name)){
		JOBAD.error("Module is not available and cant be loaded. ");	
	}

	if(!JOBAD.util.isArray(args)){
		var args = []; //we force arguments
	}

	this.globalStore = JOBAD.modules.globalStore.getFor(name);
	
	var storage = {};
	/*
		Storage contained per instance of the module.  
	*/
	this.localStore = 
	{
		"get": function(prop){
			if(JOBAD.util.isObject(prop) && !JOBAD.util.isArray(prop)){
				var prop = JOBAD.util.keys(prop);
			}

			if(JOBAD.util.isArray(prop)){
				var res = {};

				JOBAD.util.map(prop, function(key){
					res[key] = storage[key];
				});

				return res;
			}
			return  storage[prop];		
		},
		"set": function(prop, val){
			if(JOBAD.util.isObject(prop) && !JOBAD.util.isArray(prop)){
				var prop = JOBAD.util.keys(prop);
			}

			if(JOBAD.util.isArray(prop)){

				return JOBAD.util.map(prop, function(key){
					storage[key] = prop[key];
				});

			}

			storage[prop] = val;
			return prop;
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

	/*
		Gets the origin of this module. 
	*/
	this.getOrigin = function(what){
		return JOBAD.modules.getOrigin(name, what); 
	}


	this.isActive = function(){
		return JOBADInstance.modules.isActive(this.info().identifier);
	}

	//add JOBADINstance
	var params = args.slice(0);
	params.unshift(JOBADInstance);

	var limited = {}; //limited this for globalinit
	limited.info = ServiceObject.info;
	limited.globalStore = this.globalStore;

	if(JOBAD.config.cleanModuleNamespace){
		if(!ServiceObject.info.hasCleanNamespace){
			JOBAD.console.warn("Warning: Module '"+name+"' may have unclean namespace, but JOBAD.config.cleanModuleNamespace is true. ");		
		}

	} else {
		var orgClone = JOBAD.util.clone(ServiceObject.namespace);
		
		for(var key in orgClone){
			if(!JOBAD.modules.cleanProperties.hasOwnProperty(key) && orgClone.hasOwnProperty(key)){
				this[key] = orgClone[key];
				limited[key] = orgClone[key];
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
		if(JOBAD.util.isArray(mod["globalProperties"])){
			for(var i=0;i<mod["globalProperties"].length;i++){
				var prop = mod["globalProperties"][i];
				limited[prop] = this[prop];
			}
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

	// .setHandler, scoped alias for .Event.bind
	this.setHandler = function(evt, handleName){
		this.getJOBAD().Event.bind(evt, me, handleName); 
	}

	var do_next = function(){
		ServiceObject.init.apply(me, params); 
		next.call(me, true);
	};

	if(!moduleStorage[name]["init"]){
		moduleStorage[name]["init"] = true;

		for(var key in JOBAD.modules.extensions){
			var mod = JOBAD.modules.extensions[key];
			var val = ServiceObject[key];
			if(typeof mod["onFirstLoad"] == 'function'){
				mod.onFirstLoad(me.globalStore);
			}
		}

		JOBAD.util.loadExternalCSS(ServiceObject.info.externals.css, function(urls, suc){
			if(!suc){
				next(false, "Can't load external CSS dependencies: Timeout. "); 
			} else {
				JOBAD.util.loadExternalJS(ServiceObject.info.externals.js, function(urls, suc){
					if(!suc){
						next(false, "Can't load external JavaScript dependencies: Timeout. "); 
					} else {
						ServiceObject.globalinit.call(limited, do_next);
					}
					
				});
			}
			
		});

		
	} else {
		do_next();
	}
	
	
};/* end   <core/JOBAD.core.modules.js> */
/* start <core/JOBAD.core.setup.js> */
/*
	JOBAD Core Event Logic
		
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/


//These are special events
//Do not setup these
var SpecialEvents = ["on", "off", "once", "trigger", "bind", "handle"]; 

//preEvent and postEvent Handlers 
//will be called for module-style events

var preEvent = function(me, event, params){
	me.Event.trigger("event.before."+event, [event, params]);
};

var postEvent = function(me, event, params){
	me.Event.trigger("event.after."+event, [event, params]);
	me.Event.handle(event, params); 
};


//Provides custom events for modules
JOBAD.ifaces.push(function(me, args){

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
		Checks if this Instance is enabled. 
	*/
	this.Setup.isEnabled = function(){
		return enabled;
	};


	/*
		Calls the function cb if this JOBADINstance is enabled, 
		otherwise calls it once this JOBADInstance is enabled. 
	*/
	this.Setup.enabled = function(cb){
		var cb = JOBAD.util.forceFunction(cb).bind(me); 

		if(enabled){
			cb(); 
		} else {
			me.Event.once("instance.enable", cb);
		}
	}

	/*
		Calls the function cb if this JOBADINstance is disabled, 
		otherwise calls it once this JOBADInstance is disbaled. 
	*/
	this.Setup.disabled = function(cb){
		var cb = JOBAD.util.forceFunction(cb).bind(me); 

		if(!enabled){
			cb(); 
		} else {
			me.Event.once("instance.disable", cb);
		}
	}

	
	/*
		Defer an event until JOBAD is enabled. 
		depracated
	*/
	this.Setup.deferUntilEnabled = function(func){
		JOBAD.console.warn("deprecated: .Setup.deferUntilEnabled, use .Event.once('instance.enable', callback) instead. "); 
		me.Event.once("instance.enable", func); 
	};
	
	/*
		Defer an even until JOBAD is disabled
	*/
	this.Setup.deferUntilDisabled = function(func){
		JOBAD.console.warn("deprecated: .Setup.deferUntilDisabled, use .Event.once('instance.disable', callback) instead. "); 
		me.Event.once("instance.disable", func);
	};
	
	/*
		Enables this JOBAD instance 
		@returns boolean indicating success. 
	*/
	this.Setup.enable = function(){
		if(enabled){
			return false;
		}

		var root = me.element;

		me.Event.trigger("instance.beforeEnable", []);

		for(var key in me.Event){
			if(JOBAD.util.contains(SpecialEvents, key)){
				continue;
			}

			try{
				JOBAD.events[key].Setup.enable.call(me, root);
			} catch(e){
				JOBAD.console.error("Failed to enable Event '"+key+"': "+e.message);
				JOBAD.console.error(e); 
			}
			
		}

		enabled = true; //we are enabled;
		var res = me.Event.trigger("instance.enable", []); 	

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

		me.Event.trigger("instance.beforeDisable", []);

		for(var key in JOBAD.events){
			if(JOBAD.util.contains(SpecialEvents, key)){
				continue;
			}
			if(JOBAD.events.hasOwnProperty(key) && !JOBAD.isEventDisabled(key)){
				try{
					JOBAD.events[key].Setup.disable.call(me, root);
				} catch(e){
					JOBAD.console.error("Failed to disable Event '"+key+"': "+e.message);
					JOBAD.console.error(e); 
				}
			}	
		}


		enabled = false;
		me.Event.trigger("instance.disable", []); 

		return true;
	};

	//Setup the events
	for(var key in JOBAD.events){
		if(JOBAD.events.hasOwnProperty(key) && !JOBAD.isEventDisabled(key)){

			me.Event[key] = JOBAD.util.bindEverything(JOBAD.events[key].namespace, me);
			
			(function(){
				var k = key; 
				var orgResult = me.Event[k].getResult; 
				me.Event[k].getResult = function(){
					var augmentedResult = me.Event.trigger(k, arguments); //Trigger the event
					var realResult = orgResult.apply(this, arguments); 
					return realResult; 
				}
			})()

			if(typeof JOBAD.events[key].Setup.init == "function"){
				JOBAD.events[key].Setup.init.call(me, me);
			} else if(typeof JOBAD.events[key].Setup.init == "object"){
				for(var name in JOBAD.events[key].Setup.init){
					if(JOBAD.events[key].Setup.init.hasOwnProperty(name)){
						if(me.hasOwnProperty(name)){
							JOBAD.console.warn("Setup: Event '"+key+"' tried to override '"+name+"'")
						} else {
							me[name] = JOBAD.util.bindEverything(JOBAD.events[key].Setup.init[name], me);
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
		for(var key in JOBAD.events){
			if(ModuleObject.hasOwnProperty(key)){
				properObject[key] = ModuleObject[key];
			}
		}
		return properObject;
	},
	function(ServiceObject){
		for(var key in JOBAD.events){
			if(ServiceObject.hasOwnProperty(key)){
				this[key] = ServiceObject[key];
			} else {
				this[key] = JOBAD.events[key]["default"];
			}
		}
	}]);

/*
	Checks if an Event is disabled by the configuration. 
	@param evtname Name of the event that is disabled. 
*/
JOBAD.isEventDisabled = function(evtname){
	return (JOBAD.util.indexOf(JOBAD.config.disabledEvents, evtname) != -1);
};

JOBAD.events = {};

//config
JOBAD.config.disabledEvents = []; //Disabled events
JOBAD.config.cleanModuleNamespace = false;//if set to true this.loadedModule instances will not allow additional functions/* end   <core/JOBAD.core.setup.js> */
/* start <ui/JOBAD.ui.js> */
/*
	JOBAD 3 UI Functions
	JOBAD.ui.js
		
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

// This file contains general JOBAD.UI functions

//Mouse coordinates
var mouseCoords = [0, 0];


JOBAD.refs.$(document).on("mousemove.JOBADListener", function(e){
	mouseCoords = [e.pageX-JOBAD.refs.$(window).scrollLeft(), e.pageY-JOBAD.refs.$(window).scrollTop()];
});

//UI Namespace. 
JOBAD.UI = {}


/*
	highlights an element
*/
JOBAD.UI.highlight = function(element, color){
	var element = JOBAD.refs.$(element);
	var col;
	if(typeof element.data("JOBAD.UI.highlight.orgColor") == 'string'){
		col = element.data("JOBAD.UI.highlight.orgColor");
	} else {
		col = element.css("backgroundColor");
	}
	
	element
	.stop().data("JOBAD.UI.highlight.orgColor", col)
	.animate({ backgroundColor: (typeof color == 'string')?color:"#FFFF9C"}, 1000);	
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
};

/*
	Sorts a table. 
	@param	el	Table Row Head to sort. 
	@param sortFunction	A function, `ascending`, `descending`, `reset` or `rotate`
	@param callback(state) Callback only for rotate state. 0 = originial, 1=ascend, 2=descend
*/
JOBAD.UI.sortTableBy = function(el, sortFunction, callback){
	//get the table
	var el = JOBAD.refs.$(el); 
	var table = el.closest("table"); //the table
	var rows = JOBAD.refs.$("tbody > tr", table); //find the rows
	var colIndex = el.parents().children().index(el); //colIndex

	if(!table.data("JOBAD.UI.TableSort.OrgOrder")){
		table.data("JOBAD.UI.TableSort.OrgOrder", rows); 
	}

	if(typeof sortFunction == "undefined"){
		sortFunction = "rotate"; 
	}
	if(typeof sortFunction == "string"){
		if(sortFunction == "rotate"){
			var now = el.data("JOBAD.UI.TableSort.rotationIndex");
			if(typeof now != "number"){
				now = 0;
			}
			
			now = (now+1) % 3; 

			sortFunction = ["reset", "ascending", "descending"][now]; 

			el.data("JOBAD.UI.TableSort.rotationIndex", now); 

			callback.call(el, now); 
		}

		if(sortFunction == "ascending"){
			var sortFunction = function(a, b){return (a>b)?1:0; };
		} else if(sortFunction == "descending"){
			var sortFunction = function(a, b){return (a<b)?1:0; };
		} else if(sortFunction == "reset"){
			table.data("JOBAD.UI.TableSort.OrgOrder").each(function(){
				var row = JOBAD.refs.$(this); 
				row.detach().appendTo(table); 
			});
			return el;
		}
	}

	rows.sort(function(a, b){
		var textA = JOBAD.refs.$("td", a).eq(colIndex).text();
		var textB = JOBAD.refs.$("td", b).eq(colIndex).text();
		return sortFunction(textA, textB); 
	}).each(function(i){
		var row = JOBAD.refs.$(this); 
		row.detach().appendTo(table); 
	})

	return el; 
}

/*
	Making Bootstrap scoped. 
*/

/*
	Enables Bootstrap on some element
*/
JOBAD.refs.$.fn.BS = function(){
	JOBAD.refs.$(this).addClass(JOBAD.config.BootstrapScope); 
	return this; 
}

JOBAD.UI.BS = function(element){
	return JOBAD.refs.$(element).BS(); 
}

var Bootstrap_hacks = JOBAD.refs.$([]); 

JOBAD.UI.BSStyle = function(element){
	//Remove all the old hacks
	Bootstrap_hacks = Bootstrap_hacks.filter(function(){
		var me = JOBAD.refs.$(this); 

		if(me.children().length == 0){
			me.remove(); 
			return false; 
		}

		return true; 
	});


	var el = JOBAD.refs.$(".modal-backdrop").wrap(JOBAD.refs.$("<div>").BS().addClass("hacked")); 

	Bootstrap_hacks = Bootstrap_hacks.add(el.parent()); 
}/* end   <ui/JOBAD.ui.js> */
/* start <ui/JOBAD.ui.hover.js> */
/*
	JOBAD 3 UI Functions - Hover Text
	JOBAD.ui.hover.js

	Copyright (C) 2013-17 KWARC Group <kwarc.info>

	This file is part of JOBAD.

	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

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

	// if we are already active, remove the old tooltip. 
	if(hoverActive){
		JOBAD.UI.hover.disable();
	}

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
/* end   <ui/JOBAD.ui.hover.js> */
/* start <ui/JOBAD.ui.contextmenu.js> */
/*
	JOBAD 3 UI Functions - Context Menu
	JOBAD.ui.contextmenu.js

	Copyright (C) 2013-17 KWARC Group <kwarc.info>

	This file is part of JOBAD.

	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

//Context Menu UI
JOBAD.UI.ContextMenu = {}

JOBAD.UI.ContextMenu.config = {
	'margin': 20, //margin from page borders
	'width': 250, //menu width
	'radiusConst': 50, //Radius spacing constant
	'radius': 20 //small radius size
};

var ContextMenus = JOBAD.refs.$([]);

/*
	Registers a context menu on an element.
	@param	element jQuery element to register on.
	@param	demandFunction Function to call to get menu.
	@param	config	Configuration.
		config.type Optional. Type of menu to use. 0 => jQuery UI menu, 1 => Pie menu.
		config.open Optional. Will be called before the context menu is opened.
		config.close Optional. Will be called after the context menu has been closed.
		config.callback Optional. Will be called after a menu callback was called.
		config.parentMenus	Optional. Parent menus not to remove on trigger.
		config.element	Optional. Force to use this as an element for searching for menus.
		config.callBackTarget	Optional. Element to use for callback.
		config.callBackOrg	Optional. Element to use for callback.
		config.unbindListener	Optional. Element to listen to for unbinds.
		config.block	Optional. Always block the Browser context menu.
		config.stopPropagnate	Optional. If set to true, will not propagnate menus (check parents if no menu is available).
	@return the jquery element.
*/
JOBAD.UI.ContextMenu.enable = function(element, demandFunction, config){

	var element = JOBAD.refs.$(element);
	var config = JOBAD.util.defined(config);

	if(typeof demandFunction != 'function'){
		JOBAD.console.warn('JOBAD.UI.ContextMenu.enable: demandFunction is not a function. '); //die
		return element;
	}

	//Force functions for all of this
	var typeFunction = JOBAD.util.forceFunction(config.type);
	var onCallBack = JOBAD.util.forceFunction(config.callback, true);
	var onOpen = JOBAD.util.forceFunction(config.open, true);
	var onShow = JOBAD.util.forceFunction(config.show, true);
	var onEmpty = JOBAD.util.forceFunction(config.empty, true);
	var onClose = JOBAD.util.forceFunction(config.close, true);
	var parents = JOBAD.refs.$(config.parents);
	var block = JOBAD.util.forceBool(config.block, false);
	var stop = JOBAD.util.forceBool(config.stopPropagnate, false);

	//are we on a mac
	var isMac = (navigator.appVersion.indexOf("Mac")!=-1);

	element.on('contextmenu.JOBAD.UI.ContextMenu', function(e){
		//control overrides

		//for a mac we want to check if we have cmd key pressed, for others the control key
		if(isMac){
			if(e.metaKey){
				return true;
			}
		} else {
			if(e.ctrlKey){
				return true;
			}
		}

		//are we hidden => do not trigger
		if(JOBAD.util.isHidden(e.target)){
			return false; //we're hidden
		}

		//get the targetElement
		var targetElement = (config.element instanceof JOBAD.refs.$)?config.element:JOBAD.refs.$(e.target);
		var orgElement = targetElement;

		var result = false;
		while(true){
			result = demandFunction(targetElement, orgElement);
			result = JOBAD.UI.ContextMenu.generateMenuList(result); //generate the menu
			if(result.length > 0 || element.is(this) || stop){
				break;
			}
			targetElement = targetElement.parent();
		}


		JOBAD.UI.ContextMenu.clear(config.parents);

		//we are empty => allow browser to handle stuff
		if(result.length == 0 || !result){
			onEmpty(orgElement);
			return !block;
		}

		//trigger the open callback
		onOpen(element);

		//get the type of the menu
		var menuType = typeFunction(targetElement, orgElement);
		if(typeof menuType == "undefined"){
			menuType = 0;
		}

		//
		var cbTarget = JOBAD.util.ifType(config.callBackTarget, JOBAD.refs.$, targetElement);
		var oTarget = JOBAD.util.ifType(config.callBackOrg, JOBAD.refs.$, orgElement);

		//create the context menu element
		var menuBuild = JOBAD.refs.$("<div>")
		.addClass("JOBAD JOBAD_Contextmenu")
		.appendTo(JOBAD.refs.$("body"))
		.data("JOBAD.UI.ContextMenu.renderData", {
			"items": result,
			"targetElement": cbTarget,
			"orgElement": oTarget,
			"coords": mouseCoords.slice(0),
			"callback": onCallBack
		});


		//a handler for closing
		var closeHandler = function(e){
			menuBuild
			.remove();
			onClose(element);
		};


		//switch between menu types
		if(menuType == 0 || JOBAD.util.equalsIgnoreCase(menuType, 'standard')){
			//build the standard menu
			menuBuild
			.data("JOBAD.UI.ContextMenu.menuType", "standard")
			.append(
				JOBAD.UI.ContextMenu.buildContextMenuList(
					result,
					cbTarget,
					oTarget,
				onCallBack)
				.show()
				.dropdown()
			).on('contextmenu', function(e){
				return (e.ctrlKey);
			});

		} else if(menuType == 1 || JOBAD.util.equalsIgnoreCase(menuType, 'radial')){



			//build the radial menu

			var eventDispatcher = JOBAD.refs.$("<span>");

			JOBAD.refs.$(document).trigger('JOBAD.UI.ContextMenu.unbind'); //close all other menus

			menuBuild
			.data("JOBAD.UI.ContextMenu.menuType", "radial")
			.append(
				JOBAD.UI.ContextMenu.buildPieMenuList(result,
					cbTarget,
					oTarget,
					onCallBack,
					mouseCoords[0],
					mouseCoords[1]
				)
				.find("div")
				.click(function(){
					eventDispatcher.trigger('JOBAD.UI.ContextMenu.unbind'); //unbind all the others.
					JOBAD.refs.$(this).trigger("contextmenu.JOBAD.UI.ContextMenu"); //trigger my context menu.
					return false;
				}).end()
			);

			JOBAD.UI.ContextMenu.enable(menuBuild, function(e){
				return JOBAD.refs.$(e).closest("div").data("JOBAD.UI.ContextMenu.subMenuData");
			}, {
				"type": 0,
				"parents": parents.add(menuBuild),
				"callBackTarget": targetElement,
				"callBackOrg": orgElement,
				"unbindListener": eventDispatcher,
				"open": function(){
					eventDispatcher.trigger('JOBAD.UI.ContextMenu.unbind'); //unbind all the others.
				},
				"empty": function(){
					eventDispatcher.trigger('JOBAD.UI.ContextMenu.unbind');
				},
				"callback": closeHandler,
				"block": true,
				"stopPropagnate": stop
			});
		} else {
			JOBAD.console.warn("JOBAD.UI.ContextMenu: Can't show Context Menu: Unkown Menu Type '"+menuType+"'. ")
			return true;
		}


		//set its css and append it to the body

		menuBuild
		.BS() //enable Bootstrap on the menu
		.css({
			'width': JOBAD.UI.ContextMenu.config.width,
			'position': 'fixed'
		}).css({
			"left": Math.min(mouseCoords[0], window.innerWidth-menuBuild.outerWidth(true)-JOBAD.UI.ContextMenu.config.margin),
			"top":  Math.min(mouseCoords[1], window.innerHeight-menuBuild.outerHeight(true)-JOBAD.UI.ContextMenu.config.margin)
		})
		.on('mousedown', function(e){
			e.stopPropagation();//prevent closemenu from triggering
		});

		onShow(menuBuild, result);


		//unbind listener
		JOBAD.refs.$(document).add(config.unbindListener).add(menuBuild)
		.on("JOBAD.UI.ContextMenu.hide", function(){
			menuBuild.hide();
		})
		.on('JOBAD.UI.ContextMenu.unbind', function(e){
				closeHandler();
				JOBAD.refs.$(document).unbind('mousedown.UI.ContextMenu.Unbind JOBAD.UI.ContextMenu.unbind JOBAD.UI.ContextMenu.hide');
				e.stopPropagation();
				ContextMenus = ContextMenus.not(menuBuild);
		});

		JOBAD.refs.$(document)
		.one('mousedown.UI.ContextMenu.Unbind', function(){
			JOBAD.UI.ContextMenu.clear();
		})


		//add to all ContextMenus
		ContextMenus = ContextMenus.add(menuBuild);

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
	Clears all context menus.
	@param	keep	Menus to keep open.
*/
JOBAD.UI.ContextMenu.clear = function(keep){
	var keepers = ContextMenus.filter(keep);
	var clearers = ContextMenus.not(keep).trigger("JOBAD.UI.ContextMenu.unbind").remove();
	ContextMenus = keepers;
};


/*
	Builds the menu html element for a standard context menu.
	@param items The menu to build.
	@param element The element the context menu has been requested on.
	@param orgElement The element the context menu call originates from.
	@returns the menu element.
*/
JOBAD.UI.ContextMenu.buildContextMenuList = function(items, element, orgElement, callback){

	//get callback
	var cb = JOBAD.util.forceFunction(callback, function(){});

	//create ul
	var $ul = JOBAD.refs.$("<ul class='JOBAD JOBAD_Contextmenu'>").addClass("dropdown-menu");
	$ul
	.attr("role", "menu")
	.attr("aria-labelledby", "dropdownMenu");

	for(var i=0;i<items.length;i++){
		var item = items[i];

		//create current item
		var $li = JOBAD.refs.$("<li>").appendTo($ul);

		//create link
		var $a = JOBAD.refs.$("<a href='#' tabindex='-1'>")
		.appendTo($li)
		.text(item[0])
		.click(function(e){
			return false; //Don't follow link.
		});

		if(item[2] != "none" ){
			$a.prepend(
				JOBAD.refs.$("<span class='JOBAD JOBAD_InlineIcon'>")
				.css({
					"background-image": "url('"+JOBAD.resources.getIconResource(item[2])+"')"
				})
			)

		}

		if(typeof item[3] == "string"){
			$li.attr("id", item[3]);
		}

		(function(){
			if(typeof item[1] == 'function'){
				var callback = item[1];

				$a.on('click', function(e){
					JOBAD.refs.$(document).trigger('JOBAD.UI.ContextMenu.hide');
					callback(element, orgElement);
					cb(element, orgElement);
					JOBAD.refs.$(document).trigger('JOBAD.UI.ContextMenu.unbind');
				});
			} else if(item[1] === false){
				$a.parent().addClass("disabled");
			} else {
				$li.append(
					JOBAD.UI.ContextMenu.buildContextMenuList(item[1], element, orgElement, cb)
				).addClass("dropdown-submenu");
			}
		})()
	}
	return $ul;
};

/*
	Builds the menu html element for a pie context menu.
	@param items The menu to build.
	@param element The element the context menu has been requested on.
	@param orgElement The element the context menu call originates from.
	@returns the menu element.
*/
JOBAD.UI.ContextMenu.buildPieMenuList = function(items, element, orgElement, callback, mouseX, mouseY){
	//get callback
	var cb = JOBAD.util.forceFunction(callback, function(){});

	//position statics

	var r = JOBAD.UI.ContextMenu.config.radius;
	var n = items.length;
	var R = n*(r+JOBAD.UI.ContextMenu.config.radiusConst)/(2*Math.PI);


	//minimal border allowed
	var minBorder = R+r+JOBAD.UI.ContextMenu.config.margin;

	//get the reight positions
	var x = mouseX;
	if(x < minBorder){
		x = minBorder;
	} else if(x > window.innerWidth - minBorder){
		x = window.innerWidth - minBorder;
	}

	var y = mouseY;
	if(y < minBorder){
		y = minBorder;
	} else if(y > window.innerHeight - minBorder){
		var y = window.innerHeight - minBorder;
	}



	//create a container
	var $container = JOBAD.refs.$("<div class='JOBAD JOBAD_Contextmenu JOBAD_ContextMenu_Radial'>");
	for(var i=0;i<items.length;i++){
		var item = items[i];

		//compute position
		var t = (2*(n-i)*Math.PI) / items.length;
		var X = R*Math.sin(t)-r+x;
		var Y = R*Math.cos(t)-r+y;

		//create item and position
		var $item = JOBAD.refs.$("<div>").appendTo($container)
		.css({
			"top": y-r,
			"left": x-r,
			"height": 2*r,
			"width": 2*r
		}).addClass("JOBAD JOBAD_Contextmenu JOBAD_ContextMenu_Radial JOBAD_ContextMenu_RadialItem")

		$item.animate({
			"deg": 360,
			"top": Y,
			"left": X
		}, {
			"duration": 400,
			"step": function(d, prop) {
				if(prop.prop == "deg"){
					$container.find(".JOBAD_ContextMenu_RadialItem").css({transform: 'rotate(' + d + 'deg)'})
				}
			}
		});

		$item.append(
			JOBAD.refs.$("<img src='"+JOBAD.resources.getIconResource(item[2], {"none": "warning"})+"'>")
			.width(2*r)
			.height(2*r)
		);

		if(typeof item[3] == "string"){
			$item.attr("id", item[3]);
		}

		(function(){
			var text = JOBAD.refs.$("<span>").text(item[0]);
			if(typeof item[1] == 'function'){
				var callback = item[1];

				$item.click(function(e){
					JOBAD.UI.hover.disable();
					JOBAD.refs.$(document).trigger('JOBAD.UI.ContextMenu.hide');
					callback(element, orgElement);
					cb(element, orgElement);
					JOBAD.refs.$(document).trigger('JOBAD.UI.ContextMenu.unbind');
				});
			} else if(item[1] === false){
				$item.addClass("JOBAD_ContextMenu_Radial_Disabled");
			} else {
				$item.data("JOBAD.UI.ContextMenu.subMenuData", item[1]);
			}

			$item.hover(function(){
				JOBAD.UI.hover.enable(text, "JOBAD_Hover");
			}, function(){
				JOBAD.UI.hover.disable();
			})
		})()
	}

	return $container;
};


/*
	Generates a list menu representation from an object representation.
	@param menu Menu to generate.
	@returns the new representation.
*/
JOBAD.UI.ContextMenu.generateMenuList = function(menu){
	var DEFAULT_ICON = "none";
	if(typeof menu == 'undefined'){
		return [];
	}

	if(menu === false){
		return [];
	}

	var generateMenuItem = function(v){
		if(typeof v == "function" || v === false){
			return v;
		} else {
			return JOBAD.UI.ContextMenu.generateMenuList(v);
		}
	};

	var generateMenuConfig = function(a, b){
		var config = {
			"icon": DEFAULT_ICON,
			"id": false
		};

		var a = JOBAD.util.defined(a);
		var b = JOBAD.util.defined(b);

		if(typeof a == "string"){
			config["icon"] = a;
		} else if(JOBAD.util.isObject(a)){
			if(typeof a.icon == "string"){
				config["icon"] = a.icon;
			}
			if(typeof a["id"] == "string"){
				config["id"] = a["id"];
			}
			return config;
		}

		if(typeof b == "string"){
			config["id"] = b;
		} else if(JOBAD.util.isObject(b)){
			if(typeof b.icon == "string"){
				config["icon"] = b.icon;
			}
			if(typeof b["id"] == "string"){
				config["id"] = b["id"];
			}
		}


		return config;
	}

	var res = [];
	if(JOBAD.util.isArray(menu)){
		//We have an array as the menu
		//=> generate an object style representation
		var newmenu = {};

		for(var i=0;i<menu.length;i++){
			var key = menu[i][0];
			var val = menu[i].slice(1);

			newmenu[key] = val;
		}

		return (JOBAD.UI.ContextMenu.generateMenuList(newmenu));
	} else {
		for(var key in menu){
			if(menu.hasOwnProperty(key)){
				var val = menu[key];

				if(JOBAD.util.isArray(val)){
					//we have an array
					//check got everything individually
					if(val.length == 0){
						//nothing here, lets disable it
						res.push([key, false, DEFAULT_ICON, false]);
					} else if(val.length == 1){
						//we have exactly length 1
						//we should act as if we are only that.
						res.push([key, generateMenuItem(val[0]), DEFAULT_ICON, false]);
					} else if(val.length == 2 || val.length == 3){
						//we are of length 2 or 3 => generate config
						//if we have a string, then treat it as icon, otherwise assume a config style object

						var cfg = generateMenuConfig(val[1], val[2]); //generate the config

						res.push([key, generateMenuItem(val[0]), cfg["icon"], cfg["id"]]);
					} else {
						//overlength
						console.warn("JOBAD.UI.ContextMenu.generateMenuList: menu too long. ")
					}
				} else {
					//we are a single item.
					res.push([key, generateMenuItem(val), DEFAULT_ICON, false]);
				}
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
JOBAD.UI.ContextMenu.fullWrap = function(menu, wrapper){
	var menu = JOBAD.UI.ContextMenu.generateMenuList(menu);
	var menu2 = [];
	for(var i=0;i<menu.length;i++){
		if(typeof menu[i][1] == 'function'){
			(function(){
				var org = menu[i][1];
				menu2.push([menu[i][0], function(){
					return wrapper(org, arguments)
				}, menu[i][2]]);
			})();
		} else {
			menu2.push([menu[i][0], JOBAD.UI.ContextMenu.fullWrap(menu[i][1]), menu[i][2]]);
		}

	}
	return menu2;
};


JOBAD.UI.ContextMenu.updateMenu = function(callback){
	var callback = (typeof callback == "function")?callback:function(e){return e; };


	var menu = $("div.JOBAD_Contextmenu").eq(0);
	var renderData = menu.data("JOBAD.UI.ContextMenu.renderData");

	if(typeof renderData == "undefined"){
		return false; //no data found
	}

	if(menu.data("JOBAD.UI.ContextMenu.menuType") == "standard"){
		renderData["items"] = JOBAD.UI.ContextMenu.generateMenuList(callback(renderData["items"]));

		menu.data("JOBAD.UI.ContextMenu.renderData", renderData);

		var build = JOBAD.UI.ContextMenu.buildContextMenuList(
			renderData["items"],
			renderData["targetElement"],
			renderData["orgElement"],
			renderData["callback"]
		);

		//standard
		menu.empty().append(
			build
			.show()
			.dropdown()
		);

	} else {
			return false; //unsupported for now
	}

	return menu;
}
/* end   <ui/JOBAD.ui.contextmenu.js> */
/* start <ui/JOBAD.ui.sidebar.js> */
/*
	JOBAD 3 UI Functions
	JOBAD.ui.sidebar.js
		
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

JOBAD.UI.Sidebar = {}; 

JOBAD.UI.Sidebar.config = 
{
	"width": 50, //Sidebar Width
	"iconDistance": 15 //Minimal Icon distance
};

/*
	Wraps an element to create a sidebar UI. 
	@param element The element to wrap. 
	@param align Alignment of the new sidebar. 
	@returns the original element, now wrapped. 
*/
JOBAD.UI.Sidebar.wrap = function(element, align){

	var org = JOBAD.refs.$(element);

	if(org.length > 1){
        var me = arguments.callee;
        return org.each(function(i, e){
            me(e, align);
        });
    }

    //get alignement and sidebar css class
	var sbar_align = (align === 'left')?'left':'right';
	var sbar_class = "JOBAD_Sidebar_"+(sbar_align);


	//wrap the original element
	var orgWrapper = JOBAD.refs.$("<div>").css({"overflow": "hidden"});

	var sideBarElement = JOBAD.refs.$("<div class='JOBAD "+sbar_class+" JOBAD_Sidebar_Container'>").css({
		"width": JOBAD.UI.Sidebar.config.width
	});

	var container = JOBAD.refs.$("<div class='JOBAD "+sbar_class+" JOBAD_Sidebar_Wrapper'>");

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
			var el = JOBAD.util.closest(e.data("JOBAD.UI.Sidebar.orgElement"), function(element){
				//check if an element is visible
				return !JOBAD.util.isHidden(element);
			});
			var offset = el.offset().top - sideBarElement.offset().top; //offset
			e.data("JOBAD.UI.Sidebar.hidden", false);
			

			if(e.data("JOBAD.UI.Sidebar.orgElement").is(":hidden") && e.data("JOBAD.UI.Sidebar.hide")){
			     e.data("JOBAD.UI.Sidebar.hidden", true);
			} else {
			     e.data("JOBAD.UI.Sidebar.hidden", false);
			}

			e.css("top", offset).css(sbar_align, 0);
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
			(function(){
				//Group them together
				var unhidden_count = 0;
				var last_unhidden = -1;
				for(var j=0;j<groups[i].length;j++){
					if(!groups[i][j].data("JOBAD.UI.Sidebar.hidden")){
						unhidden_count++;
						last_unhidden = j;
					}
				}


				if(unhidden_count > 1){
						var group = JOBAD.refs.$(groups[i]);
						var top = parseInt(JOBAD.refs.$(group[0]).css("top"));

						var par = JOBAD.refs.$("<div class='JOBAD "+sbar_class+" JOBAD_Sidebar_Group'><img src='"+JOBAD.resources.getIconResource("open")+"' width='16' height='16'></div>")
						.css("top", top).appendTo(sideBarElement);

						par.on("contextmenu", function(e){return (e.ctrlKey)}); 

						var img = par.find("img");
						var state = false;
						par.click(function(){
							if(state){
								for(var j=0;j<group.length;j++){
									group[j].hide();
								}
								img.attr("src", JOBAD.resources.getIconResource("open"));
							} else {
								for(var j=0;j<group.length;j++){
									if(!group[j].data("JOBAD.UI.Sidebar.hidden")){
									    group[j].show();
									}
								}
								img.attr("src", JOBAD.resources.getIconResource("close"));
							}
							state = !state;
							
						});
						for(var j=0;j<group.length;j++){
							JOBAD.refs.$(group[j]).appendTo(par).hide().removeClass("JOBAD_Sidebar_Single").addClass("JOBAD_Sidebar_group_element")
							.css("top", -16)
							.css(sbar_align, 20);
						}
				} else {
					if(last_unhidden != -1){
						var single = JOBAD.refs.$(groups[i][last_unhidden]);
					    sideBarElement.append(JOBAD.refs.$(groups[i][last_unhidden]).removeClass("JOBAD_Sidebar_group_element").show());
					}
				     
				    for(var j=0;j<groups[i].length;j++){
				    	if(j != last_unhidden){
				    		sideBarElement.append(JOBAD.refs.$(groups[i][j]).removeClass("JOBAD_Sidebar_group_element"));
				    	}
				   	}
				     
				}
			})()
		}
	})
	
	JOBAD.refs.$(window).on("resize", function(){
		JOBAD.UI.Sidebar.forceNotificationUpdate();
	});

	org.data("JOBAD.UI.Sidebar.active", true);
	org.data("JOBAD.UI.Sidebar.align", sbar_align);
	return org;
};

/*
	Unwraps an element, destroying the sidebar. 
	@param The element which has a sidebar. 
	@returns the original element unwrapped. 
*/
JOBAD.UI.Sidebar.unwrap = function(element){
	var org = JOBAD.refs.$(element);

	if(org.length > 1){
        var me = arguments.callee;
        return org.each(function(i, e){
            me(e);
        });
    }
	
	if(!org.data("JOBAD.UI.Sidebar.active")){
		return;
	}
	
	org
	.unwrap()
	.parent()
	.find("div")
	.first().remove();

	org
	.removeData("JOBAD.UI.Sidebar.active")
	.removeData("JOBAD.UI.Sidebar.align");

	return org.unwrap();
};

/*
	Adds a new notification to the sidebar. Will be auto created if it does not exist. 
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
			config.hide: 	Should we hide this element (true) when it is not visible or travel up the dom tree (false, default)?
	@param align Alignment of sidebar if it still has to be created. 
	@returns an empty new notification element. 
*/
JOBAD.UI.Sidebar.addNotification = function(sidebar, element, config, align){	
	var config = (typeof config == 'undefined')?{}:config;
	
	var sbar = JOBAD.refs.$(sidebar);
	
	if(sbar.data("JOBAD.UI.Sidebar.active") !== true){
		sbar = JOBAD.UI.Sidebar.wrap(sbar, align); //init the sidebar first. 
	}
	
	var sbar_class = "JOBAD_Sidebar_"+((sbar.data("JOBAD.UI.Sidebar.align") === 'left')?'left':'right');
	
	var child = JOBAD.refs.$(element);
	var offset = child.offset().top - sbar.offset().top; //offset
	sbar = sbar.parent().parent().find("div").first();

	var newGuy =  JOBAD.refs.$("<div class='JOBAD "+sbar_class+" JOBAD_Sidebar_Notification'>").css({"top": offset}).appendTo(sbar)
	.data("JOBAD.UI.Sidebar.orgElement", element)
	.data("JOBAD.UI.Sidebar.isElement", true)
	.data("JOBAD.UI.Sidebar.hide", config.hide === true);
	
	//config
	if(typeof config.menu != 'undefined'){
		var entries = JOBAD.UI.ContextMenu.fullWrap(config.menu, function(org, args){
			return org.apply(newGuy, [element, config.menuThis]);
		});
		JOBAD.UI.ContextMenu.enable(newGuy, function(){return entries;});
	}
	
	
	if(config.hasOwnProperty("text")){
		newGuy.text(config.text);
	}

	var icon = false;
	var class_color = "#C0C0C0";
	var class_colors = {
		"info": "#00FFFF",
		"error": "#FF0000",
		"warning": "#FFFF00",
		"none": "#FFFF00"
	};

	config["class"] = (typeof config["class"] == "string")?config["class"]:"none";
		
	if(typeof config["class"] == 'string'){
		var notClass = config["class"];
		
		if(JOBAD.resources.available("icon", notClass)){
			icon = JOBAD.resources.getIconResource(notClass);
		}
		
		if(class_colors.hasOwnProperty(notClass)){
			class_color = class_colors[notClass];
		}
	}
	
	if(config.trace){
		//highlight the element
		newGuy.hover(
		function(){
			JOBAD.UI.Folding.show(element);
			JOBAD.UI.highlight(element, class_color);
		},
		function(){
			JOBAD.UI.unhighlight(element);
		});
	}
	
	if(typeof config.click == "function"){
		newGuy.click(config.click);
	} else {
		newGuy.click(function(){
			newGuy.trigger("contextmenu");
			return false;
		})
	}
	
	if(typeof config.icon == 'string'){
		icon =  JOBAD.resources.getIconResource(config.icon);
	}
	if(typeof icon !== 'string'){
		icon =  JOBAD.resources.getIconResource("none");
	}
	if(typeof icon == 'string'){
		newGuy.html("<img src='"+icon+"' width='16px' height='16px'>")
		.hover(function(){
			JOBAD.UI.hover.enable(JOBAD.refs.$("<div>").text(config.text).html(), "JOBAD_Sidebar_Hover JOBAD_Sidebar "+((typeof config["class"] == 'string')?" JOBAD_Notification_"+config["class"]:""));
		}, function(){
			JOBAD.UI.hover.disable();
		});
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
};/* end   <ui/JOBAD.ui.sidebar.js> */
/* start <ui/JOBAD.ui.overlay.js> */
/*
	JOBAD 3 UI Functions
	JOBAD.ui.overlay.js
		
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/
//JOBAD UI Overlay namespace. 
JOBAD.UI.Overlay = {};

/*
	Draws an overlay for an element. 
	@param	element			Element to draw overlay for. 
*/
JOBAD.UI.Overlay.draw = function(element){

	//trigger undraw
	var element = JOBAD.refs.$(element);
	var overlay_active = true;

	//trigger undraw
	JOBAD.UI.Overlay.undraw(element.find("div.ui-widget-overlay").parent());
	
	//get offset for element and draw it. 
	var offset = element.offset()
	var overlay_element = JOBAD.refs.$('<div>')
	.css({
		"position": "absolute",
		"top": offset.top
,		"left": offset.left,
		"width": JOBAD.refs.$(element).outerWidth(),
		"height": JOBAD.refs.$(element).outerHeight()
	})
	.addClass('ui-widget-overlay ui-front')
	.appendTo(element);

	JOBAD.util.markHidden(overlay_element); //hide the overlay element

	//listen for undraw
	element.one("JOBAD.UI.Overlay.undraw", function(){
		overlay_element.remove();
		overlay_active = false; //preven redrawing
	})

	JOBAD.refs.$(window).one("resize.JOBAD.UI.Overlay", function(){
		if(overlay_active){
			JOBAD.UI.Overlay.draw(element); //redraw me
		}
	});

	return overlay_element;
}

/*
	Removes the overlay from an element. 
	@param	element	element to remove overlay from. 
*/
JOBAD.UI.Overlay.undraw = function(element){
	return JOBAD.refs.$(element).trigger("JOBAD.UI.Overlay.undraw");
}

/*
	Readraws all overlays. 
	*/
JOBAD.UI.Overlay.redraw = function(){
	JOBAD.refs.$(window).trigger("resize.JOBAD.UI.Overlay");
}
/* end   <ui/JOBAD.ui.overlay.js> */
/* start <ui/JOBAD.ui.folding.js> */
/*
    JOBAD 3 UI Functions
    JOBAD.ui.folding.js
        
    Copyright (C) 2013-17 KWARC Group <kwarc.info>
    
    This file is part of JOBAD.
    
    JOBAD is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    JOBAD is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

//JOAD UI Folding Namespace
JOBAD.UI.Folding = {};

//Folding config
JOBAD.UI.Folding.config = {
    "placeHolderHeight": 50, //height of the place holder
    "placeHolderContent": "<p>Click to unfold me. </p>" //jquery ish stuff in the placeholder; ignored for livePreview mode. 
}

/*
    Enables folding on an element
    @param element      Element to enable folding on. 
    @param config       Configuration. 
        config.enable       Callback on enable. 
        config.autoFold     Fold on init? Default: false. 
        config.disable      Callback on disable
        config.fold         Callback on folding
        config.unfold       Callback on unfold
        config.stateChange  Callback on state change. 
        config.align        Alignment of the folding. Either 'left' (default) or 'right'.  
        config.update       Called every time the folding UI is updated. 
        config.height       Height fo the preview / replacement element. Leave empyt to assume default. 
        config.livePreview  Enable livePreview, shows a preview of the element instead of a replacement div. Default: false. 
        config.preview      String or function which describes the element(s). Will be used as preview text. Optional. 
*/

JOBAD.UI.Folding.enable = function(e, c){

    var results = JOBAD.refs.$(e).each(function(i, element){
        var element = JOBAD.refs.$(element);

        //check if we are already enabled
        if(element.data("JOBAD.UI.Folding.enabled")){
            JOBAD.console.log("Can't enable folding on element: Folding already enabled. ");
            return;
        }

        var config = JOBAD.util.clone(JOBAD.util.defined(c));
        var went_deeper = false; 

        if(typeof config == "undefined"){
            config = {};
        }

        //normalise config properties
        config.autoFold = (typeof config.autoFold == 'boolean')?config.autoFold:false;
        config.autoRender = JOBAD.util.forceBool(config.autoRender, true);
        config.enable = (typeof config.enable == 'function')?config.enable:function(){};
        config.disable = (typeof config.disable == 'function')?config.disable:function(){};
        config.fold = (typeof config.fold == 'function')?config.fold:function(){};
        config.unfold = (typeof config.unfold == 'function')?config.unfold:function(){};
        config.stateChange = (typeof config.stateChange == 'function')?config.stateChange:function(){};
        config.update = (typeof config.update == 'function')?config.update:function(){}
        config.align = (config.align == "right")?"right":"left";
        config.height = (typeof config.height == "number")?config.height:JOBAD.UI.Folding.config.placeHolderHeight;


        if(typeof config.preview == "undefined"){
            config.preview = function(){return JOBAD.UI.Folding.config.placeHolderContent;};
        } else {
            if(typeof config.preview !== "function"){
                //scope to keeo variables intatc
                (function(){
                    var old_preview = config.preview;
                    config.preview = function(){return old_preview; }
                })();
            }
        }

        config.livePreview = (typeof config.livePreview == "boolean")?config.livePreview:false;

        //Folding class
        var folding_class = "JOBAD_Folding_"+config.align;

        var wrapper = JOBAD.refs.$("<div class='JOBAD "+folding_class+" JOBAD_Folding_Wrapper'>");

        //spacing
        var spacer = JOBAD.refs.$("<div class='JOBAD "+folding_class+" JOBAD_Folding_Spacing'></div>").insertAfter(element);

        element.wrap(wrapper);
        wrapper = element.parent();

        //make the placeHolder
        var placeHolder = JOBAD.refs.$("<div class='JOBAD "+folding_class+" JOBAD_Folding_PlaceHolder'>")
        .prependTo(wrapper)
        .height(JOBAD.UI.Folding.config.placeHolderHeight)
        .hide().click(function(){
            JOBAD.UI.Folding.unfold(element);
        }); //prepend and hide me
        

        var container = JOBAD.refs.$("<div class='JOBAD "+folding_class+" JOBAD_Folding_Container'>")
        .prependTo(wrapper)
        .on("contextmenu", function(e){
            return (e.ctrlKey);
        });

        wrapper
        .data("JOBAD.UI.Folding.state", JOBAD.util.forceBool(element.data("JOBAD.UI.Folding.state"), config.autoFold))
        .data("JOBAD.UI.Folding.update", function(){
            JOBAD.UI.Folding.update(element);
        })
        .on("JOBAD.UI.Folding.fold", function(event){
            event.stopPropagation();
            if(wrapper.data("JOBAD.UI.Folding.state")){
                //we are already folded
                return; 
            }
            //fold me
            wrapper.data("JOBAD.UI.Folding.state", true);
            //trigger event
            config.fold(element);
            config.stateChange(element, true);
            JOBAD.UI.Folding.update(element);
        })
        .on("JOBAD.UI.Folding.unfold", function(event){
            event.stopPropagation();

            if(!wrapper.data("JOBAD.UI.Folding.state")){
                //we are already unfolded
                return; 
            }

            //unfold me
            wrapper.data("JOBAD.UI.Folding.state", false);
            //trigger event
            config.unfold(element);
            config.stateChange(element, false);
            JOBAD.UI.Folding.update(element);
        })
        .on("JOBAD.UI.Folding.update", config.livePreview?
        function(event){

            event.stopPropagation();

            element
            .unwrap()


            JOBAD.util.markHidden(element);

            container
            .css("height", "")

            if(wrapper.data("JOBAD.UI.Folding.state")){
                element
                .wrap(
                    JOBAD.refs.$("<div style='overflow: hidden; '>").css("height", config.height)
                );

                //we are folded
                container.removeClass("JOBAD_Folding_unfolded").addClass("JOBAD_Folding_folded");
            } else {
                element.wrap("<div style='overflow: hidden; '>");
                

                //mark me visible
                JOBAD.util.markDefault(element);

                //we are unfolded
                container.removeClass("JOBAD_Folding_folded").addClass("JOBAD_Folding_unfolded");
            }

            //reset height
            container
            .height(wrapper.height());

            config.update();
        }
        :
        function(event){
            //we dont have life preview; fallback to the old stuff. 
            //reset first

            element.parent().show()
            .end().show();

            //hide me. 
            JOBAD.util.markHidden(element);

            container
            .css("height", "")


            if(wrapper.data("JOBAD.UI.Folding.state")){
                //hide the element again
                element.parent().hide()
                .end().hide();

                //adjust placeholder height
                placeHolder.height(config.height)
                .show()

                JOBAD.util.markHidden(placeHolder); //hide it from JOBAD

                //append stuff to the placeholder
                placeHolder.empty().append(
                    config.preview(element)
                )

                //we are folded
                container.removeClass("JOBAD_Folding_unfolded").addClass("JOBAD_Folding_folded");
            } else {
                //we are going back to the normal state
                //hide the placeholder
                placeHolder.hide();

                //we are unfolded
                container.removeClass("JOBAD_Folding_folded").addClass("JOBAD_Folding_unfolded");

                //mark me visible
                JOBAD.util.markDefault(element);
            }

            container
            .height(wrapper.height());

            config.update();
        });

        container
        .add(spacer)
        .click(function(event){
            //fold or unfold goes here
            if(wrapper.data("JOBAD.UI.Folding.state")){
                JOBAD.UI.Folding.unfold(element);
            } else {
                JOBAD.UI.Folding.fold(element);
            }
        });

        element
        .wrap("<div style='overflow: hidden; '>")
        .data("JOBAD.UI.Folding.wrappers", container.add(placeHolder).add(spacer))
        .data("JOBAD.UI.Folding.enabled", true)
        .data("JOBAD.UI.Folding.callback", config.disable)
        .data("JOBAD.UI.Folding.onStateChange", config.update)
        .data("JOBAD.UI.Folding.config", config);

        element.click(function(ev){
                //we are folded
                JOBAD.UI.Folding.unfold();       
                ev.stopPropagation();
        });

        config.enable(element);
    }); 

    JOBAD.UI.Folding.update(e); //update everythign at the ened

    return results; 
}


/*
    Updates a folded element. 
    @param element  Element to update folding on. 
*/

JOBAD.UI.Folding.updating = false; 

JOBAD.UI.Folding.update = function(elements){
    if(JOBAD.UI.Folding.updating){
        return false;
    } else {
        var elements = JOBAD.refs.$(elements);

        if(elements.length > 0){
            var updaters = elements.parents().filter("div.JOBAD_Folding_Wrapper");
        } else {
            var updaters = JOBAD.refs.$("div.JOBAD_Folding_Wrapper");
        }
        JOBAD.UI.Folding.updating = true;
        JOBAD.util.orderTree(updaters).each(function(){
                //trigger each one individually. 
                JOBAD.refs.$(this).trigger("JOBAD.UI.Folding.update");
            });
        JOBAD.UI.Folding.updating = false; 
        JOBAD.refs.$(window).trigger("JOBAD.UI.Folding.update");
        return true; 
    }
    
}

/*
    Folds an element. 
    @param element  Element to update folding on. 
*/
JOBAD.UI.Folding.fold = function(element){
    var element = JOBAD.refs.$(element);
    if(!element.data("JOBAD.UI.Folding.enabled")){
        JOBAD.console.log("Can't fold element: Folding not enabled. ");
        return false;
    }
    element.parent().parent().trigger("JOBAD.UI.Folding.fold");
    return true;
}

/*
    Unfolds an element
*/
JOBAD.UI.Folding.unfold = function(el){
    JOBAD.refs.$(el)
    .each(function(){
        var element = JOBAD.refs.$(this);
        if(!element.data("JOBAD.UI.Folding.enabled")){
            JOBAD.console.log("Can't unfold element: Folding not enabled. ");
        }
        element.parent().parent().trigger("JOBAD.UI.Folding.unfold");
    });
    
    return true;
}

/*
    Disables folding on an element
    @param element Element to disable folding on. 
    @param keep Keeps elements hidden if set to true. 
*/
JOBAD.UI.Folding.disable = function(element, keep){
    var element = JOBAD.refs.$(element);

    if(element.length > 1){
        var me = arguments.callee;
        return element.each(function(i, e){
            me(e);
        });
    }

    if(element.data("JOBAD.UI.Folding.enabled") !== true){
        JOBAD.console.log("Can't disable folding on element: Folding already disabled. ");
        return;
    }

    //store the state of the current hiding. 
    element.data("JOBAD.UI.Folding.state", element.data("JOBAD.UI.Folding.wrappers").eq(0).parent().data("JOBAD.UI.Folding.state")?true:false);


    //do we keep it hidden?
    if(keep?false:true){
        JOBAD.UI.Folding.unfold(element);
        JOBAD.util.markDefault(element); 
        element.removeData("JOBAD.UI.Folding.config"); //reset to default. 
    }
    
    //call event handlers
    element.data("JOBAD.UI.Folding.callback")(element);
    element.data("JOBAD.UI.Folding.onStateChange")(element);

    //remove unneccesary elements. 
    element.data("JOBAD.UI.Folding.wrappers").remove();

    //remove any overlays. 
    JOBAD.UI.Overlay.undraw(element.parent());

    //clear up the last stuff
    element
    .unwrap()
    .unwrap()
    .removeData("JOBAD.UI.Folding.callback")
    .removeData("JOBAD.UI.Folding.onStateChange")
    .removeData("JOBAD.UI.Folding.enabled")
    .removeData("JOBAD.UI.Folding.wrappers");

    return element;
}
/*
    Checks if an element is foldable. 
    @param  element element to check. 
*/
JOBAD.UI.Folding.isFoldable = function(element){
    return JOBAD.util.closest(element, function(e){
        return e.data("JOBAD.UI.Folding.enabled");
    }).length > 0;
}
/*
    Shows this element if it is folded.
*/
JOBAD.UI.Folding.show = function(element){
    return JOBAD.refs.$(element).each(function(){
        var folded = JOBAD.refs.$(this).parents().add(JOBAD.refs.$(this)).filter(function(){
            var me = JOBAD.refs.$(this);
            return me.data("JOBAD.UI.Folding.enabled")?true:false;
        });

        if(folded.length > 0){
            JOBAD.UI.Folding.unfold(folded.get().reverse()); //unfold
         }
    });
}/* end   <ui/JOBAD.ui.folding.js> */
/* start <ui/JOBAD.ui.toolbar.js> */
/*
	JOBAD 3 UI Functions - Toolbar
	JOBAD.ui.toolbar.js
		
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

JOBAD.UI.Toolbar = {};

var Toolbars = JOBAD.refs.$(); 

JOBAD.UI.Toolbar.config = {
	"height": 20 //The default toolbar height
};

/*
	Adds a toolbar to the page and returns a reference to it. 
*/
JOBAD.UI.Toolbar.add = function(){
	var TB = JOBAD.refs.$("<div>").css({
		"width": "100%",
		"height": JOBAD.UI.Toolbar.config.height,
	}).appendTo("body");

	Toolbars = Toolbars.add(TB); 
	JOBAD.UI.Toolbar.update(); 
	JOBAD.util.markHidden(TB); 
	TB.on("contextmenu", function(e){
		return e.ctrlKey; 
	}); 
	return TB; 
}

/*
	Updates all toolbars at the bottom of the page. 
*/
JOBAD.UI.Toolbar.update = JOBAD.util.throttle(function(){
	var pos = 0; 
	Toolbars = Toolbars.filter(function(){
		var me = JOBAD.refs.$(this); 

		if(me.parent().length != 0){

			me.css({
				"position": "fixed",
				"right": 0, 
				"bottom": pos
			})

			pos += me.height(); 
			return true; 
		} else {
			return false; 
		}
	})
}, 300); 

JOBAD.UI.Toolbar.moveUp = function(TB){
	var x = Toolbars.index(TB); 
	if(x >= 0){
		Toolbars = JOBAD.refs.$(JOBAD.util.permuteArray(Toolbars, x, x+1));
	}
	JOBAD.UI.Toolbar.update(); 
}

JOBAD.UI.Toolbar.moveDown = function(TB){
	var x = Toolbars.index(TB); 
	if(x >= 0){
		Toolbars = JOBAD.refs.$(JOBAD.util.permuteArray(Toolbars, x, x-1));
	}
	JOBAD.UI.Toolbar.update(); 
}/* end   <ui/JOBAD.ui.toolbar.js> */
/* start <events/JOBAD.sidebar.js> */
/*
	JOBAD 3 Sidebar
	JOBAD.sidebar.js
		
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/


JOBAD.Sidebar = {};
JOBAD.Sidebar.styles = {};
JOBAD.Sidebar.types = [];
JOBAD.Sidebar.desc = ["Sidebar position"];


JOBAD.Sidebar.registerSidebarStyle = function(styleName, styleDesc, registerFunc, deregisterFunc, updateFunc){
	if(JOBAD.Sidebar.styles.hasOwnProperty(styleName)){
		JOBAD.console.log("Warning: Sidebar Style with name '"+styleName+"' already exists");
		return true;
	}
	
	JOBAD.Sidebar.types.push(styleName);
	JOBAD.Sidebar.desc.push(styleDesc);
	
	JOBAD.Sidebar.styles[styleName] = {
		"register": registerFunc, //JOBAD.UI.Sidebar.addNotification
		"deregister": deregisterFunc, //JOBAD.UI.Boundbar.removeItem(item);
		"update": updateFunc //JOBAD.UI.Sidebar.redraw
	};
};

JOBAD.Sidebar.registerSidebarStyle("right", "Right", 
	JOBAD.util.argWrap(JOBAD.UI.Sidebar.addNotification, function(args){
		args.push("right"); return args;
	}), 
	JOBAD.UI.Sidebar.removeNotification,
	function(){
		JOBAD.UI.Sidebar.forceNotificationUpdate();
		if(JOBAD.util.objectEquals(this.Sidebar.PastRequestCache, {})){
			JOBAD.UI.Sidebar.unwrap(this.element);
		}
	}
);

JOBAD.Sidebar.registerSidebarStyle("left", "Left", 
	JOBAD.util.argWrap(JOBAD.UI.Sidebar.addNotification, function(args){
		args.push("left"); return args;
	}), 
	JOBAD.UI.Sidebar.removeNotification,
	function(){
		JOBAD.UI.Sidebar.forceNotificationUpdate();
		if(JOBAD.util.objectEquals(this.Sidebar.PastRequestCache, {})){
			JOBAD.UI.Sidebar.unwrap(this.element);
		}
	}
);

JOBAD.Sidebar.registerSidebarStyle("none", "Hidden", 
	function(){return JOBAD.refs.$("<div>"); },
	function(){},
	function(){}
);


/* sidebar: SideBarUpdate Event */
JOBAD.events.SideBarUpdate = 
{
	'default': function(){
		//Does nothing
	},
	'Setup': {
		'init': {
			/* Sidebar namespace */
			'Sidebar': {
				'getSidebarImplementation': function(type){
					return JOBAD.util.bindEverything(JOBAD.Sidebar.styles[type], this);
				},
				'forceInit': function(){
					if(typeof this.Sidebar.ElementRequestCache == 'undefined'){
						this.Sidebar.ElementRequestCache = [];
					}
					
					if(typeof this.Sidebar.Elements == 'undefined'){
						this.Sidebar.Elements = {};
					}
					
					if(typeof this.Sidebar.PastRequestCache == 'undefined'){
						this.Sidebar.PastRequestCache = {};
					}
					
					
					if(!this.Event.SideBarUpdate.enabled){
						return;
					}
				
					var new_type = this.Config.get("sidebar_type");
			
					if(this.Event.SideBarUpdate.type != new_type){
						this.Sidebar.transit(new_type); //update sidebar type
					}
				},
				'makeCache': function(){
					var cache = [];
					
					for(var key in this.Sidebar.PastRequestCache){
						cache.push(this.Sidebar.PastRequestCache[key]);
					}
					
					return cache;
				},
				/*
					Post-Config-Switching
				*/
				'transit': function(to){
					var cache = this.Sidebar.makeCache(); //cache the loaded requests
					
					//remove all old requests
					for(var key in this.Sidebar.Elements){
						this.Sidebar.removeNotificationT(this.Event.SideBarUpdate.type, this.Sidebar.Elements[key], false);
					}
					 	
					this.Sidebar.redrawT(this.Event.SideBarUpdate.type); //update it one mroe time (this also clears the toolbar)
					
					
					this.Event.SideBarUpdate.type = to;
					
					
					for(var i=0;i<cache.length;i++){
						this.Sidebar.registerNotification(cache[i][0], cache[i][1], false);
					}
					
					this.Sidebar.redrawT(to); //redraw the sidebar
					
				},				
				/*
					Redraws the sidebar. 
				*/
				'redraw': function(){				
					if(typeof this.Event.SideBarUpdate.enabled == "undefined"){
						return; //do not run if disabled
					}
				
					this.Sidebar.forceInit();
					
					return this.Sidebar.redrawT(this.Event.SideBarUpdate.type);
				},
				/*
					Redraws the sidebar assuming the specefied type. 
				*/
				'redrawT': function(type){
					this.Sidebar.redrawing = true;

					var root = this.element;

					var implementation = this.Sidebar.getSidebarImplementation(type);
					
					for(var i=0;i<this.Sidebar.ElementRequestCache.length;i++){
						var id = this.Sidebar.ElementRequestCache[i][0];
						var element = this.Sidebar.ElementRequestCache[i][1];
						var config = this.Sidebar.ElementRequestCache[i][2];
						
						this.Sidebar.Elements[id] = implementation["register"](this.element, element, config)
						.data("JOBAD.Events.Sidebar.id", id);
						
						this.Sidebar.PastRequestCache[id] = [element, config]; 
						
					}
												
					this.Sidebar.ElementRequestCache = []; //clear the cache
					
					//update and trigger event
					implementation["update"]();
					this.Event.SideBarUpdate.trigger();

					this.Sidebar.redrawing = false;
				},
				
				/*
					Registers a new notification. 
					@param element Element to register notification on. 
					@param config
							config.class:	Notificaton class. Default: none. 
							config.icon:	Icon (Default: Based on notification class. )
							config.text:	Text
							config.menu:	Context Menu
							config.trace:	Trace the original element on hover? (Ignored for direct)
							config.click:	Callback on click, Default: Open Context Menu
					@param autoRedraw Optional. Should the sidebar be redrawn? (default: true)
					@return jQuery element used as identification. 
							
				*/
				'registerNotification': function(element, config, autoRedraw){
				
					this.Sidebar.forceInit(); //force an init
				
					var id = JOBAD.util.UID(); //generate new UID
					
					var config = (typeof config == 'undefined')?{}:config;
					config.menuThis = this;
					
					this.Sidebar.ElementRequestCache.push([id, JOBAD.refs.$(element), JOBAD.util.clone(config)]); //cache the request. 
					
					if((typeof autoRedraw == 'boolean')?autoRedraw:true){
						this.Sidebar.redraw();
						return this.Sidebar.Elements[id];
					}		
					
				},
				/*
					removes a notification. 
					@param	item	Notification to remove.
					@param autoRedraw Optional. Should the sidebar be redrawn? (default: true)
					
				*/
				'removeNotification': function(item, autoRedraw){
					this.Sidebar.forceInit();
					if(!this.Event.SideBarUpdate.enabled){
						//we're disabled; just remove it from the cache
						var id = item.data("JOBAD.Events.Sidebar.id");
						
						for(var i=0;i<this.Sidebar.PastRequestCache.length;i++){
							if(this.Sidebar.PastRequestCache[i][0] == id){
								this.Sidebar.PastRequestCache.splice(i, 1); //remove the element
								break;
							}
						}
					} else {
						return this.Sidebar.removeNotificationT(this.Event.SideBarUpdate.type, item, autoRedraw);
					}
				},
				/*
					removes a notification assuming the specefied sidebar type. 
					@param	item	Notification to remove.
					@param autoRedraw Optional. Should the sidebar be redrawn? (default: true)
					
				*/
				'removeNotificationT': function(type, item, autoRedraw){
					var implementation = this.Sidebar.getSidebarImplementation(type);
					
					if(item instanceof JOBAD.refs.$){
						var id = item.data("JOBAD.Events.Sidebar.id");
	
						implementation["deregister"](item);
						
						delete this.Sidebar.Elements[id];
						delete this.Sidebar.PastRequestCache[id];
						
						if((typeof autoRedraw == 'boolean')?autoRedraw:true){
							this.Sidebar.redrawT(type); //redraw
						}
						return id;
					} else {
						JOBAD.error("JOBAD Sidebar Error: Tried to remove invalid Element. ");
					}
				}
			}
		},
		'enable': function(root){
			var me = this; 

			this.Event.SideBarUpdate.enabled = true;
			this.Event.SideBarUpdate.type = this.Config.get("sidebar_type"); //init the type
			this.Sidebar.redraw(); //redraw the sidebar
		},
		'disable': function(root){
		
			var newCache = []
		
			//remove all old requests
			for(var key in this.Sidebar.Elements){
				newCache.push([key, this.Sidebar.PastRequestCache[key][0], this.Sidebar.PastRequestCache[key][1]]);
				
				this.Sidebar.removeNotification(this.Sidebar.Elements[key], false);
			}
			
			this.Sidebar.redraw(); //redraw it one more time. 
			
			this.Sidebar.ElementRequestCache = newCache; //everything is now hidden
			
			this.Event.SideBarUpdate.enabled = undefined; 
		}
	},
	'namespace': 
	{
		
		'getResult': function(){
			if(this.Event.SideBarUpdate.enabled){
				this.modules.iterateAnd(function(module){
					module.SideBarUpdate.call(module, module.getJOBAD());
					return true;
				});
			}
		},
		'trigger': function(){
			preEvent(this, "SideBarUpdate", []);
			this.Event.SideBarUpdate.getResult();
			postEvent(this, "SideBarUpdate", []);
		}
	}
};/* end   <events/JOBAD.sidebar.js> */
/* start <events/JOBAD.folding.js> */
/*
	JOBAD 3 Folding
	JOBAD.folding.js
		
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

JOBAD.ifaces.push(function(){
	var me = this;

	//Folding namespace. 
	this.Folding = {};

	/*
		Enables Folding on an element. 
		@param element	Element to enable folding on.  
		@param config	Configuration for folding. See JOBAD.UI.Folding.enable. 
	*/
	this.Folding.enable = function(element, config){

		if(typeof element == "undefined"){
			var element = this.element; 
		}

		var element = JOBAD.refs.$(element); //TODO: Add support for folding the entire thing. 

		if(!this.contains(element)){
			JOABD.console.warn("Can't enable folding on element: Not a member of this JOBADInstance")
			return;
		}

		this.Event.trigger("folding.enable", [element]); 

		if(element.is(this.element)){
			if(element.data("JOBAD.UI.Folding.virtualFolding")){
				element = element.data("JOBAD.UI.Folding.virtualFolding");
			} else {
				this.element.wrapInner(JOBAD.refs.$("<div></div>"));
				element = this.element.children().get(0);
				this.element.data("JOBAD.UI.Folding.virtualFolding", JOBAD.refs.$(element));
			}
			
		}

		return JOBAD.UI.Folding.enable(
			element, 
			JOBAD.util.extend(JOBAD.util.defined(config), 
				{
					"update": 
						function(){
							if(!me.Sidebar.redrawing){
								me.Sidebar.redraw(); //redraw the sidebar. 
							}
						}
				}
			)
		);
	};

	/*
		Disables folding on an element. 
		@param	element	Element to disable folding on. 
	*/
	this.Folding.disable = function(element){
		if(typeof element == "undefined"){
			var element = this.element; 
		}
		var element = JOBAD.refs.$(element);

		this.Event.trigger("folding.disable", [element]); 

		if(element.data("JOBAD.UI.Folding.virtualFolding")){
			var vElement = element.data("JOBAD.UI.Folding.virtualFolding");
			JOBAD.UI.Folding.disable(vElement);
			vElement.replaceWith(vElement.children()); //replace the element completely with the children
			return element.removeData("JOBAD.UI.Folding.virtualFolding"); 
		} else {
			return JOBAD.UI.Folding.disable(element);
		}
	};

	/*
		Checks if folding is enabled on an element. 
	*/
	this.Folding.isEnabled = function(element){
		if(typeof element == "undefined"){
			var element = this.element; 
		}
		var element = JOBAD.refs.$(element);

		if(element.data("JOBAD.UI.Folding.virtualFolding")){
			return element.data("JOBAD.UI.Folding.virtualFolding").data("JOBAD.UI.Folding.enabled")?true:false;
		} else {
			return element.data("JOBAD.UI.Folding.enabled")?true:false;
		}
	};

	this.Folding = JOBAD.util.bindEverything(this.Folding, this);
});/* end   <events/JOBAD.folding.js> */
/* start <events/JOBAD.toolbar.js> */
/*
	JOBAD 3 Toolbar
	JOBAD.toolbar.js
		
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/


JOBAD.events.Toolbar = 
{
	'default': function(JobadInstance, Toolbar){
		return false; 
	},
	'Setup': {
		'enable': function(){
			this.modules.iterateAnd(function(module){
				module.Toolbar.enable(); 
				return true;
			});
		},
		'disable': function(){
			//Remove all toolbars
			this.modules.iterateAnd(function(module){
				module.Toolbar.disable(); 
				return true;
			});
		}
	},
	'namespace': {
		
		'getResult': function(){
			var me = this; 
			this.modules.iterateAnd(function(module){
				module.Toolbar.show(); 
				return true;
			});
		},
		'trigger': function(){
			preEvent(this, "Toolbar", []);
			this.Event.Toolbar.getResult();
			postEvent(this, "Toolbar", []);
		}
	}
};

JOBAD.modules.ifaces.push([
	function(properObject, ModuleObject){
		return properObject; 
	}, function(){
		var me = this; 
		var id = me.info().identifier; 

		var TBElement = undefined; 
		var JOBADInstance = me.getJOBAD(); 

		var enabled = false; 


		this.Toolbar.get = function(){
			//gets the toolbar if available, otherwise undefined
			return TBElement;
		};

		this.Toolbar.isEnabled = function(){
			return enabled; 
		}

		this.Toolbar.enable = function(){
			//enable the toolbar

			if(me.Toolbar.isEnabled()){
				//Remove any old toolbars
				me.Toolbar.disable();
			}

			//create a new Toolbar
			var TB = JOBAD.UI.Toolbar.add(); 


			//check if we need it
			var res = me.Toolbar.call(me, me.getJOBAD(), TB);

			if(res !== true){
				//we do not want it => remove it. 
				TB.remove(); 
				JOBAD.UI.Toolbar.update(); 

				return false;  
			} else {
				//we need it. 
				TBElement = TB; 
				JOBADInstance.Event.trigger("Toolbar.add", id);

				enabled = true; //it is now enabled

				return true; 
			}

			
		}

		this.Toolbar.disable = function(){
			//disable the toolbar

			if(!me.Toolbar.isEnabled()){
				//we are already disabled
				return false; 
			}

			//remove the toolbar element

			TBElement.remove(); 
			JOBAD.UI.Toolbar.update();
			TBElement = undefined;  

			JOBADInstance.Event.trigger("Toolbar.remove", id);

			enabled = false; //it is now disabled
			return true; 
		}

		//Hide / Show the Toolbar

		var visible = JOBADInstance.Config.get("auto_show_toolbar"); //get the default

		this.Toolbar.setVisible = function(){
			visible = true; 
			me.Toolbar.show(); 
		}

		this.Toolbar.setHidden = function(){
			visible = false; 
			me.Toolbar.show(); 
		}

		this.Toolbar.isVisible = function(){
			return visible; 
		}

		this.Toolbar.show = function(){
			if(me.Toolbar.isVisible() && me.isActive() && JOBADInstance.Instance.isFocused()){
				me.Toolbar.enable(); 
				return true; 
			} else {
				me.Toolbar.disable(); 
				return false; 
			}
		}

		this.Toolbar.moveUp = function(){
			return JOBAD.UI.Toolbar.moveUp(TBElement); 
		}

		this.Toolbar.moveDown = function(){
			return JOBAD.UI.Toolbar.moveDown(TBElement); 
		}


		//Register Event Handlers for activation + focus

		JOBADInstance.Event.on("module.activate", function(m){
			if(m.info().identifier == id){
				me.Toolbar.show(); 
			}
		});

		JOBADInstance.Event.on("instance.focus", function(m){
			me.Toolbar.show(); 
		});

		JOBADInstance.Event.on("instance.unfocus", function(m){
			me.Toolbar.disable(); 
		});
	}]); /* end   <events/JOBAD.toolbar.js> */
/* start <events/JOBAD.events.js> */
/*
	JOBAD 3 Events
	JOBAD.events.js
		
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

/* left click */
JOBAD.events.leftClick = 
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
						preEvent(me, "leftClick", [element]); 
						me.Event.leftClick.trigger(element);
						postEvent(me, "leftClick", [element]); 
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
			if(JOBAD.util.isHidden(target)){
				return true;
			}
			var evt = this.Event.leftClick.getResult(target);
			return evt;
		}
	}
};

/* keypress */
JOBAD.events.keyPress = 
{
	'default': function(key, JOBADInstance){
		return false;
	},
	'Setup': {
		'enable': function(root){
			var me = this;
			me.Event.keyPress.__handlerName = JOBAD.util.onKey(function(k){
				if(me.Instance.isFocused()){
					preEvent(me, "keyPress", [k]); 
					var res = me.Event.keyPress.trigger(k); 
					postEvent(me, "keyPress", [k]); 
					return res; 
				} else {
					return true; 
				}
			}); 
		},
		'disable': function(root){
			JOBAD.refs.$(document).off(this.Event.keyPress.__handlerName); 
		}
	},
	'namespace': 
	{
		
		'getResult': function(key){
			var res = this.modules.iterateAnd(function(module){
				return !module.keyPress.call(module, key, module.getJOBAD());
			});
 
			return res; 
		},
		'trigger': function(key){
			var evt = this.Event.keyPress.getResult(key);
			return evt;
		}
	}
};

/* double Click */
JOBAD.events.dblClick = 
{
	'default': function(){
		return false;
	},
	'Setup': {
		'enable': function(root){
			var me = this;
			root.delegate("*", 'dblclick.JOBAD.dblClick', function(event){
				var element = JOBAD.refs.$(event.target); //The base element.  
				preEvent(me, "dblClick", [element]); 
				var res = me.Event.dblClick.trigger(element);
				postEvent(me, "dblClick", [element]); 
				event.stopPropagation(); //Not for the parent. 
			});
		},
		'disable': function(root){
			root.undelegate("*", 'dblclick.JOBAD.dblClick');	
		}
	},
	'namespace': 
	{
		
		'getResult': function(target){
			return this.modules.iterateAnd(function(module){
				module.dblClick.call(module, target, module.getJOBAD());
				return true;
			});
		},
		'trigger': function(target){
			if(JOBAD.util.isHidden(target)){
				return true;
			}
			var evt = this.Event.dblClick.getResult(target);
			return evt;
		}
	}
};

/* onEvent */
JOBAD.events.onEvent = 
{
	'default': function(){},
	'Setup': {
		'enable': function(root){
			var me = this;

			me.Event.onEvent.id = 
			me.Event.on("event.handlable", function(event, args){
				me.Event.onEvent.trigger(event, args);
			});
		},
		'disable': function(root){
			var me = this;
			me.Event.off(me.Event.onEvent.id);
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
			if(JOBAD.util.isHidden(element)){
				return true;
			}
			return this.Event.onEvent.getResult(event, element);
		}
	}
};

/* context menu entries */
JOBAD.events.contextMenuEntries = 
{
	'default': function(){
		return [];
	},
	'Setup': {
		'enable': function(root){
			var me = this;
			JOBAD.UI.ContextMenu.enable(root, function(target){
				preEvent(me, "contextMenuEntries", [target]);
				var res = me.Event.contextMenuEntries.getResult(target);
				postEvent(me, "contextMenuEntries", [target]);
				return res;
			}, {
				"type": function(target){
					return me.Config.get("cmenu_type");
				}, 
				"show": function(){
					me.Event.trigger("contextmenu.open", []); 
					me.Event.handle("contextMenuOpen");
				},
				"close": function(){
					me.Event.trigger("contextmenu.close", []); 
					me.Event.handle("contextMenuClose");
				},
				"stopPropagnate": true
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
				var mtarget = target;
				var res = []; 
				while(true){
					if(mtarget.length == 0 
						|| res.length > 0){
						
						return res; 
					}
					res = module.contextMenuEntries.call(module, mtarget, module.getJOBAD());
					res = JOBAD.UI.ContextMenu.generateMenuList(res); 
					mtarget = mtarget.parent(); 
				}
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

/* configUpdate */
JOBAD.events.configUpdate = 
{
	'default': function(setting, JOBADInstance){},
	'Setup': {
		'enable': function(root){
			var me = this;
			JOBAD.refs.$("body").on('JOBAD.ConfigUpdateEvent', function(jqe, setting, moduleId){
				preEvent(me, "configUpdate", [setting, moduleId]);
				me.Event.configUpdate.trigger(setting, moduleId);
				postEvent(me, "configUpdate", [setting, moduleId]);
			});
		},
		'disable': function(root){
			JOBAD.refs.$("body").off('JOBAD.ConfigUpdateEvent');
		}
	},
	'namespace': 
	{
		
		'getResult': function(setting, moduleId){
			return this.modules.iterateAnd(function(module){
				if(module.info().identifier == moduleId){ //only call events for own module. 
					module.configUpdate.call(module, setting, module.getJOBAD());
				}
				return true;
			});
		},
		'trigger': function(setting, moduleId){
			return this.Event.configUpdate.getResult(setting, moduleId);
		}
	}
};

/* hover Text */
JOBAD.events.hoverText = 
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
			if(JOBAD.util.isHidden(target)){
				return true;
			}
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

			preEvent(this, "hoverText", [source]);
			var EventResult = this.Event.hoverText.getResult(source); //try to do the event
		
			if(typeof EventResult == 'boolean'){
				postEvent(this, "hoverText", [source]);
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

			postEvent(this, "hoverText", [source]);

			if(!source.is(this.element)){
				this.Event.hoverText.trigger(source.parent());//we are in the parent now
				return false;
			}
		}
	}
}/* end   <events/JOBAD.events.js> */
/* start <JOBAD.config.js> */
/*
	JOBAD Configuration
	JOBAD.config.js
	
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/


//StorageBackend

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
	"none": [function(key){}, function(key, value){}],
	"cookie": [function(key){
		var cookies = document.cookie;
		
		var startIndex = cookies.indexOf(" "+key+"=");
		startIndex = (startIndex == -1)?cookies.indexOf(key+"="):startIndex;

		if(startIndex == -1){
			//we can't find it
			return false;
		}

		//we sart after start Index
		startIndex = cookies.indexOf("=", startIndex)+1; 

		var endIndex = cookies.indexOf(";", startIndex);
		endIndex = (endIndex == -1)?cookies.length:endIndex; //till the end

		return unescape(cookies.substring(startIndex, endIndex));
	}, function(key, value){
		//sets a cookie
		var expires=new Date();
		expires.setDate(expires.getDate()+7); //store settings for 7 days
		document.cookie = escape(key) + "=" + escape(value) + "; expires="+expires.toUTCString()+"";
	}]
};

JOBAD.config.storageBackend = "cookie";



//Config Settings - module based
/*
	Validates if specefied object of a configuration object can be set. 
	@param	obj Configuration Object
	@param	key	Key to validate. 
	@param	val	Value to validate. 
	@returns boolean
*/
JOBAD.modules.validateConfigSetting = function(obj, key, val){
	if(!obj.hasOwnProperty(key)){
		JOBAD.console.warn("Undefined user setting: "+key);
		return false;
	}
	var type = obj[key][0];
	var validator = obj[key][1];
	switch(type){
		case "string":
			return (validator(val) && typeof val == "string");
			break;
		case "bool":
			return (typeof val == "boolean");
			break;
		case "integer":
			return (typeof val == "number" && val % 1 == 0 && validator(val));
			break;
		case "number":
			return (typeof val == "number" && validator(val));
			break;
		case "list":
			return validator(val);
			break;
		case "none":
			return true;
			break;
	}
};

/*
	Creates a proper User Settings Object
	@param	obj Configuration Object
	@param modName	Name of the module
	@returns object
*/
JOBAD.modules.createProperUserSettingsObject = function(obj, modName){

	var newObj = {};
	for(var key in obj){
		
		var WRONG_FORMAT_MSG = "Ignoring UserConfig '"+key+"' in module '"+modName+"': Wrong description format";
		
		if(obj.hasOwnProperty(key)){
			(function(){
			var spec = obj[key];
			var newSpec = [];
			
			if(!JOBAD.util.isArray(spec)){
				JOBAD.console.warn(WRONG_FORMAT_MSG+" (Array required). ");
				return;
			}
			
			if(spec.length == 0){
				JOBAD.console.warn(WRONG_FORMAT_MSG+" (Array may not have length zero). ");
				return;
			}
			
			if(typeof spec[0] == 'string'){
				var type = spec[0];
			} else {
				JOBAD.console.warn(WRONG_FORMAT_MSG+" (type must be a string). ");
				return; 
			}
			if(type == "none"){
				if(spec.length == 2){
					var def = spec[1];
				} else {
					JOBAD.console.warn(WRONG_FORMAT_MSG+" (Array length 2 required for 'none'. ");
					return;
				}
			} else if(spec.length == 4){
				var validator = spec[1];
				var def = spec[2];
				var meta = spec[3];
			} else if(spec.length == 3) {
				var validator = function(){return true;};
				var def = spec[1];
				var meta = spec[2];
			} else {
				JOBAD.console.warn(WRONG_FORMAT_MSG+" (Array length 3 or 4 required). ");
				return; 
			}
			
			switch(type){
				case "string":
					newSpec.push("string");
					
					//validator
					if(JOBAD.util.isRegExp(validator)){
						newSpec.push(function(val){return validator.test(val)});
					} else if(typeof validator == 'function') {
						newSpec.push(validator);
					} else {
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Unknown restriction type for 'string'. ). ");
						return;
					}
					
					//default
					try{
						if(newSpec[newSpec.length-1](def) && typeof def == 'string'){
							newSpec.push(def);
						} else {
							JOBAD.console.warn(WRONG_FORMAT_MSG+" (Validation function failed for default value. ). ");
							return;
						}
					} catch(e){
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Validation function caused exception for default value. ). ");
						return;
					}
					
					//meta
					if(typeof meta == 'string'){
						newSpec.push([meta, ""]);
					} else if(JOBAD.util.isArray(meta)) {
						if(meta.length == 1){
							meta.push("");
						}
						if(meta.length == 2){
							newSpec.push(meta);
						} else {
							JOBAD.console.warn(WRONG_FORMAT_MSG+" (Meta data not allowed for length > 2). ");
							return;
						}
					} else {
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Meta data needs to be a string or an array). ");
						return;
					}
					
					break;
				case "bool":
					newSpec.push("bool");
					
					//Validator
					if(spec.length == 4){
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Type 'boolean' may not have restrictions. )");
						return;
					}					
					newSpec.push(validator);
					
					//default
					try{
						if(newSpec[newSpec.length-1](def) && typeof def == 'boolean'){
							newSpec.push(def);
						} else {
							JOBAD.console.warn(WRONG_FORMAT_MSG+" (Validation function failed for default value. ). ");
							return;
						}
					} catch(e){
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Validation function caused exception for default value. ). ");
						return;
					}
					
					//meta
					if(typeof meta == 'string'){
						newSpec.push([meta, ""]);
					} else if(JOBAD.util.isArray(meta)) {
						if(meta.length == 1){
							meta.push("");
						}
						if(meta.length == 2){
							newSpec.push(meta);
						} else {
							JOBAD.console.warn(WRONG_FORMAT_MSG+" (Meta data not allowed for length > 2). ");
							return;
						}
					} else {
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Meta data needs to be a string or an array). ");
						return;
					}
					
					break;
				case "integer":
					newSpec.push("integer");
					
					//validator
					if(JOBAD.util.isArray(validator)){
						if(validator.length == 2){
							newSpec.push(function(val){return (val >= validator[0])&&(val <= validator[1]);});
						} else {
							JOBAD.console.warn(WRONG_FORMAT_MSG+" (Restriction Array must be of length 2). ");
						}
					} else if(typeof validator == 'function') {
						newSpec.push(validator);
					} else {
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Unknown restriction type for 'integer'. ). ");
						return;
					}
					
					//default
					try{
						if(newSpec[newSpec.length-1](def) && (def % 1 == 0)){
							newSpec.push(def);
						} else {
							JOBAD.console.warn(WRONG_FORMAT_MSG+" (Validation function failed for default value. ). ");
							return;
						}
					} catch(e){
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Validation function caused exception for default value. ). ");
						return;
					}
					
					//meta
					if(typeof meta == 'string'){
						newSpec.push([meta, ""]);
					} else if(JOBAD.util.isArray(meta)) {
						if(meta.length == 1){
							meta.push("");
						}
						if(meta.length == 2){
							newSpec.push(meta);
						} else {
							JOBAD.console.warn(WRONG_FORMAT_MSG+" (Meta data not allowed for length > 2). ");
							return;
						}
					} else {
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Meta data needs to be a string or an array). ");
						return;
					}
					
					break;
				case "number":
					newSpec.push("number");
					
					//validator
					if(JOBAD.util.isArray(validator)){
						if(validator.length == 2){
							newSpec.push(function(val){return (val >= validator[0])&&(val <= validator[1]);});
						} else {
							JOBAD.console.warn(WRONG_FORMAT_MSG+" (Restriction Array must be of length 2). ");
						}
					} else if(typeof validator == 'function') {
						newSpec.push(validator);
					} else {
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Unknown restriction type for 'number'. ). ");
						return; 
					}
					
					//default
					try{
						if(newSpec[newSpec.length-1](def) && typeof def == 'number'){
							newSpec.push(def);
						} else {
							JOBAD.console.warn(WRONG_FORMAT_MSG+" (Validation function failed for default value. ). ");
							return;
						}
					} catch(e){
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Validation function caused exception for default value. ). ");
						return;
					}
					
					//meta
					if(typeof meta == 'string'){
						newSpec.push([meta, ""]);
					} else if(JOBAD.util.isArray(meta)) {
						if(meta.length == 1){
							meta.push("");
						}
						if(meta.length == 2){
							newSpec.push(meta);
						} else {
							JOBAD.console.warn(WRONG_FORMAT_MSG+" (Meta data not allowed for length > 2). ");
							return;
						}
					} else {
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Meta data needs to be a string or an array). ");
						return;
					}
					
					break;
				case "list":
					newSpec.push("list");
					
					
					//validator
					if(JOBAD.util.isArray(validator) && spec.length == 4){
							if(validator.length == 0){
								JOBAD.console.warn(WRONG_FORMAT_MSG+" (Array restriction must be non-empty). ");
								return;
							}
							newSpec.push(function(val){return (JOBAD.util.indexOf(validator, val) != -1);});
					} else {
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Type 'list' needs array restriction. ). ");
						return;
					}
					
					//default
					try{
						if(newSpec[newSpec.length-1](def)){
							newSpec.push(def);
						} else {
							JOBAD.console.warn(WRONG_FORMAT_MSG+" (Validation function failed for default value. ). ");
							return;
						}
					} catch(e){
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Validation function caused exception for default value. ). ");
						return;
					}
					
					//meta
					if(JOBAD.util.isArray(meta)) {
						if(meta.length == validator.length+1){
							newSpec.push(meta);
						} else {
							JOBAD.console.warn(WRONG_FORMAT_MSG+" (Meta data has wrong length). ");
							return;
						}
					} else {
						JOBAD.console.warn(WRONG_FORMAT_MSG+" (Meta data for type 'list' an array). ");
						return;
					}
					
					//construction-data
					newSpec.push(validator); 
					
					break;
				case "none": 
					newSpec.push("none");
					newSpec.push(def);
					break;
				default:
					JOBAD.console.warn(WRONG_FORMAT_MSG+" (Unknown type '"+type+"'. ). ");
					return;
					break;
			}
			newObj[key] = newSpec;
			})();
		}
	}
	return newObj;
};

/*
	Gets the default of a configuration object
	@param	obj Configuration Object
	@param	key	Key to get. 
	@returns object
*/
JOBAD.modules.getDefaultConfigSetting = function(obj, key){
	if(!obj.hasOwnProperty(key)){
		JOBAD.console.warn("Undefined user setting: "+key);
		return;
	}
	return obj[key][2];
};

//Config Settings - module
var configCache = {}; //cache of set values
var configSpec = {};

var userConfigMessages = {}; //messages for userconfig


JOBAD.UserConfig = {};

/*
	Sets a user configuration. 
	@param id   Id of module to use UserConfig from. 
	@param prop	Property to set
	@param val	Value to set. 
*/
JOBAD.UserConfig.set = function(id, prop, val){
	var value = JOBAD.UserConfig.getTypes(id);

	if(JOBAD.util.isObject(prop)){
		var keys = JOBAD.util.keys(prop);
		return JOBAD.util.map(keys, function(key){
			return JOBAD.UserConfig.set(id, key, prop[key]);
		});
	}

	if(JOBAD.UserConfig.canSet(id, prop, val)){
		if(JOBAD.util.objectEquals(val, JOBAD.UserConfig.get(id, prop))){
			return; //we have it already; no change neccessary. 
		}
		configCache[id][prop] = val;
		
	} else {
		JOBAD.console.warn("Can not set user config '"+prop+"' of module '"+id+"': Validation failure. ");
		return; 
	}
	JOBAD.storageBackend.setKey(id, configCache[id]);
	JOBAD.refs.$("body").trigger("JOBAD.ConfigUpdateEvent", [prop, id]);

	return prop;
};

/*
	Checks if a user configuration can be set. 
	@param id   Id of module to use UserConfig from. 
	@param prop	Property to set
	@param val	Value to set. 
*/
JOBAD.UserConfig.canSet = function(id, prop, val){
	var value = JOBAD.UserConfig.getTypes(id);
	if(JOBAD.util.isObject(prop)){
		var keys = JOBAD.util.keys(prop);
		return JOBAD.util.lAnd(JOBAD.util.map(keys, function(key){
			return JOBAD.modules.validateConfigSetting(value, key, prop[key]);
		}));
	} else {
		return JOBAD.modules.validateConfigSetting(value, prop, val);
	}
};

/*
	Retrieves a user configuration setting. 
	@param id   Id of module to use UserConfig from. 
	@param prop	Property to get
	@param val	Value to get. 
*/
JOBAD.UserConfig.get = function(id, prop){
	var value = JOBAD.UserConfig.getTypes(id);

	if(JOBAD.util.isObject(prop) && !JOBAD.util.isArray(prop)){
		var prop = JOBAD.util.keys(prop);
	}
	if(JOBAD.util.isArray(prop)){
		var res = {};

		JOBAD.util.map(prop, function(key){
			res[key] = JOBAD.UserConfig.get(id, key);
		});

		return res;
	}

	var res = configCache[id][prop];
	if(JOBAD.modules.validateConfigSetting(value, prop, res)){
		return res;
	} else {
		JOBAD.console.log("Failed to access user setting '"+prop+"'");
		return;
	}
};

/*
	Gets the user configuration types. 
	@param id   Id of module to use UserConfig from. 
*/
JOBAD.UserConfig.getTypes = function(id){
	if(!configSpec.hasOwnProperty(id)){
		JOBAD.error("Can't access UserConfig for module '"+id+"': Module not found (is it registered yet? )");
		return; 
	}
	return configSpec[id];
}

/*
	Resets the user configuration. 
	@param id   Id of module to use UserConfig from. 
	@param prop Optional. Property to reset. 
*/
JOBAD.UserConfig.reset = function(id, prop){
	var value = JOBAD.UserConfig.getTypes(id);
	configCache[id] = JOBAD.storageBackend.getKey(id);
	if(typeof configCache[id] == "undefined"){
		configCache[id] = {};
		for(var key in value){
			configCache[id][key] = JOBAD.modules.getDefaultConfigSetting(value, key);
			JOBAD.refs.$("body").trigger("JOBAD.ConfigUpdateEvent", [key, id]);
		}
	}
};


/*
	Gets the current message set by the module. 
	@param	id	Id of module to use. 
*/
JOBAD.UserConfig.getMessage = function(id){
	if(!configSpec.hasOwnProperty(id)){
		JOBAD.error("Can't access UserConfig for module '"+id+"': Module not found (is it registered yet? )");
		return; 
	}

	var msg = userConfigMessages[id];

	return (typeof msg == "undefined"?"":msg); 
}

/*
	Sets the current message set by the module. 
	@param	id	Id of module to use. 
	@param	msg	New message
*/
JOBAD.UserConfig.setMessage = function(id, msg){
	if(!configSpec.hasOwnProperty(id)){
		JOBAD.error("Can't access UserConfig for module '"+id+"': Module not found (is it registered yet? )");
		return; 
	}
	userConfigMessages[id] = msg;
	return msg;
}

/*
	Gets a UserConfig Object for the sepecefied module id. 
	@param id	Identifier of module to get UserConfig for. 
*/
JOBAD.UserConfig.getFor = function(id){
	if(!configSpec.hasOwnProperty(id)){
		JOBAD.error("Can't access UserConfig for module '"+id+"': Module not found (is it registered yet? )");
		return; 
	}

	return {
		"set": function(prop, val){
			return JOBAD.UserConfig.set(id, prop, val);
		},
		"setMessage": function(msg){
			return JOBAD.UserConfig.setMessage(id, msg);
		},
		"get": function(prop){
			return JOBAD.UserConfig.get(id, prop);
		},
		"getMessage": function(){
			return JOBAD.UserConfig.getMessage(id);
		},
		"canSet": function(prop, val){
			return JOBAD.UserConfig.canSet(id, prop, val);
		},
		"getTypes": function(){
			return JOBAD.UserConfig.getTypes(id);
		},
		"reset": function(prop){
			return JOBAD.UserConfig.reset(id, prop);
		}
	};
};



JOBAD.modules.extensions.config = {
	"required": false, //not required
	
	"validate": function(prop){return true; }, 
	
	"init": function(available, value, originalObject, properObject){
		return JOBAD.modules.createProperUserSettingsObject(available ? value : {}, properObject.info.identifier);
	},
	"globalProperties": ["UserConfig", "Config"],
	"onRegister": function(value, properObject, moduleObject){
		var id = properObject.info.identifier;

		configSpec[id] = value; //store the specification in the cache; it is now open. 

		if(!configCache.hasOwnProperty()){
			JOBAD.UserConfig.reset(properObject.info.identifier);
		}
	},
	"onLoad": function(value, properObject, loadedModule){
		this.UserConfig = JOBAD.UserConfig.getFor(properObject.info.identifier);		
	}
}

/*
	Instance Based Configuration
*/

JOBAD.ifaces.push(function(JOBADRootElement, params){

	var config = params[1];
	
	var spec = JOBAD.modules.createProperUserSettingsObject({
		"cmenu_type": ["list", ['standard', 'radial'], 'standard', ["Context Menu Type", "Standard", "Radial"]],
		"sidebar_type": ["list", JOBAD.Sidebar.types, JOBAD.Sidebar.types[0], JOBAD.Sidebar.desc],
		"restricted_user_config": ["none", []],
		"auto_show_toolbar": ["bool", false, "Auto show all toolbars"]
	}, "");
	var cache = JOBAD.util.extend({}, (typeof config == 'undefined')?{}:config);

	this.Config = {};
	
	this.Config.get = function(key){
		if(!spec.hasOwnProperty(key)){
			JOBAD.console.log("Can't find '"+key+"' in Instance Config. ");
			return undefined;
		}
		if(cache.hasOwnProperty(key)){
			return cache[key];
		} else {
			return spec[key][2]; 
		}
		
	};
	
	this.Config.set = function(key, val){
		cache[key] = val;
	};
	
	this.Config.getTypes = function(){
		return spec;
	};
	
	this.Config.reset = function(){
		cache = {};
	};
	
	this.Config = JOBAD.util.bindEverything(this.Config, this);
});/* end   <JOBAD.config.js> */
/* start <core/JOBAD.core.instances.js> */
/*
	JOBAD 3 Core Instances
	
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/

JOBAD.Instances = {}; 

JOBAD.Instances.all = {}; 

var focused = undefined; 
var waiting = undefined; 

JOBAD.ifaces.push(function(me){
	//store a reference so it is kept

	JOBAD.Instances.all[me.ID] = this;

	var i_am_focused = false; 
	var i_am_waiting = false; 

	var prev_focus = undefined; //previous focus

	//Instance Namespace
	me.Instance = {}; 

	/*
		Focuses this JOBADInstance
		@returns false if the Instance can not be focused for some reason, true otherwise. 
	*/
	me.Instance.focus = function(){
		if(i_am_focused){
			return false; 
		}

		if(i_am_waiting){
			return false; 
		}

		if(typeof focused != "undefined"){
			//someone else is focused => throw him out. 
			prev_focus = focused; 
			focused.Instance.unfocus(); 

		}

		if(typeof waiting != "undefined"){
			//someone else is waiting => throw him out. 
			waiting.Instance.unfocus(); 
		}

		i_am_waiting = true; 
		waiting = me; 

		me.Event.trigger("instance.focus_query"); 

		me.Setup.enabled(function(){ //(IF ENABLED)
			if(i_am_waiting){
				i_am_waiting = false; 
				waiting = undefined; 
				i_am_focused = true; 
				focused = me; 

				me.Event.trigger("instance.focus", [prev_focus]); 

				me.Event.focus.trigger(prev_focus);

				prev_focus = undefined; 
			}
		});

		return true; 
	};

	/*
		Unfocuses this JOBADInstance
		@returns false if the Instance can not be unfocused for some reason, true otherwise. 
	*/
	me.Instance.unfocus = function(){
		if(i_am_focused){
			//we are fully focused, we can just unfocus

			focused = undefined; 
			i_am_focused = false; 

			me.Event.trigger("instance.unfocus"); 

			me.Event.unfocus.trigger(); 

			return true; 
		} else {
			if(i_am_waiting){
				i_am_waiting = false; //I am no longer waiting
				waiting = undefined; 

				me.Event.trigger("instance.focus_query_lost"); 

				return true; 
			} else {
				JOBAD.console.warn("Can't unfocus JOBADInstance: Instance neither focused nor queried. ");
				return false; 
			}
		}
	};

	/*
		Checks if this JOBADInstance is focused. 
	*/
	me.Instance.isFocused = function(){
		return i_am_focused; 
	};

	/*
		Call a function if this JOBADInstance is focused, otherwise call it
		once that is true. 
	*/
	me.Instance.focused = function(cb){
		if(me.Instance.isFocused()){
			cb.call(me); 
		} else {
			me.Event.once("instance.focus", function(){
				cb.call(me); 
			})
		}
	};

	var isAutoFocusEnabled = false; 
	var autoFocusHandlers = []; 
	var handlerNames = ["contextmenu.open", "contextMenuEntries", "dblClick", "leftClick", "hoverText"];

	me.Instance.enableAutoFocus = function(){
		if(isAutoFocusEnabled){
			return false; 
		}

		me.Event.trigger("instance.autofocus.enable"); 

		for(var i=0;i<handlerNames.length;i++){
			(function(){
				var handlerName = handlerNames[i];
				autoFocusHandlers.push(
					me.Event.on(handlerName, function(){
						me.Event.trigger("instance.autofocus.trigger", [handlerName]);
						me.Instance.focus(); 
					})
				);
			})(); 
			
		}

		isAutoFocusEnabled = true; 
		return true; 
	};

	me.Instance.disableAutoFocus = function(){
		if(!isAutoFocusEnabled){
			return false; 
		}

		me.Event.trigger("instance.autofocus.disable"); 

		while(autoFocusHandlers.length > 0){
			var handler = autoFocusHandlers.pop(); 
			me.Event.off(handler); 
		}

		isAutoFocusEnabled = false; 
		return true; 
	};

	me.Instance.isAutoFocusEnabled = function(){
		return isAutoFocusEnabled; 
	}
	
	me.Event.on("instance.beforeDisable", function(){
		if(i_am_focused){ //we are focused and are not waiting
			me.Instance.unfocus(); //unfocus me

			me.Event.once("instance.disable", function(){
				prev_focus = me; //I was the last one as well
				me.Instance.focus(); //requery me for enabling once I am disabled
			})
		}
	})
}); 

/*
	Gets the currently focused JOBADInstance. 
*/
JOBAD.Instances.focused = function(){
	return focused; 
}

/* focus Event */
JOBAD.events.focus = 
{
	'default': function(JOBADInstance, prevFocus){},
	'Setup': {
		'enable': function(root){
			return; //nothing to do
		},
		'disable': function(root){
			return; //nothing to do
		}
	},
	'namespace': 
	{
		
		'getResult': function(prevFocus){
			return this.modules.iterateAnd(function(module){
				module.focus.call(module, module.getJOBAD(), prevFocus);
				return true;
			});
		},
		'trigger': function(prevFocus){
			return this.Event.focus.getResult(prevFocus); 
		}
	}
};

/* focus Event */
JOBAD.events.unfocus = 
{
	'default': function(JOBADInstance){},
	'Setup': {
		'enable': function(root){
			return; //nothing to do
		},
		'disable': function(root){
			return; //nothing to do
		}
	},
	'namespace': 
	{
		
		'getResult': function(){
			return this.modules.iterateAnd(function(module){
				module.unfocus.call(module, module.getJOBAD());
				return true;
			});
		},
		'trigger': function(){
			return this.Event.unfocus.getResult(); 
		}
	}
};/* end   <core/JOBAD.core.instances.js> */
/* start <JOBAD.wrap.js> */
/*
	JOBAD.wrap.js
	
	Included at the end of the build to register all ifaces.
	
	Copyright (C) 2013-17 KWARC Group <kwarc.info>
	
	This file is part of JOBAD.
	
	JOBAD is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	JOBAD is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with JOBAD.  If not, see <http://www.gnu.org/licenses/>.
*/
for(var key in JOBAD.modules.extensions){
	JOBAD.modules.cleanProperties.push(key);
}

for(var key in JOBAD.events){
	if(!JOBAD.util.contains(SpecialEvents, key)){
		JOBAD.modules.cleanProperties.push(key);
	}
}/* end   <JOBAD.wrap.js> */
JOBAD.config.BootstrapScope = "bootstrap";
return JOBAD;
})();
