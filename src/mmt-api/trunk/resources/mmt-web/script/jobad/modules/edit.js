var edit = {
	info: {
		'identifier' : 'kwarc.latin.editing',
		'title' : 'LATIN Editing Service',
		'author': 'Kwarc',
		'description' : 'The editing service for LATIN-MMT content',
		'version' : '1.0',
		'dependencies' : [],
		'hasCleanNamespace': false
	},
	

	contextMenuEntries: function(target, JOBADInstance) {
		var me = this;
		return {
			"edit" : function() {me.edit(target);}
		};
	},
	
	findClosest : function(target) {
		var ancestor = $(target).closest('math');
		if (typeof ancestor !== 'undefined' && ancestor !== false) {
			var sibling = $(target).siblings('math');
			console.log(sibling);
			return sibling;
		} else {
			return ancestor;
		}
		
	},

	edit : function(target) {
		console.log("calling edit");
		var math = this.findClosest(target);
		var path = math.attr('jobad:owner');
		var comp = math.attr('jobad:component');
		var arr = path.split('?');
		if (arr.length >= 2) {
    		url = "/:mmt?" + arr[0] + "?" + arr[1] + "?" ;
    		if (arr.length == 3) {
    			url += arr[2];
    			if (comp != "" && comp != "name") {
    				url += " component " + comp
    			}
    		}
    		url += "?text";    
    		function cont(data) {
    			console.log(path);
    			console.log(comp);
    			var math = null;
    			$("math").each(function(i,v) {
    				if($(v).attr("jobad:owner") == path && $(v).attr("jobad:component") == comp) {
    					math = $(v).parent();
    				} 
    			});
    			
    			if (math == null) {
    				return false;
    			}
    			var spres = data.split("\n"); 
    			var rows = spres.length;
    			var columns = 20;
    			var i;
    			for (i = 0; i < spres.length; ++i) {
    				if (spres[i].length > columns)
    					columns = spres[i].length
    			}
    			
    			math.html('<textarea rows="' + rows +'" cols="' + columns + '\">' + data + '</textarea>');
    			console.log(math);
				console.log(math.parent());
				math.parent().append('<button id="save" style="width:20px;height:20px" onClick=\'compileText(\"' + path + '\", \"true\")\'> Save </button>');   	
				//   			"<div class=\"parser-info\" style=\"padding-bottom:5px;\"></div>" + 
				//   			"<button id=\"compile\" onClick=compileText(\"" + path + "\",\"false\") class=\"ui-button ui-widget ui-state-default\"" +
				// 			"role=\"button\">Compile</button>" + 
				//  			"<button id=\"save\" onClick=compileText(\"" + path + "\",\"true\") disabled=\"true\" class=\"ui-button ui-widget ui-state-disabled ui-state-default ui-corner-all ui-button-text-only\"" +
				//   			"role=\"button\" aria-disabled=\"false\">Save</button>" + 
				//   			"<div class=\"parser-response\" style=\"padding-top:5px;\"></div> + "
				//				"");
    	    
				/** broken jquery ui stuff -- to fix later
 					math.parent().find("button").button({
 					icons: {
 					primary: "ui-icon-disk"
 					},
					
 					text: false
 					});
 				*/
				
    			var textarea = math.find("textarea")[0];
    			
    			mycm = CodeMirror.fromTextArea(textarea);
				/*
    			  mycm.setOption('onChange', function() {
    			  var save = $("#save");
    			  save.attr("disabled", "disabled");
    			  save.addClass("ui-state-disabled")
    			  });
				*/
    		}
    		$.ajax({
    			'type' : 'get',
    			'url' : url,
    			'dataType' : 'text',
    			'success' : cont,
    			'async' : false
    		});	
		}	
	},
}


JOBAD.modules.register(edit);
