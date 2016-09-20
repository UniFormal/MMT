// initialize jobad
var JOBAD1;
$(function(){	
  JOBAD1 = new JOBAD($("#formal,#formal-table"));

  JOBAD1.modules.load("kwarc.mmt.hovering", []);
  JOBAD1.modules.load("kwarc.mmt.intvw", []);
  JOBAD1.Setup();
});

var conceptmenu = {
	concepts: function() {
		$("#center").replaceWith('<table id="layouttable"><tr><td class="layouttablecell"><div id="sidebar" class="cellcontent"> <div id="concepttree"><span/></div></div></td><td class="layouttablecell"><div class="cellcontent"><div id="conceptmain" class="conceptmain"><span/></div></div></div></td></tr></table>');
		$("#concepttree").jstree({
      "core" : {"animation": 0},
      "xml_data" : {
         "ajax" : {
            "url" : ":concepts",
            "data" : function(node) {return (node == -1) ? ":menu" : node.attr("id");}
         },
         "xsl" : "nest"
      },
      "themes" : {"theme" : "classic", "icons" : false},
      "plugins" : ["xml_data", "themes", "ui", "hotkeys"]
   });
	},

	replaceIn : function (url, targetid) {
      			function cont(data) {
         			var serializer = new XMLSerializer();
         			var xmlString = serializer.serializeToString(data);
         			var targetnode = $('#' + targetid).children();
         			targetnode.replaceWith(xmlString);
         			// $(targetid).replaceWith(xmlString);
      			}
      			$.ajax({ 'url': url,
             			'dataType': 'xml',
             			'async': true,
             			'success': cont
            		});
   		},

	sideBarClick : function(p) {
         this.replaceIn(mmt.makeURL('/:concepts?:concept=' + p), 'conceptmain');
   	},
};

var interaction = {
   /** click on a search result */
   resultClick: function(p){
      interactiveViewing.navigate(p);
   },
};
