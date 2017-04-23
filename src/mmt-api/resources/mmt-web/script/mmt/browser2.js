// initialize jobad
var JOBAD1;
$(function(){	
  JOBAD1 = new JOBAD($("#main,#parseForm #inputviewterm,#parseForm #inputviewtype,#results,#breadcrumbs,#sidebar,#Archives"));

  JOBAD1.modules.load("kwarc.mmt.hovering", []);
  JOBAD1.modules.load("kwarc.mmt.intvw", []);
  JOBAD1.Setup();
});

// intialize sidebar
$(function () {
            mmt.ajaxAppendBox(mmt.makeURL('/:bla?:root'), '#Archives');
});

var tmenu = {
	concepts: function() {
		$("#main").replaceWith('<table id="layouttable"><tr><td class="layouttablecell"><div id="sidebar" class="cellcontent"> <div id="pathtree"><span/></div></div></td><td class="layouttablecell"><div class="cellcontent"><div id="hurtz"><span/></div></div></div></td></tr></table>');
		$("#pathtree").jstree({
      "core" : {"animation": 0},
      "xml_data" : {
         "ajax" : {
            "url" : ":bla",
            "data" : function(node) {return (node == -1) ? ":concepts" : node.attr("id");}
         },
         "xsl" : "nest"
      },
      "themes" : {"theme" : "classic", "icons" : false},
      "plugins" : ["xml_data", "themes", "ui", "hotkeys"]
   });
		// this.replaceIn(mmt.makeURL('/:bla?:concepts'),'#main');
		// mmt.ajaxAppendBox(mmt.makeURL('/:bla?:concepts'),'#main');
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
         this.replaceIn(mmt.makeURL('/:bla?:concept=' + p), 'hurtz');
   	},
};
