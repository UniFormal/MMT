// initialize jobad
var JOBAD1;
$(function(){	
  JOBAD1 = new JOBAD($("#main,#parseForm #inputviewterm,#parseForm #inputviewtype,#results,#breadcrumbs,#sidebar,#pathtree"));

  JOBAD1.modules.load("kwarc.mmt.hovering", []);
  JOBAD1.modules.load("kwarc.mmt.intvw", []);
  JOBAD1.Setup();
});

// intialize sidebar
$(function () {
   $("#pathtree").jstree({
      "core" : {"animation": 0},
      "xml_data" : {
         "ajax" : {
            "url" : ":tree",
            "data" : function(node) {return (node == -1) ? ":root" : node.attr("id");}
         },
         "xsl" : "nest"
      },
      "themes" : {"theme" : "classic", "icons" : false},
      "plugins" : ["xml_data", "themes", "ui", "hotkeys"]
   });
});

// initialize tooltips (still used?)
$(function () {
   $('[data-toggle="tooltip"]').tooltip()
});

// initialize popovers (still used?)
$(function () {
   $('[data-toggle="popover"]').popover({html:true})
});

// initialize incremental search
$(function() {
   $('#treeview').jstree({
      "plugins" : []
   });
   var searcher = new Incsearch($('#incsearch'));
   var treeeview = new Treeview(searcher);
});

// add event handler to parse box that parses via the server 
$(function(){
   var inpbox = $('#parseForm #inputbox');
   var ana = $('#parseForm #inputanalyze')[0];
   var simp = $('#parseForm #inputsimplify')[0];
   function simpPres(e) {
      var eS = simp.checked ? qmt.simplify(e) : e;
      return qmt.present(eS);
   }
   $('#parseForm #inputbox,#parseForm #inputanalyze,#parseForm #inputsimplify').on('change keyup paste', function(){
      var q = qmt.parse(qmt.literalString(inpbox.val()));
      if (ana.checked) {
         var qA = qmt.analyze(q);
         q = qmt.let(qA, qmt.tuple(simpPres(qmt.bound(1)) + simpPres(qmt.bound(2))));
      } else {
         q = simpPres(q);
      }
      qmt.exec(q,
            function(resultNode){
         var result = $(resultNode.firstChild).children();
         var inpviewterm = $('#parseForm #inputviewterm')[0];
         var inpviewtype = $('#parseForm #inputviewtype')[0];
         var tm = result[0].firstChild;
         var tp = ana.checked ? result[1].firstChild : window.document.createElement('div');
         inpviewterm.replaceChild(tm, inpviewterm.firstChild);
         inpviewtype.replaceChild(tp, inpviewtype.firstChild);
      }
      );
   })
});

/** function called by generated interaction elements */
var interaction = {
   /** click on a search result */
   resultClick: function(p){
      interactiveViewing.navigate(p);
   },
};
