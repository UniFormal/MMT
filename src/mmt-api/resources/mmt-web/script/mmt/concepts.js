// initialize jobad
var JOBAD1;
$(function(){	
  JOBAD1 = new JOBAD($("#formal,#formal-table"));

  JOBAD1.modules.load("kwarc.mmt.hovering", []);
  JOBAD1.modules.load("kwarc.mmt.concintvw", []);
  JOBAD1.Setup();
});


$( function() {
	$.ajax({
            type: "GET",
            url: ":concepts?conlist",
	    // datatype: "json",
	    success: function(data) {
		$( "#mysearch" ).autocomplete({
                         source: $.parseJSON(data)
                       });
	    }
          });
} );
