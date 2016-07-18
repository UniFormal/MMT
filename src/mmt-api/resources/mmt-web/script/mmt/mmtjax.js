/*
  mmtjax is a Javascript function that allows interspersing MMT content into an HTML page.
  This content is rendered by sending ajax requests to an MMT server.
  It is not affiliated with MathJax in any way except for sharing the basic idea.
*/
var mmtjax = {
   
   /* like jax but takes the input from a <script> element */
   jaxElement(server, elem, cont) {
	  var thy = $(elem).attr(mmtattr.theory);
	  if (thy != null) {
		 var inFormat = $(elem).attr(mmtattr.inFormat);
		 var outFormat = $(elem).attr(mmtattr.outFormat);
		 mmteval.exec(server, thy, $(elem).text(), inFormat, outFormat, cont)
	  }
   },
   
   /* run jaxElement on all <script type='mmt'> tags and replace them with the result */
   jaxDocument: function(server) {
	   var me = this;
	   
	   $("script[type='mmt']").each(function(index, elem){
		   function replace(res) {$(elem).replaceWith(res);};
		   me.jaxElement(server, elem, replace);
	   });
   },
};
