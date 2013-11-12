function goto(s) {
   $('#content').attr('src',s)
}

$(function() {
  var query = window.location.search.substring(1);
  if (query != "")
	 goto(query);
});