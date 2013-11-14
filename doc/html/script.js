function goto(s) {
  $('#content').attr('src',s)

  var cssTag = document.createElement("link");
  cssTag.setAttribute("rel", "stylesheet");
  cssTag.setAttribute("type", "text/css");
  cssTag.setAttribute("href", filename);
  frames[1].document.head.appendChild(cssTag)
}

$(function() {
  var query = window.location.search.substring(1);
  if (query != "")
	 goto(query);
});