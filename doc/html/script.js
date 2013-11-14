function goto(s) {
  $('#content').attr('src',s)
}

function addStyle() {
  var cssTag = document.createElement("link");
  cssTag.setAttribute("rel", "stylesheet");
  cssTag.setAttribute("type", "text/css");
  var depth = $('#content').attr('src').split("/").length - 1;
  var up = new Array(depth + 1).join("../");
  cssTag.setAttribute("href", up + 'style.css');
  frames[1].document.head.appendChild(cssTag)
}

$(function() {
  var query = window.location.search.substring(1);
  if (query != "")
	 goto(query);
});