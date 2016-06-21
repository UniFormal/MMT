(function($){
$.tAutoResize = {
options : {
minHeight : false,
maxHeight : 200
}
}
$.fn.tAutoResize = function( options ){
var opt = {};
$.extend( opt, $.tAutoResize.options, options );
return this.filter('textarea').each(function(){
opt.minHeight = opt.minHeight || $(this).height();
var com = {
self : $(this),
clone : null,
paddingBottom : parseInt( $(this).css('paddingBottom'), 10 ),
paddingTop : parseInt( $(this).css('paddingTop'), 10 ),
min : opt.minHeight || 50,
max : opt.maxHeight || 150,
oldHeight : -1
}
setResize( com );
});
// Private Functions
function setResize( com ){
com.clone = com.self.clone()[0];
var attributes = ['height', 'width', 'lineHeight', 'textDecoration', 'letterSpacing', 'wordWrap',
'paddingTop', 'paddingBottom', 'paddingLeft', 'paddingRight', 'fontFamily', 'fontSize', 'fontWeight'];
var cloneCSS = [];
for( var i in attributes )
cloneCSS[ attributes[i] ] = com.self.css( attributes[i] );
$(com.clone)
.css( cloneCSS )
.css({
position : 'absolute',
left : -10000,
height : 0,
overflow : 'hidden'
})
.attr({
'id' : null,
'class' : null
})
.insertBefore( com.self );
com.self
.bind('change.resize', function(){ resizeMe( com ); })
.bind('keypress.resize', function(){ resizeMe( com ); })
.bind('keyup.resize', function(){ resizeMe( com ); });
resizeMe( com );
}
function resizeMe( com ){
com.clone.innerHTML = com.self.val();
var height = Math.min( com.max, Math.max( com.min, getScrollHeight( com.clone, com ) ) );
if( height != com.oldHeight ){
com.self
.stop()
.animate( {height:height}, 400, function(){
com.self[0].style.overflow = getScrollHeight( com.self[0], com ) > com.max ? 'auto' : 'hidden';
} );
com.oldHeight = height;
}
}
function getScrollHeight( elem, com ){
return $.browser.webkit ? elem.scrollHeight - com.paddingBottom - com.paddingTop : elem.scrollHeight;
}
}
})(jQuery); 
