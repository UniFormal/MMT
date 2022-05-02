function stexOverlayOn(id,url) {
    var elem = document.getElementById(id);
    elem.classList.add('activatemouse');
    elem.src = url;
    elem.style.opacity = 1;
    elem.style.width = "100ch";
    elem.style.height = "50ch";
}
function stexOverlayOff(id) {
    var elem = document.getElementById(id);
    elem.classList.remove('activatemouse');
    elem.src="";
    elem.style.opacity = 0;
    elem.style.width = "0%";
    elem.style.height = "0";
}

function stexMainOverlayFade() {
    var iframe = document.getElementById("stexMainOverlay");
    iframe.classList.add('activatemouse');
    iframe.style.opacity = 1;
    iframe.style.width = "80ch";
    iiframe = document.getElementById("stexoverlayinner");
    iiframe.style.width="100%";
    iiframe.style.height = iiframe.contentWindow.document.documentElement.scrollHeight + 'px';
    iframe.style.height = (iiframe.contentWindow.document.documentElement.scrollHeight + 70) + 'px';
}
function stexMainOverlayOn(url) {
    document.getElementById("stexoverlayinner").src = url;
    stexMainOverlayFade();
}