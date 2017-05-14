// this file should only be included via ?do=mmt_resource&path=/script/mmt/mmt-url.js
// as certain content is replaced at runtime and expected to be present

var mmtUrl = (function(){

    // function to check if a string ends with another string
    var endsWith = function(subjectString, searchString) {
        var lastIndex = subjectString.lastIndexOf(searchString);
        return lastIndex !== -1 && lastIndex === subjectString.length - searchString.length;
    };

    // cleaning up a url
    var cleanURL = function(url){
        return url.replace(/\/+$/, '');
    };


    // the current pathname requested on the server
    // this will be replaced by the MMT Webserver at runtime
    var serverRequestPath = cleanURL("%s");

    // the actual pathname requested on the server
    var actualRequestPath = cleanURL(window.location.pathname);

    var basePath;

    // if the path ends with our path
    if(endsWith(actualRequestPath, serverRequestPath)){
        basePath = actualRequestPath.substring(0, actualRequestPath.length - serverRequestPath.length);
    } else {
        basePath = '/';
    }

    basePath = window.location.protocol + '//' + window.location.host + basePath;
    return basePath;
})();