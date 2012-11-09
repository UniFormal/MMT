CodeMirror.defineMode("diff", function() {
  return {
    token: function(stream) {
	var ch = stream.next();
	stream.eatWhile(function (a) {return a != " " && a != "." && a != "=" && a != "#" && a != "%"});
	//stream.eatWhile("/[^ .]/");
	//console.log(ch);
	//console.log(stream.current());
	if (stream.current() == "%sig") {
	    //console.log(stream.current());
	    return "keyword";
        } else if (stream.current() == " type") return "atom";
        else if (stream.current() == "%") {
	    stream.skipToEnd();
	    return "comment";
	} else return "other"
    }
  };
});

CodeMirror.defineMIME("text/x-diff", "diff");
