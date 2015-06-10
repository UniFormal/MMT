var browserApp = angular.module('browserApp', []);

browserApp.controller('browserController', function ($scope, $log) {

    $scope.baseInput = '';
    $scope.moduleInput = '';
    $scope.nameInput = '';
    $scope.theoryInput = '';
    $scope.patternInput = '';
    $scope.expType = true;
    $scope.expDef = true;
    $scope.searchResults = 'Results';
    $scope.activeTheoryInput = $('#activetheory').val;
    $scope.inputSimplify = false;
    $scope.inputAnalyze = false;
    $scope.inputBox = '';
    $scope.inputViewTerm = '';
    $scope.inputViewType = '';
    $scope.startClicked = 0;
    $scope.searchClicked = 0;
    $scope.parseClicked = 0;
    $scope.showHelpKey = 0;
    $scope.showArc = 0;
    $scope.showMain = 0;
    $scope.showMainKey = 0;
    $scope.showSearch = 0;
    $scope.showSearchKey = 0;
    $scope.showParse = 0;
    $scope.showParseKey = 0;
    $scope.showResultsKey = 0;


    $scope.start = function () {
        $scope.startClicked = 1;
        $scope.showArc = 1;
        $scope.showMain = 1;
        $scope.showMainKey = 1;
        $("#first").show();
    };

    $scope.search = function () {
        $scope.showResultsKey = 1;
        var base   = $scope.baseInput;
        var module = $scope.moduleInput;
        var name   = $scope.nameInput;
        var theory = $scope.theoryInput;
        var pattern= $scope.patternInput;

        $log.debug('Search is executed!');

        var type   = $scope.expType;
        var definition= $scope.expDef;
        $.ajax({'url': '/:search',
            'dataType': 'xml',
            'data': {base: base, module: module, name: name,
                theory: theory, pattern: pattern, type: type, definition: definition},
            'success': function(data){
                $('#results').children().replaceWith(data.firstChild);
                $log.debug('Search was successful! ' + data.firstChild);
            }
        });

    };

    $(function () {
        $("#pathtree").jstree({
            "core" : {"animation": 0},
            "xml_data" : {
                "ajax" : {
                    "url" : "/:tree",
                    "data" : function(node) {return (node == -1) ? ":root" : node.attr("id");}
                },
                "xsl" : "nest"
            },
            "themes" : {"theme" : "classic", "icons" : false},
            "plugins" : ["xml_data", "themes", "ui", "hotkeys"]
        });
    });

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

    $(function () {
        $('[data-toggle="tooltip"]').tooltip()
    });

    $(function () {
        $('[data-toggle="popover"]').popover({html:true})
    });

});
