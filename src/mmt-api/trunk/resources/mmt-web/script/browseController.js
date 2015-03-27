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
        $scope.showSearchKey = 0;
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
        $('[data-toggle="tooltip"]').tooltip()
    });

    $(function () {
        $('[data-toggle="popover"]').popover()
    });

});
