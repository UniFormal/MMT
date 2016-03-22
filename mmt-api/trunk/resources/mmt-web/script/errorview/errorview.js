angular.module('searchApp', ['ngSanitize']).controller('SearchController',
  [ '$scope', '$http', '$window', function($scope, $http, $window) {
    $scope.columns =
      { errLevel : { x : true, long : 'level', search : '' }
      , group : { x : true, long : 'group', search : '' }
      , repo : { x : true, long : 'repo', search : '' }
      , fileName : { x : true, long : 'file name', search : '' }
      , fileLink : { x : false, long : 'content', search : '' }
      , fileDate : { x : false, long : 'modified', search : '' }
      , target : { x : true, long : 'target', search : '' }
      , sourceRef : { x : false, long : 'source', search : '' }
      , sourceRegion : { x : false, long : 'range', search : '' }
      , shortMsg : { x : true, long : 'short message', search : '' }};
    $scope.colProps = [];
    for (k in $scope.columns) { $scope.colProps.push(k) };
    $scope.field = 'shortMsg';
    $scope.results = [];
    $scope.hiddenData = [];
    $scope.groups = [];
    $scope.number = 0;
    $scope.maxNumber = 100;
    $scope.maxGroups = 6;
    $scope.searchText = '';
    $scope.host=document.location.origin;
    $scope.date=(new Date()).toString();
    $scope.query = function (doFilter, limit) {
         var res = '';
         for (k in $scope.columns) {
                var str = '';
                if (doFilter) str = escape($scope.columns[k].search);
                if (str != '') res = res + '&' + k + "=" + str;
            };
         for (var i = 0; i < $scope.hiddenData.length; i++) {
            for (k in $scope.columns) {
               var str = escape($scope.hiddenData[i][k]);
               if (str != '') res = res + '&' + k + i + "=" + str;
               };
            };
         return '?limit=' + limit + res;
        };
    $scope.clear = function() {
        for (k in $scope.columns) {
           $scope.columns[k].search = '';
        };
    };
    $scope.group = function() {
        $scope.columns[$scope.field].x = true;
        $http.get(':errors/group/' + $scope.field + $scope.query(false, $scope.maxGroups)).success(function(data) {
          $scope.groups = data;
        });
    };
    $scope.buildCount = 0;
    $scope.htmlText = '';
    $scope.showBuildResult = true;
    $scope.buildLevel = 0;
    $scope.build = function(res, doClean) {
        $scope.buildCount += 1;
        var tgt = res.target;
        if (doClean) tgt = "-" + tgt
        else tgt = tgt + $scope.buildLevel;
        action.exec(action.build(res.group + "/" + res.repo, tgt, encodeURIComponent(res.fileName)), function(data) {
          if (doClean) {
              $scope.htmlText = '';
              $scope.search();
          } else {
              if (data !== '<div></div>') $scope.htmlText = data;
          };
          $scope.$apply(function () {
              $scope.buildCount -= 1;
          });
        });
    };
    $scope.matchRow = function(res) {
        var match = true;
        for (k in $scope.columns) {
            match = match && (String(res[k]).indexOf($scope.columns[k].search) > -1);
        };
        return match;
    };
    $scope.buildAll = function(doClean) {
        for (var i = 0; i < $scope.results.length; i++) {
            var res = $scope.results[i];
            if ($scope.matchRow(res)) $scope.build(res, doClean);
        };
    };
    $scope.hide = function(content) {
        $http.get(':errors/search2?' + $scope.field + '=' + content + '&hide=true').success(function(data) {
            $scope.hiddenData.push(data);
            $scope.columns[$scope.field].search = '';
            $scope.search();
        });
    };
    $scope.clearlast = function() {
        $scope.hiddenData.pop();
        $scope.search();
    };
    $scope.clearhidden = function() {
        $scope.hiddenData = [];
        $scope.search();
    };
    $scope.search = function() {
        $scope.count();
        $scope.group();
        $http.get(':errors/search2' + $scope.query(true, $scope.maxNumber)).success(function(data) {
          $scope.results = data;
        });
    };
    $scope.count = function() {
        $http.get(':errors/count2' + $scope.query(true, $scope.maxNumber)).success(function(data) {
          $scope.number = data[0].count;
        });
    };
    $scope.serve = function(file) {
        $window.open(':errors/file?' + encodeURIComponent(file), '_blank');
    };
    $scope.search();
    $scope.sort = {
        col: 'id',
        asc: false
    };
    $scope.matchFilters = function() {
        return $scope.matchRow;
    };
  } ]);
