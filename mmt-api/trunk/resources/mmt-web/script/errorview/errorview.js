angular.module('searchApp', ['ngSanitize']).controller('SearchController',
  [ '$scope', '$http', function($scope, $http) {
    $scope.columns =
      { errLevel : { x : true, long : 'level', search : '' }
      , errType : { x : false, long : 'error type', search : '' }
      , archive : { x : false, long : 'archive', search : '' }
      , group : { x : true, long : 'group', search : '' }
      , repo : { x : true, long : 'repo', search : '' }
      , fileName : { x : true, long : 'file name', search : '' }
      , fileDate : { x : false, long : 'modified', search : '' }
      , target : { x : true, long : 'target', search : '' }
      , sourceRef : { x : true, long : 'source', search : '' }
      , shortMsg : { x : true, long : 'short message', search : '' }
      , longMsg : { x : false, long : 'long message', search : '' }
      , stackTrace : { x : false, long : 'stack trace', search : '' }};
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
    $scope.groupMode = false;
    $scope.query = function (limit) {
         var res = '';
         for (k in $scope.columns) {
                var str = escape($scope.columns[k].search);
                res = res + '&' + k + "=" + str;
            };
         return '?limit=' + limit + res;
        };
    $scope.clear = function() {
        for (k in $scope.columns) {
           $scope.columns[k].search = '';
        };
    };
    $scope.group = function() {
        $scope.groupMode = true;
        $scope.clear();
        $scope.columns[$scope.field].x = true;
        $http.get(':errors/group/' + $scope.field + $scope.query($scope.maxGroups)).success(function(data) {
          $scope.groups = data;
        });
    };
    $scope.buildCount = 0;
    $scope.htmlText = '';
    $scope.showBuildResult = true;
    $scope.build = function(res) {
        $scope.buildCount += 1;
        action.exec(action.build(res.archive, res.target, encodeURIComponent(res.fileName)), function(data) {
           if (data !== '<div></div>') $scope.htmlText = data;
           $scope.$apply(function () {
             $scope.buildCount -= 1;
           });
        });
    }
    $scope.hidden = function() {
        $http.get(':errors/hidden').success(function(data) {
          $scope.hiddenData = data;
        });
    };
    $scope.hide = function() {
        $http.get(':errors/search2' + $scope.query($scope.maxNumber) + '&hide=true').success(function(data) {
            $scope.clear();
            $scope.group();
            $scope.hidden();
            $scope.search();
        });
    };
    $scope.clearhidden = function() {
        $http.get(':errors/clear').success(function(data) {
            $scope.clear();
            $scope.group();
            $scope.hidden();
            $scope.search();
        });
    };
    $scope.search = function() {
        $scope.groupMode = false;
        $http.get(':errors/search2' + $scope.query($scope.maxNumber)).success(function(data) {
          $scope.results = data;
        });
      $scope.count();
    };
    $scope.count = function() {
        $http.get(':errors/count2' + $scope.query($scope.maxNumber)).success(function(data) {
          $scope.number = data[0].count;
        });
    };
    $scope.search();
    $scope.group();
    $scope.sort = {
        col: 'id',
        asc: false
    };
    $scope.matchFilters = function() {
        return function(elem) {
            var res = true;
            for (k in $scope.columns) {
                res = res && (String(elem[k]).indexOf($scope.columns[k].search) > -1);
            };
            return res;
        }
    };
  } ]);
