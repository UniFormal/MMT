angular.module('buildQueueApp', []).controller('QueueViewer',
  [ '$scope', '$http', '$interval', function($scope, $http, $interval) {
      $scope.queue =
        { queue: []
        , blocked: []
        , finished: []
      };
      $scope.refreshCount = 0;
      $scope.refreshRate = 1;
      $scope.list = function() {
        $http.get('/:buildserver/queue').success(function(data) {
          $scope.queue = data;
          $scope.refreshCount += 1;
          if ($scope.refreshCount >= 20) {
              $scope.refreshCount = 1;
              $scope.refreshRate *= 2;
          }
        })
      };
      $scope.buildLevel = "5";
      $scope.archives =
        { list: []
        , current: ""
        };
      $scope.getArchives = function() {
        $http.get('/:buildserver/archives').success(function(data) {
          $scope.archives.list = data;
          $scope.archives.list.push("");
        })
      };
      $scope.getArchives();
      $scope.targets =
        { list: []
        , current: "sms"
        };
      $scope.getTargets = function() {
        $http.get('/:buildserver/targets').success(function(data) {
          $scope.targets.list = data;
        })
      };
      $scope.getTargets();
      $scope.clear = function() {
        $http.get('/:buildserver/clear').success(function(data) {
          $scope.refreshRate = 1;
          $scope.refreshCount = 1;
        })
      };
      $scope.clearfinished = function() {
          $http.get('/:buildserver/clearfinished').success(function(data) {
              $scope.refreshRate = 1;
              $scope.refreshCount = 1;
          })
      };
      $scope.redo = function(id) {
          $http.get('/:buildserver/redo?'+id).success(function(data) {
              $scope.refreshRate = 1;
              $scope.refreshCount = 1;
          })
      };
      $scope.fileName ='';
      $scope.make = function() {
        var t = $scope.targets.current;
        if ($scope.buildLevel == "-") t = "-" + t
        else t = t + $scope.buildLevel;
        $scope.buildLevel = "5";
        var ca = $scope.archives.current;
        for (var i = 0; i < $scope.archives.list.length; i++) {
          var a = $scope.archives.list[i];
          if (a != '' && (ca == a || ca == '')) {
          action.exec(action.build(a, t, encodeURIComponent($scope.fileName)), function(data) {
          })}}};
      var stop;
      $scope.repeat = function() {
         stop = $interval($scope.list, $scope.refreshRate * 1000)
      };
      $scope.stopRepeat = function() {
          if (angular.isDefined(stop)) {
            $interval.cancel(stop);
            $scope.refreshCount = 0;
            stop = undefined;
          }
      };
      $scope.colorOf = function(result) {
        var res = 'text-mute';
        if (result.indexOf('success') > -1) res = 'text-success';
        if (result.indexOf('up-to-date') > -1) res = 'text-warning';
        if (result.indexOf('failure') > -1) res = 'text-danger';
        return res
      };
      $scope.repeat();
      $scope.$watch('refreshRate', function() {
            $scope.stopRepeat();
            $scope.repeat();
        });
  } ]);
