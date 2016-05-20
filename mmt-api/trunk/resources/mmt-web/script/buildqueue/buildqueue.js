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
        $http.get(':queue').success(function(data) {
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
        $http.get(':queue/archives').success(function(data) {
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
        $http.get(':queue/targets').success(function(data) {
          $scope.targets.list = data;
        })
      };
      $scope.getTargets();
      $scope.clear = function() {
        $http.get(':queue/clear').success(function(data) {
          $scope.refreshRate = 1;
          $scope.refreshCount = 1;
        })
      };
      $scope.make = function() {
        action.exec(action.build($scope.archives.current, $scope.targets.current + $scope.buildLevel, ""), function(data) {
      })};
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
      $scope.repeat();
      $scope.$watch('refreshRate', function() {
            $scope.stopRepeat();
            $scope.repeat();
        });
  } ]);
