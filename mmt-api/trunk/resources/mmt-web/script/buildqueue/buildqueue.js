angular.module('buildQueueApp', []).controller('QueueViewer',
  [ '$scope', '$http', '$interval', function($scope, $http, $interval) {
      $scope.queue =
        { queue: []
        , blocked: []
        , finished: []
      };
      $scope.refreshRate = 1000;
      $scope.list = function() {
        $http.get(':queue').success(function(data) {
          $scope.queue = data;
        })
      };
      var stop;
      $scope.repeat = function() {
         stop = $interval($scope.list, $scope.refreshRate)
      };
      $scope.stopRepeat = function() {
          if (angular.isDefined(stop)) {
            $interval.cancel(stop);
            stop = undefined;
          }
      };
      $scope.repeat();
      $scope.$watch('refreshRate', function() {
            $scope.stopRepeat();
            $scope.repeat();
        });
  } ]);
