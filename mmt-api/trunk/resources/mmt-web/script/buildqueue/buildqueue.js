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
