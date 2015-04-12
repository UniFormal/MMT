'use strict';

var helpApp = angular.module('helpApp', ['ngRoute']);


helpApp.config(['$routeProvider', function ($routeProvider) {
        $routeProvider.
            when('/', {templateUrl: 'title.html', controller: 'viewController'}).
            when('/api', {templateUrl: 'api.html', controller: 'viewController'}).
            when('/archives', {templateUrl: 'archives.html', controller: 'viewController'}).
            when('/backend', {templateUrl: 'backend.html', controller: 'viewController'}).
            when('/build', {templateUrl: 'build.html', controller: 'viewController'}).
            when('/configure', {templateUrl: 'configure.html', controller: 'viewController'}).
            when('/content', {templateUrl: 'content.html', controller: 'viewController'}).
            when('/controller', {templateUrl: 'controller.html', controller: 'viewController'}).
            when('/demos', {templateUrl: 'demos.html', controller: 'viewController'}).
            when('/extensions', {templateUrl: 'extensions.html', controller: 'viewController'}).
            when('/independence', {templateUrl: 'independence.html', controller: 'viewController'}).
            when('/install', {templateUrl: 'install.html', controller: 'viewController'}).
            when('/language', {templateUrl: 'language.html', controller: 'viewController'}).
            when('/log', {templateUrl: 'log.html', controller: 'viewController'}).
            when('/memory', {templateUrl: 'memory.html', controller: 'viewController'}).
            when('/oaf', {templateUrl: 'oaf.html', controller: 'viewController'}).
            when('/papers', {templateUrl: 'papers.html', controller: 'viewController'}).
            when('/projects', {templateUrl: 'projects.html', controller: 'viewController'}).
            when('/query', {templateUrl: 'query.html', controller: 'viewController'}).
            when('/related', {templateUrl: 'related.html', controller: 'viewController'}).
            when('/repos', {templateUrl: 'repos.html', controller: 'viewController'}).
            when('/run', {templateUrl: 'run.html', controller: 'viewController'}).
            when('/services', {templateUrl: 'services.html', controller: 'viewController'}).
            when('/twelfmod', {templateUrl: 'twelfmod.html', controller: 'viewController'}).
            when('/uris', {templateUrl: 'uris.html', controller: 'viewController'}).
            when('/applications-build', {templateUrl: 'applications/build.html', controller: 'viewController'}).
            when('/applications-gui', {templateUrl: 'applications/gui.html', controller: 'viewController'}).
            when('/applications-http', {templateUrl: 'applications/http.html', controller: 'viewController'}).
            when('/applications-index', {templateUrl: 'applications/index.html', controller: 'viewController'}).
            when('/applications-jedit', {templateUrl: 'applications/jedit.html', controller: 'viewController'}).
            when('/applications-shell', {templateUrl: 'applications/shell.html', controller: 'viewController'}).
            when('/applications-web', {templateUrl: 'applications/web.html', controller: 'viewController'}).
            when('/language-index', {templateUrl: 'language/index.html', controller: 'viewController'}).
            when('/language-literals', {templateUrl: 'language/literals.html', controller: 'viewController'}).
            when('/language-namespaces', {templateUrl: 'language/namespaces.html', controller: 'viewController'}).
            when('/syntax-index', {templateUrl: 'syntax/index.html', controller: 'viewController'}).
            when('/syntax-text', {templateUrl: 'syntax/text.html', controller: 'viewController'}).
            when('/syntax-text-parser', {templateUrl: 'syntax/text-parser.html', controller: 'viewController'}).
            when('/syntax-xml', {templateUrl: 'syntax/xml.html', controller: 'viewController'}).
            when('/syntax-xml-parser', {templateUrl: 'syntax/xml-parser.html', controller: 'viewController'}).
            otherwise({redirectTo: '/'});
    }]);

helpApp.directive('bsActiveLink', ['$location', '$log', function ($location, $log) {
        return {
            restrict: 'A', //use as attribute
            replace: false,
            link: function (scope, elem) {
                //after the route has changed
                scope.$on("$routeChangeSuccess", function () {
                    $log.debug('Route changed!');
                    var hrefs = ['/#' + $location.path(),
                        '#' + $location.path(),
                        $location.path()];
                    angular.forEach(elem.find('a'), function (a) {
                        a = angular.element(a);
                        if (-1 !== hrefs.indexOf(a.attr('href'))) {
                            $log.debug('Route changed to ' + a + '!');
                            a.parent().addClass('active');
                        } else {
                            a.parent().removeClass('active');
                        }

                    });
                });
            }
        }
    }]);


helpApp.controller('helpController',['$scope', '$location', function($scope, $location) {
    $scope.dropdownKey = 1;

}]);

helpApp.controller('viewController',['$scope', '$location', function($scope, $location) {
    $scope.height = 80;
    $scope.reload = function(){$scope.height = 80;};
    $scope.reload();

}]);

