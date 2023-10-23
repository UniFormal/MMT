angular.module('QueryApp', []).controller('QueryViewer', [ '$scope', '$http', function($scope, $http) {



    $scope.query_test = function() {
        document.getElementById("query_return").innerHTML = "Loading...";
        $http.get('/:sTeX/query/test').success(function(data) {
            document.getElementById("query_return").innerHTML = JSON.stringify(data,null,2);
        });
    };



    $scope.query_all = function() {
        document.getElementById("query_return").innerHTML = "Loading...";
        $http.get('/:sTeX/query/all').success(function(data) {
            document.getElementById("query_return").innerHTML = JSON.stringify(data,null,2);
        });
    };
    $scope.query_all_undefined = function() {
        document.getElementById("query_return").innerHTML = "Loading...";
        $http.get('/:sTeX/query/all_undefined').success(function(data) {
            document.getElementById("query_return").innerHTML = JSON.stringify(data,null,2);
        });
    };
    $scope.query_all_with_data = function() {
        document.getElementById("query_return").innerHTML = "Loading...";
        $http.get('/:sTeX/query/all_with_data').success(function(data) {
            document.getElementById("query_return").innerHTML = JSON.stringify(data,null,2);
        });
    };
    $scope.do_query = function(s) {
        document.getElementById("query_return").innerHTML = "Loading...";
        $http.post('/:query/sparql',s).success(function(data) {
            document.getElementById("query_return").innerHTML = JSON.stringify(data,null,2);
        });
    };
    $scope.queryString ="SELECT ?x WHERE { \n  <http://mathhub.info/MiKoMH/AI/course/notes/notes.omdoc#> (<http://mathhub.info/ulo#crossrefs>|<http://mathhub.info/ulo#specifies>|<http://mathhub.info/ulo#contains>|<http://mathhub.info/ulo#has-language-module>)+ ?x .\n  ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://mathhub.info/ulo#constant> .\n}";
    $scope.query_custom = function() {
        $scope.do_query($scope.queryString)
    };
}]);