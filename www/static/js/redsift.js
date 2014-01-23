var redSift = angular.module('myApp', []);
redSift.service('queryData', function($http, $rootScope) {
  var baseURL = "api/query?q=",
      exportURL = "api/export?e="
      tblData = {};
  return {
    getTables: function(db, tbl) {
      var url = baseURL + "SELECT * FROM " + db + "." + tbl + " ORDER BY random() LIMIT 100;";
      $http.get(url).success(function(data, status, headers, config) {
        tblData.tblHeaders = data[0]
        tblData.tblRows = data.slice(1, data.length);
        tblData.dbName = db;
        tblData.tblName = tbl;

        $rootScope.$broadcast('tblDataChanged', tblData);
      }).error(function(data, status, headers, config) {
        alert("Error: could not process request - server returned " + status + ":" + data);
        $rootScope.$broadcast('queryDataServerError');
      });
    },
    getQuery: function(query) {
      var url = baseURL + escape(query);
      $http.get(url).success(function(data, status, headers, config) {
        tblData.tblHeaders = data[0]
        tblData.tblRows = data.slice(1, data.length);
        //tblData.dbName = db;
        //tblData.tblName = tbl;

        $rootScope.$broadcast('tblDataChanged', tblData);
      }).error(function(data, status, headers, config) {
        alert("Error: could not process request - server returned " + status + ":" + data);
        $rootScope.$broadcast('queryDataServerError');
      });
    },
    exportQuery: function(query, fn) {
      var url = exportURL + escape(query) + "&n=" + fn;
      $http.get(url).success(function(data, status, headers, config) {
        alert(data);
      }).error(function(data, status, headers, config) {
        alert("Error: could not process request - server returned " + status + ":" + data);
        $rootScope.$broadcast('queryDataServerError');
      });
    }
  }
});
// controllers
redSift.controller('MenuController',
  function($scope, $http, queryData) {
    $scope.menuState = {}
    $scope.menuState.show = false;
    $scope.isLocked = false;
    $scope.databases = [];
    $scope.statusMsg = "";
    $scope.user_query = "";
    $scope.user_filename = "";

    $scope.runQuery = function() {
      $scope.lockMenu("loading", "your query");
      queryData.getQuery($scope.user_query);
    };

    $scope.runExport = function() {
      $scope.lockMenu("sending", "your export request");
      queryData.exportQuery($scope.user_query, $scope.user_filename);
    }

    // fetch tables from API
    $http.get('api/table/list').success(function(data, status, headers, config) {
      $scope.databases = data;
    }).error(function(data, status, headers, config) {
    });

    $scope.toggleMenu = function() {
      $scope.menuState.show = !$scope.menuState.show;
    }
    $scope.lockMenu = function(ops, tblName) {
      $('html, body').animate({ scrollTop: 0 }, 'fast');
      $scope.menuState.show = false;
      $scope.isLocked = true;
      $scope.statusMsg = "Please wait, " + ops + " " + tblName + "...";
    }
    $scope.unlockMenu = function() {
      $scope.isLocked = false;
    }
    $scope.loadTbl = function(dbName,tblName) {
      $scope.lockMenu("loading", tblName);
      queryData.getTables(dbName, tblName);
    }
    $scope.$on('tblDataChanged', function(event, tblData) {
      $scope.isLocked = false;
    });
    $scope.$on('queryDataServerError', function(event) {
      $scope.isLocked = false;
    });
  });
redSift.controller('DataViewController',
  function($scope, $timeout, queryData) {
    $scope.tblData = {}
    $scope.tblName = "";
    $scope.dbName = "";

    $scope.$on('tblDataChanged', function(event, tblData) {
      $scope.tblData = tblData;
      $scope.tblName = tblData.tblName;
      $scope.dbName = tblData.dbName;
      $timeout(function () { shortHandTableHeaders('dvtbl', 25)}, 1000);
    });
  });

// helper functions
function shortHandTableHeaders(tableID, limit) {
  // shorten table header names (reveal on hover)
  function shortHandHeaderTxt(txt, limit) {
    return txt.substring(0, limit - 3) + "...";
  }
  var ths = $('#' + tableID + ' thead tr th');
  var content;
  ths.each (function () {
      var $this = $(this);
      content = $this.text();
      if (content.length > limit) {
         $this.data('longheader', content);
         $this.text (shortHandHeaderTxt(content, limit));

         $this.hover (
             function() {
                 $(this).text($this.data('longheader'));
             },
             function () {
                 $(this).text(shortHandHeaderTxt($this.data('longheader'), limit));
             }
         );
       }
  });
}