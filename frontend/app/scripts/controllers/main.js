'use strict';

/**
 * @ngdoc function
 * @name frontendApp.controller:MainCtrl
 * @description
 * # MainCtrl
 * Controller of the frontendApp
 */
angular.module('frontendApp')
  .controller('MainCtrl', function ($scope) {
    $scope.messageInput = "Quillio";
    $scope.colorPickerColor = "red";
    $scope.redRangeValue = 0;
    $scope.greenRangeValue = 0;
    $scope.blueRangeValue = 0;
    $scope.colorChanged = function() {
      console.log(redRangeValue);
      console.log(greenRangeValue);
      console.log(blueRangeValue);
    };
  });
