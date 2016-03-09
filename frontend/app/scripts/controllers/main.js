'use strict';

/**
 * @ngdoc function
 * @name frontendApp.controller:MainCtrl
 * @description
 * # MainCtrl
 * Controller of the frontendApp
 */
angular.module('frontendApp')
  .controller('MainCtrl', function ($scope, $http) {
    
    this.messageInput = 'Quillio';
    this.colorPickerColor = 'red';
    this.redRangeValue = 0;
    this.greenRangeValue = 0;
    this.blueRangeValue = 0;
    this.handwritingStyle = '';
    this.myColor = 'red';
    
    
    this.changeColor = function () {
      
      // Convert to hex
      var hexColor = this.RGB2Color(this.redRangeValue,this.greenRangeValue,this.blueRangeValue);
      
      this.myColor = hexColor;
    };
    
    this.RGB2Color = function(r,g,b) {
      return '#' + this.byte2Hex(r) + this.byte2Hex(g) + this.byte2Hex(b);
    }
    this.byte2Hex = function(n) {
      var nybHexString = "0123456789ABCDEF";
      return String(nybHexString.substr((n >> 4) & 0x0F,1)) + nybHexString.substr(n & 0x0F,1);
    }
     
    
    this.handwritings = [];
    
    var mainScope = this;
    
    // Get all handwritings
    $http.get('http://localhost:8080/handwritings').success(function(data) {
      mainScope.handwritings = data;
    });
  });
  
