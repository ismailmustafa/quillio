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
    this.handwritingStyle;
    this.handwritingColor = 'red';
    this.imageData;
    this.lookupTitle = {};
    
    
    this.changeColor = function () {
      
      // Convert to hex
      var hexColor = this.RGB2Color(this.redRangeValue,this.greenRangeValue,this.blueRangeValue);
      
      this.handwritingColor = hexColor;
    };
    
    this.RGB2Color = function(r,g,b) {
      return '#' + this.byte2Hex(r) + this.byte2Hex(g) + this.byte2Hex(b);
    };
    this.byte2Hex = function(n) {
      var nybHexString = "0123456789ABCDEF";
      return String(nybHexString.substr((n >> 4) & 0x0F,1)) + nybHexString.substr(n & 0x0F,1);
    };
     
    this.handwritings = [];
    
    var mainScope = this;
    
    // Get all handwritings
    $http.get('http://localhost:8080/handwritings').success(function(data) {
      mainScope.handwritings = data;
      mainScope.lookup = {};
      for (var i = 0, len = data.length; i < len; i++) {
        mainScope.lookup[data[i].title] = data[i];
      }
    });
    
    this.getImage = function() {
      if (this.handwritingStyle != null) {
        var queryString = 'http://localhost:8080/image?red=' + this.redRangeValue + '&green=' + this.greenRangeValue + 
                                                              '&blue=' + this.blueRangeValue + '&handwritingId=' + 
                                                              this.lookup[this.handwritingStyle].handwritingId + '&text=' + this.messageInput;
        console.log(queryString);
        $http.get(queryString).success(function(data) {
          mainScope.imageData = data;
        });
      }
      // Default to custom handwriting
      else {
        console.log("defaulting to Whitwell");
        this.handwritingStyle = 'Whitwell';
        var queryString = 'http://localhost:8080/image?red=' + this.redRangeValue + '&green=' + this.greenRangeValue + 
                                                              '&blue=' + this.blueRangeValue + '&handwritingId=' + 
                                                              this.lookup[this.handwritingStyle].handwritingId + '&text=' + this.messageInput;
        console.log(queryString);
        $http.get(queryString).success(function(data) {
          data.replace(/\\/g , '');
          mainScope.imageData = data;
        });
      }
    };
      
  });
  
