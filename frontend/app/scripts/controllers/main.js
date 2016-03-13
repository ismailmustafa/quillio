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
    
    var mainScope = this;
    this.messageInput = 'Quillio';
    this.colorPickerColor = 'red';
    this.redRangeValue = 0;
    this.greenRangeValue = 0;
    this.blueRangeValue = 0;
    this.handwritingStyle = undefined;
    this.handwritingColor = 'red';
    this.imageData = undefined;
    this.lookupTitle = {};
    this.handwritings = [];
    this.gettingImage = false;
    this.currentImageId = '';
    
    this.changeColor = function () {
      // Convert to hex
      var hexColor = this.rgb2hex(this.redRangeValue,this.greenRangeValue,this.blueRangeValue);
      this.handwritingColor = hexColor;
      console.log(this.handwritingColor);
    };
    
    // Convert RGB to Hex
    this.rgb2hex = function(r,g,b) {
      return '#' +
        ('0' + parseInt(r,10).toString(16)).slice(-2) +
        ('0' + parseInt(g,10).toString(16)).slice(-2) +
        ('0' + parseInt(b,10).toString(16)).slice(-2);
    };

    // Get all handwritings
    $http.get('http://localhost:8080/handwritings').success(function(data) {
      mainScope.handwritings = data;
      mainScope.lookup = {};
      for (var i = 0, len = data.length; i < len; i++) {
        mainScope.lookup[data[i].title] = data[i];
      }
    });
    
    // Update handwriting image
    this.getImage = function() {

      this.gettingImage = true;
      this.currentImageId = this.randomAlphaNumericString(30);
      console.log(this.currentImageId);
      if (this.handwritingStyle === undefined) { this.handwritingStyle = 'Whitwell'; }
      var queryString = 'http://localhost:8080/image?red=' + this.redRangeValue + '&green=' + this.greenRangeValue + 
                                                            '&blue=' + this.blueRangeValue + '&imageId=' + this.currentImageId + 
                                                            '&handwritingId=' + this.lookup[this.handwritingStyle].handwritingId + 
                                                            '&text=' + this.messageInput;
      console.log(queryString);
      $http.get(queryString).success(function(data) {
        mainScope.imageData = data;
        mainScope.gettingImage = false;
      });
    };
    
    this.randomAlphaNumericString = function(x){
    var s = '';
    while(s.length < x && x > 0){
        var r = Math.random();
        s+= (r<0.1?Math.floor(r*100):String.fromCharCode(Math.floor(r*26) + (r>0.5?97:65)));
    }
    return s;
  };
  
  });
  
  
  
