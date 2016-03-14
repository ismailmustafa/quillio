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
    this.phoneInput = '';
    this.colorPickerColor = 'red';
    this.redRangeValue = 0;
    this.greenRangeValue = 0;
    this.blueRangeValue = 0;
    this.handwritingStyle = undefined;
    this.handwritingColor = 'red';
    this.imageData = undefined;
    this.lookupTitle = {};
    this.handwritings = [];
    this.imageLoader = false;
    this.currentImageId = undefined;
    
    this.changeColor = function () {
      // Convert to hex
      var hexColor = this.rgb2hex(this.redRangeValue,this.greenRangeValue,this.blueRangeValue);
      this.handwritingColor = hexColor;
    };
    
    // Convert RGB to Hex
    this.rgb2hex = function(r,g,b) {
      return '#' +
        ('0' + parseInt(r,10).toString(16)).slice(-2) +
        ('0' + parseInt(g,10).toString(16)).slice(-2) +
        ('0' + parseInt(b,10).toString(16)).slice(-2);
    };

    // Get all handwritings
    this.baseUrl = 'http://ec2-52-32-2-100.us-west-2.compute.amazonaws.com';
    $http.get(this.baseUrl + '/api/handwritings').success(function(data) {
      mainScope.handwritings = data;
      mainScope.lookup = {};
      for (var i = 0, len = data.length; i < len; i++) {
        mainScope.lookup[data[i].title] = data[i];
      }
    });
    
    // Update handwriting image
    this.getImage = function() {

      this.imageLoader = true;
      this.currentImageId = this.randomAlphaNumericString(30);
      if (this.handwritingStyle === undefined) { this.handwritingStyle = 'Whitwell'; }
      var queryString = this.baseUrl + '/api/image?red=' + this.redRangeValue + '&green=' + this.greenRangeValue + 
                                                            '&blue=' + this.blueRangeValue + '&imageId=' + this.currentImageId + 
                                                            '&handwritingId=' + this.lookup[this.handwritingStyle].handwritingId + 
                                                            '&text=' + this.messageInput;
      $http.get(queryString).success(function(data) {
        mainScope.imageData = data;
        mainScope.imageLoader = false;
      });
    };
    
    // Update handwriting image
    this.sendImage = function() {

      this.imageLoader = true;
      
      var parameter = JSON.stringify({imageId:this.currentImageId, phoneNumber:this.phoneInput});
      $http.post(this.baseUrl + '/api/sendImage', parameter).success(function(data, status, headers, config) {
          // this callback will be called asynchronously
          // when the response is available
          console.log(data + '\n' + status + '\n' + headers + '\n' + config);
        }).error(function(data, status, headers, config) {
          // called asynchronously if an error occurs
          // or server returns response with an error status.
          console.log(data + '\n' + status + '\n' + headers + '\n' + config);
      });
    };
    
    this.randomAlphaNumericString = function(x){
    var s = '';
    while(s.length < x && x > 0){
        var r = Math.random();
        s+= (r < 0.1 ? Math.floor(r*100) : String.fromCharCode(Math.floor(r*26) + (r > 0.5 ?97 : 65)));
    }
    return s;
  };
  
  });
  
  
  
