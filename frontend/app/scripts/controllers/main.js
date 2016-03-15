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
    this.displayError = false;
    
    // Setup responsive side menu
    $(".button-collapse").sideNav({
      closeOnClick: true
    });
    
    // Trigger modal
    $(document).ready(function(){
      // the "href" attribute of .modal-trigger must specify the modal ID that wants to be triggered
      $('.modal-trigger').leanModal();
    });
    
    // HTTP REQUESTS
    
    // Initalize handwritings
    this.handwritings.push("hello");
    console.log(this.handwritings.length);
    console.log("initialized");
    // Get all handwritings
    $http.get('/api/handwritings').success(function(data) {
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
      console.log(this.handwritingStyle);
      if (this.handwritingStyle === undefined) { this.handwritingStyle = 'Whitwell'; }
      if (this.handwritingStyle in this.lookup) {
        var queryString = '/api/image?red=' + this.redRangeValue + '&green=' + this.greenRangeValue + 
                                                              '&blue=' + this.blueRangeValue + '&imageId=' + this.currentImageId + 
                                                              '&handwritingId=' + this.lookup[this.handwritingStyle].handwritingId + 
                                                              '&text=' + this.messageInput;
        $http.get(queryString).success(function(data) {
          mainScope.imageData = data;
          mainScope.imageLoader = false;
          Materialize.toast('updated image',2000);
        }).error(function() {
          mainScope.imageLoader = false;
          Materialize.toast('failed to update image',2000);
        });
      }
      else {
        mainScope.imageLoader = false;
        this.displayError = true;
        Materialize.toast('failed to update image',2000);
      }
    };
    
    // Update handwriting image
    this.sendImage = function() {

      this.imageLoader = true;
      
      $http.post('/api/sendImage/?imageId='+this.currentImageId+'&phoneNumber=' + this.phoneInput).success(function() {
          mainScope.imageLoader = false;
          Materialize.toast('image sent',2000);
        }).error(function() {
          mainScope.imageLoader = false;
          Materialize.toast('failed to send image',2000);
      });
    };
    
    // HELPER FUNCTIONS
    
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
    
    // Generate arbitrary string for imageId
    this.randomAlphaNumericString = function(x){
    var s = '';
    while(s.length < x && x > 0){
        var r = Math.random();
        s+= (r < 0.1 ? Math.floor(r*100) : String.fromCharCode(Math.floor(r*26) + (r > 0.5 ?97 : 65)));
    }
    return s;
  };
  
  });
  
  
  
