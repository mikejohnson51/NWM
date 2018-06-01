$(document).ready(function(){
  $.getJSON("https://json.geoiplookup.io/api?callback=?", function(response) {
    Shiny.onInputChange("getIP", response);
  }, "json");
});
