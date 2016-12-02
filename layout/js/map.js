var transformData = undefined;

/* depends on jquery */
animate_resize_map = function(data) {
  $.each(data, function() {
    var color = "blue";
    var scale = this.scale;
    var style = {
      "fill": color,
      "transform": "scale3d(" + scale + "," + scale + ",1)",
      "transition": "all .5s ease-in-out"
    };
    $("#" + this.name).css(style)
  });
}

get_resize_data = function() {
  $.get( "js/state-size-transform.json", function( data ) {
    transformData = data;
    animate_category_and_time("Irrigation", "1985")
  });
}

animate_category_and_time = function(cat, timestep) {
  var statesTransform = transformData[timestep][cat];
  animate_resize_map(statesTransform);
}

$(document).ready(function(){
  get_resize_data();
});
