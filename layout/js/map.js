var transformData = undefined;

/* depends on jquery */
animate_resize_map = function(data) {
  $.each(data, function(state) {
    var color = "blue";
    var style = {
      "fill": color,
      "transform": "scale3d(" + state.scale + "," + state.scale + ",1)",
      "transition": "all .2s ease-in-out"
    };
    $("#" + state.name).css(style)
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

$.ready(function(){
  get_resize_data();
});
