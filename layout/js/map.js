var transformData = undefined;
var svg = undefined;
var pt = undefined;
/* depends on jquery */
animate_resize_map = function(data) {
  $.each(data, function() {
    var color = "blue";
    var scale = this.scaleFactor;
    var style = {
      "fill": color,
      "transform": "scale3d(" + scale + "," + scale + ",1)",
      "transition": "all 1s ease-in-out"
    };
    $("#" + this.state_name).css(style)
  });
}

animate_update_tooltips = function(data) {
  $.each(data, function() {
    // create the tooltip text
    var newtip = this.state_name + ': ' + this.value + ' million gallons per day'
    // this section doesn't work. trying to assign that text to the hovertext 
    // call for onmousemove for each object, but haven't found the way. help!
    var newtipobj = {
      "onmousemove": "hovertext('" + newtip + "', evt);"
    };
    $("#" + this.state_name + "-mouseover").mousemove = hovertext(newtip);
  });
}

get_resize_data = function() {
  $.get( "js/scaleFactors.json", function( data ) {
    transformData = data;
    animate_category_and_time("Irrigation", "1985")
  });
}

animate_category_and_time = function(cat, timestep) {
  var statesTransform = transformData["totState"][timestep][cat];
  animate_resize_map(statesTransform);
  animate_update_tooltips(statesTransform);
}

$(document).ready(function(){
  get_resize_data();
  svg = document.querySelector("svg");
  pt = svg.createSVGPoint();
});

function hovertext(text, evt){
  var tooltip = document.getElementById("tooltip-text");
  var tooltip_bg = document.getElementById("tooltip-box");    
  var tool_pt = document.getElementById("tooltip-point");
  if (evt === undefined){ 
    tooltip.firstChild.data = ' ';
    tooltip_bg.setAttribute("class","hidden");
    tooltip_bg.setAttribute("x",0);
    tool_pt.setAttribute("class","hidden");
  } else {
    pt = cursorPoint(evt);
    pt.x = Math.round(pt.x);
    pt.y = Math.round(pt.y);
    svgWidth = Number(svg.getAttribute("viewBox").split(" ")[2]);
    tooltip.setAttribute("x",pt.x);
    tooltip.setAttribute("y",pt.y);
    tooltip.firstChild.data = text;
    var length = Math.round(tooltip.getComputedTextLength());
    if (pt.x - length/2 - 6 < 0){
      tooltip.setAttribute("x",length/2+6);
    } else if (pt.x + length/2 + 6 > svgWidth) {
      tooltip.setAttribute("x", svgWidth-length/2-6);
    }
    tool_pt.setAttribute("transform","translate("+pt.x+","+pt.y+")");
    tooltip_bg.setAttribute("x",tooltip.getAttribute("x")-length/2-6);
    tooltip_bg.setAttribute("y",pt.y-35);
    tooltip.setAttribute("class","shown");
    tooltip_bg.setAttribute("class","tooltip-box");
    tool_pt.setAttribute("class","tooltip-box");
    tooltip_bg.setAttribute("width", length+12);
  }
}

function cursorPoint(evt){  

  pt.x = evt.clientX; pt.y = evt.clientY;
  return pt.matrixTransform(svg.getScreenCTM().inverse());
}
function changeOpacity(id, val){
  document.getElementById(id).setAttribute("opacity", val);
}
