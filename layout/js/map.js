var transformData = undefined;
var svg = undefined;
var pt = undefined;
var stateVal = " ";
var mousedState = " ";
var category = "Total";
var year = "1950";
var transitionTime = "1s";
var colors = {
  "Thermoelectric": "#EFBA5A",
  "Public_Supply": "#857EB0",
  "Industrial": "#A38775",
  "Irrigation": "#61B4A9",
  "Total": "#92C5EA"
};

/* depends on jquery */
var animate_resize_map = function(data) {
  var color = colors[category];
  $.each(data, function(index, val) {
    var scale = Math.sqrt(val.scaleFactor);
    var style = {
      "fill": color,
      "transform": "scale3d(" + scale + "," + scale + ",1)",
      "transition": "all " + transitionTime + " ease-in-out"
    };
    var state = $("#" + val.state_name);
    if (state !== undefined) {
      state.css(style);
    }
  });
};

var animate_bars = function(data) {

  $.each(data, function(prop, val) {
    var myYear = prop;
    var color = colors[category];
    var scale = val[category][0]["barScale"];
    // if we want tooltips
    var value = val[category][0]["value"];
    var style = {
      "background": color,
      "transform": "scale3d(1," + scale + ",1)",
      "transform-origin": "100% 100%",
      "transition": "all " + transitionTime + " ease-in-out"
    };
    var bar = $("#bar-" + myYear);
    if (bar !== undefined) {
      bar.css(style);
      if (myYear !== year) {
        bar.css('opacity','0.25');
      } else {
        bar.css('opacity','1.0');
      }
    }
  });
};


var setStateValue = function(state) {
  $.get( "js/scaleFactors.json", function( data ) {
      var allData = transformData["totState"][year][category];
      for (var i = 0; i < allData.length; i++) {
        if (allData[i]['state_name'] === state){
          stateVal = allData[i]['value'];
          break;
        }
      }
  });
};

var get_resize_data = function() {
  $.get( "js/scaleFactors.json", function( data ) {
    transformData = data;

    animate();

    var slider = document.getElementById('slider');
    slider.noUiSlider.on('update', function( values, handle ) {
  	  var year = "" + Math.round(values[handle]);
    	setYear(year);
    });
  });
};

var animate = function() {
  var statesTransform = transformData["totState"][year][category];
  var barsTransform = transformData["totNat"];
  animate_resize_map(statesTransform);
  animate_bars(barsTransform);
};

var setCategory = function(cat) {
  category = cat;
  $('.cat-button').css("fill-opacity", '0.7');
  $('.cat-button').css("stroke-opacity","0.0");
  $('#' + cat).css("fill-opacity", "0.0");
  $('#' + cat).css("stroke-opacity","1.0");
  animate();
};

var setYear = function(yr) {
  year = yr;
  animate();
};

$(document).ready(function(){
  get_resize_data();
  svg = document.querySelector("svg");
  pt = svg.createSVGPoint();
  setCategory(category);
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
    stateVal = " ";
    mousedState = " ";
  } else {
    var ref = evt.target.getAttribute('xlink:href').split('-')[0];
    var stateName = ref.replace(/#/g, '')
    if (stateName !== mousedState){
      setStateValue(stateName);
    }
    text = text + ': ' + Math.round(stateVal);
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
