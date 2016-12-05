var transformData = undefined;
var svg = undefined;
var pt = undefined;

var category = "Total";
var year = "1950";
var transitionTime = "1s";
var colors = {
  "Thermoelectric": "#FCBA04",
  "Public_Supply": "#BA3228",
  "Industrial": "#8A716A",
  "Irrigation": "#9BC53D",
  "Total": "#2E86AB"
};

var legendVals = {
  "Thermoelectric": 961,
  "Public_Supply": 175,
  "Industrial": 362,
  "Irrigation": 752,
  "Total": 1332
};

$(document).ready(function(){
  get_data();
  svg = document.querySelector("svg");
  pt = svg.createSVGPoint();
  for (var cat in colors) {
    var catButton = $("#" + cat + '-button');
    catButton.css({'fill': colors[cat]});
  }

  var set_slider_click = function(x) {
    $("#bar-" + x).on("click", function(){
      $("#slider")[0].noUiSlider.set(x);
    });
  };

  var yr = 1950;
  while (yr <= 2015) {
    set_slider_click(yr);
    yr += 5;
  }
});

/* depends on jquery */
var animate_resize_map = function(data) {
  var color = colors[category];
  $("#category-area-legend").css(
    {
    "fill": color,
    "stroke":"none",
    "transition": "all " + transitionTime + " ease-in-out"
  });
  $.each(data, function(index, val) {
    var scale = Math.sqrt(val.scaleFactor);
    var style = {
      "fill": color,
      "transform": "scale3d(" + scale + "," + scale + ",1)",
      "stroke":"none",
      "transition": "all " + transitionTime + " ease-in-out"
    };
    var state = $("#" + val.state_name);
    if (state !== undefined) {
      if (isNaN(scale)){ // doesn't seem to work?
        style = {
          "fill":"url(#nodata)",
          "transform": "scale3d(1,1,1)",
          "stroke":"#f1f1f1",
          "transition": "all 0s"
        };
      }
      state.css(style);
    }
  });
  document.getElementById('category-area-text').firstChild.data = legendVals[category].toLocaleString() + ' mgd water withdrawals';
};

var animate_bars = function(data) {

  $.each(data, function(prop, val) {
    var myYear = prop;
    var color = colors[category];
    var scale = val[category][0]["barScale"];
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
      value = value.toLocaleString() + ' mgd';
      bar.attr("title", value);
    }
  });
  update_bar_tips();
};

var get_state_value = (function() {
  var prevState = "";
  var prevCat = "";
  var prevYear = "";
  var prevVal = "";
  var sameHover = function(state) {
    return (prevState === state &&
            prevCat === category &&
            prevYear == year);
  }
  return function(state) {
    if (transformData !== undefined && !sameHover(state)) {
        prevState = state;
        prevCat = category;
        prevYear = year;

        var stateData = transformData["totState"][year][category];
        prevVal = function(allData) {
          for (var i = 0; i < allData.length; i++) {
            if (allData[i]['state_name'] === state){
              return allData[i]['value'];
            }
          }
        }(stateData);
    }
    return prevVal;
  }
})();

var get_data = function() {
  $.get( "js/scaleFactors.json", function( data ) {
    transformData = data;

    var slider = document.getElementById('slider');
    slider.noUiSlider.on('update', function( values, handle ) {
  	  var year = "" + Math.round(values[handle]);
    	setYear(year);
    });

    setCategory(category);
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

var update_bar_tips = function() {
  $.each($(".dataBar"), function(prop, val){
    $(val).off("mouseenter mouseleave");

    if ($(val).data("tooltipsy") !== undefined) {
      $(val).data("tooltipsy").destroy()
    }
  });
  $('.hastip').tooltipsy({
    delay: 50,
    offset: [0, -10]
  });
}

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
  } else {
    var ref = evt.target.getAttribute('xlink:href').split('-')[0];
    var stateName = ref.replace(/#/g, '');
    var displayNum = Math.round(get_state_value(stateName));
    if (isNaN(displayNum)){
      displayNum = 'no data';
    } else {
      displayNum = displayNum.toLocaleString() + ' mgd';
    }
    text = text + ': ' + displayNum;
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
