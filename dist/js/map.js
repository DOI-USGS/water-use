var transformData = undefined;
var svg = undefined;
var pt = undefined;
var smoothTransform = undefined;

var category = "Total";
var year = "1950";
var transitionTime = "1s";
// Category color ramps from the USGS "Changes in Water Use Categories"
// palette (https://www.usgs.gov/mission-areas/water-resources/science/changes-water-use-categories).
// Mid tones (3:1 against white) fill the map, the selected year's bar, the
// legend, and the buttons; light tints mark the other years' bars. Keep in
// sync with the .cat-highlight rules in stylesheets/vizlab-template.css.
var colors = {
  "Thermoelectric": "#7297B6",
  "Public_Supply": "#D57C7C",
  "Industrial": "#858E8D",
  "Irrigation": "#B38C00",
  "Total": "#539DD5"
};
var lightColors = {
  "Thermoelectric": "#D0E7F0",
  "Public_Supply": "#EADEDE",
  "Industrial": "#D2D5D4",
  "Irrigation": "#E6DED1",
  "Total": "#D0E6F2"
};

$.when(reallyReadyPromise).then(function(){
  //IE Fix
  var ua = window.navigator.userAgent;
  //IE10 and Below
  var msie = ua.indexOf("MSIE ");
  //IE11
  var trident = ua.indexOf('Trident/');
  //IE Edge
  var edge = ua.indexOf('Edge/');
  if(msie > 0 || trident > 0 || edge > 0){
    smoothTransform = false;
  }else{
    smoothTransform = true;
  }

  get_data();
  // The USWDS banner also contains an inline svg, so select the map by id
  svg = document.getElementById("water-use-svg");
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

  $.when(dataPromise, sliderPromise).then(function(){
    var yr = 1950;
    while (yr <= 2015) {
      set_slider_click(yr);
      yr += 5;
    }
    var slider = document.getElementById('slider');
    slider.noUiSlider.on('update', function( values, handle ) {
      var year = "" + Math.round(values[handle]);
      setYear(year);
    });
    setCategory(category);
  });
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
    var fillCol = color;
    var stateTranT = transitionTime;
    var stroke = "none";
    if (isNaN(scale)){
      fillCol = "url(#nodata)";
      scale = 1;
      stroke = '#f1f1f1';
      stateTranT = "0s";
    }

    var style = {
      "fill": fillCol,
      "transform": "scale3d(" + scale + "," + scale + ",1)",
      "stroke":stroke,
      "transition": "all " + stateTranT + " ease-in-out"
    };
    var state = $("#" + val.state_name);
    if (state !== undefined) {
      if (!smoothTransform){
        var stateDyno = document.getElementById(val.state_name);
        if (stateDyno !== null){
          stateDyno.setAttribute('style', "fill:"+ fillCol +"; stroke:"+stroke+";");
          stateDyno.setAttribute('transform', "scale(" + scale + ")");
        }
      } else {
        state.css(style);
      }
    }
  });
  document.getElementById('category-area-text').firstChild.data = transformData.catVals[category].toLocaleString() + ' million gallons per day (mgd) water withdrawal';
};

var animate_bars = function(data) {

  $.each(data, function(prop, val) {
    var myYear = prop;
    var color = colors[category];

    var scale = val[category][0].barScale;
    var value = val[category][0].value;
    var nodataOp = "0.0";
    var bar = $("#bar-" + myYear);

    if (bar !== undefined) {
      if (isNaN(scale)){
        scale = 0;
        color = 'grey';
      } else if (myYear !== year) {
        color = lightColors[category];
      }
      style = {
        "background": color,
        "transform": "scale3d(1," + scale + ",1)",
        "transform-origin": "100% 100%",
        "transition": "all " + transitionTime + " ease-in-out"
      };
      bar.css(style);
      var nodatabar = $("#nodataBar-" + myYear);

      if(value !== undefined){
        value = value.toLocaleString() + ' mgd';
      } else {
        nodataOp = "1.0";
      }
      style = {
        "transition": "all " + transitionTime + " ease-in-out",
        "opacity": nodataOp
      };
      nodatabar.css(style);
      bar.attr("title", "US: " + value);
      if (myYear === year) {
        var label = $("#bar-label");
        label.text(value);
        label.toggleClass("at-start", myYear === "1950");
        label.toggleClass("at-end", myYear === "2015");
        label.css({
          "left": bar.css("left"),
          "bottom": (isNaN(val[category][0].barScale) ? 0 : val[category][0].barScale * bar[0].offsetHeight) + "px",
          "transition": "all " + transitionTime + " ease-in-out"
        });
      }
    }
  });
  update_bar_tips();
};

var stateHoverDelay = 1000; // ms
var stateHoverTimer = null;
var get_state_value = (function() {
  var prevState = "";
  var prevCat = "";
  var prevYear = "";
  var prevVal = "";
  var sameHover = function(state) {
    return (prevState === state &&
            prevCat === category &&
            prevYear === year);
  }
  return function(state) {
    if (transformData !== undefined && !sameHover(state)) {
        prevState = state;
        prevCat = category;
        prevYear = year;
        if(stateHoverTimer){
          clearTimeout(stateHoverTimer);
        }
        stateHoverTimer = setTimeout(function(){
          //could send cateogory and year here too?
          gtag('event', 'figure', {
            'action': 'Hovered on ' + state
          });
        }, stateHoverDelay);

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
    dataPromise.resolve();
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
  gtag('event', 'figure', {
    'action': 'Category changed to ' + category
  });
};

var setYrTimer = null;
var sendYrDelay = 1000; //ms
var setYear = function(yr) {
  year = yr;
  animate();
  $(".noUi-value").each(function() {
    $(this).toggleClass("is-current", $(this).text() === year);
  });
  if(setYrTimer){
    clearTimeout(setYrTimer);
  }
  setYrTimer = setTimeout(function(){
     gtag('event', 'figure', {
       'action': 'Year changed to ' + year + ' ' + category
     });
  }, sendYrDelay);
 };

var update_bar_tips = function() {
  if ($('.dataBar').tooltip("instance") !== undefined) {
    $('.dataBar').tooltip("destroy");
  }
  $('.dataBar').tooltip({
    position: {
      my: 'center bottom',
      at: 'center top-5'
    }
  });
}

function hovertext(text, evt, stateName){
  var tooltip = document.getElementById("tooltip-text");
  var tooltip_bg = document.getElementById("tooltip-box");
  var tool_pt = document.getElementById("tooltip-point");
  if (evt === undefined){
    tooltip.firstChild.data = ' ';
    tooltip_bg.setAttribute("class","hidden");
    tooltip_bg.setAttribute("x",0);
    tool_pt.setAttribute("class","hidden");
    stateVal = " ";
    if (stateHoverTimer){
      clearTimeout(stateHoverTimer); // stop ga for edge states
    }
  } else {
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
    var viewBox = svg.getAttribute("viewBox").split(" ").map(Number);
    var svgTop = viewBox[1];
    var svgWidth = viewBox[2];
    tooltip.setAttribute("x",pt.x);
    tooltip.firstChild.data = text;
    var length = Math.round(tooltip.getComputedTextLength());
    if (pt.x - length/2 - 6 < 0){
      tooltip.setAttribute("x",length/2+6);
    } else if (pt.x + length/2 + 6 > svgWidth) {
      tooltip.setAttribute("x", svgWidth-length/2-6);
    }
    // Show the tooltip above the cursor, or below it when it would be
    // clipped by the top of the svg (northern states, the legend)
    if (pt.y - 35 >= svgTop) {
      tooltip.setAttribute("y",pt.y);
      tooltip.setAttribute("dy","-1.1em");
      tooltip_bg.setAttribute("y",pt.y-35);
      tool_pt.setAttribute("d","M-6,-12 l6,10 l6,-10");
      tool_pt.setAttribute("clip-path","url(#tipClip)");
    } else {
      tooltip.setAttribute("y",pt.y);
      tooltip.setAttribute("dy","1.85em");
      tooltip_bg.setAttribute("y",pt.y+11);
      tool_pt.setAttribute("d","M-6,12 l6,-10 l6,10");
      tool_pt.setAttribute("clip-path","url(#tipClipBelow)");
    }
    tool_pt.setAttribute("transform","translate("+pt.x+","+pt.y+")");
    tooltip_bg.setAttribute("x",tooltip.getAttribute("x")-length/2-6);
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
