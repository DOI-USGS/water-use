var transformData = undefined;
var svg = undefined;
var pt = undefined;
var smoothTransform = undefined;

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

$(document).ready(function(){
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
  document.getElementById('category-area-text').firstChild.data = transformData.catVals[category].toLocaleString() + ' million gallons per day (Mgal/d) water withdrawal';
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
      }
      style = {
        "background": color,
        "transform": "scale3d(1," + scale + ",1)",
        "transform-origin": "100% 100%",
        "transition": "all " + transitionTime + " ease-in-out"
      };
      bar.css(style);
      if (myYear !== year) {
        bar.css('opacity','0.25');
      } else {
        bar.css('opacity','1.0');
      }
      var nodatabar = $("#nodataBar-" + myYear);

      if(value !== undefined){
        value = value.toLocaleString() + ' Mgal/d';
      } else {
        nodataOp = "1.0";
      }
      style = {
        "transition": "all " + transitionTime + " ease-in-out",
        "opacity": nodataOp
      };
      nodatabar.css(style);
      bar.attr("title", "US: " + value);
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
          ga('send', 'event', 'figure', 'Hovered on ' + state);
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
  ga('send', 'event', 'figure', 'Category changed to ' + category);
};

var setYrTimer = null;
var sendYrDelay = 1000; //ms
var setYear = function(yr) {
  year = yr;
  animate();
  if(setYrTimer){
    clearTimeout(setYrTimer);
  }
  setYrTimer = setTimeout(function(){
     ga('send', 'event', 'figure', 'Year changed to ' + year + ' ' + category);
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
      displayNum = displayNum.toLocaleString() + ' Mgal/d';
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
