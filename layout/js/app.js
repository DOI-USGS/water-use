var dataPromise = $.Deferred();
var sliderPromise = $.Deferred();

$(document).ready(function () {

	var slider = document.getElementById('slider');

	var range_all_sliders = {
		'min': [1950],
		'7.6923076923%': [1955, 500],
		'15.384615385%': [1960, 1000],
		'23.076923077%': [1965, 1500],
		'30.769230769%': [1970, 2000],
		'38.461538462%': [1975, 2500],
		'46.153846154%': [1980, 500],
		'53.846153846%': [1985, 1000],
		'61.538461538%': [1990, 1500],
		'69.230769321%': [1995, 2000],
		'76.923076923%': [2000, 2500],
		'84.615384615%': [2005, 1500],
		'92.307692308%': [2010, 2000],
		'max': [2015, 2500],
	};

	noUiSlider.create(slider, {
		range: range_all_sliders,
		start: 2010,
		snap: true,
		pips: {
			mode: 'positions',
			behaviour: 'snap',
			values: [0, 7, 15, 23, 30, 38, 46, 53, 61, 69, 76, 84, 92, 100],
			density: 20,
			stepped: true
		}
	});
	sliderPromise.resolve();

});
