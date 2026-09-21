# U.S. Water Use from 1950-2015

> _A newer version of the software may be available. See https://github.com/DOI-USGS/water-use/releases to view all releases._

This repo contains the source for a data visualization website exploring how U.S. water use has changed over time. Every 5 years since 1950, the USGS has compiled and estimated water-use information in cooperation with State, Federal, and local agencies. This site scales each State's area in proportion to its freshwater withdrawals, by total and by category (thermoelectric, public supply, irrigation, and industrial), and lets users drag a timeline across the 14 compilations from 1950 to 2015.

**The data visualization website can be viewed at [https://water.usgs.gov/vizlab/water-use-1950-2015](https://water.usgs.gov/vizlab/water-use-1950-2015).**

The site was originally published on 2016-12-13 at `owi.usgs.gov/vizlab/water-use` (later `labs.waterdata.usgs.gov/visualizations/water-use`) and developed at [github.com/USGS-VIZLAB/water-use](https://github.com/USGS-VIZLAB/water-use), which has since moved to this repository.

## Repository contents

The repository has two parts: the **deployed static site** in `dist/`, and the **original build pipeline** at the root, which is retained for the historical record but can no longer be run (see [How the site was built](#how-the-site-was-built)).

### `dist/` – the deployed website

`dist/` is a snapshot of the site as served in production. It is plain HTML/CSS/JS with no build step:

* `dist/index.html` – the full visualization page, including the inline SVG map of State outlines
* `dist/js/` – application code (`app.js`, `map.js`, `vizlab.js`), the water use scale factors the map animates from (`scaleFactors.json`), and vendored libraries (jQuery, jQuery UI, noUiSlider, USWDS)
* `dist/css/`, `dist/stylesheets/`, `dist/fonts/` – styles and Font Awesome web fonts
* `dist/images/`, `dist/img/` – thumbnails, logos, and USWDS banner icons

Changes to the live site should be made directly to the files in `dist/`.

### Root – the original build pipeline

The remaining top-level files and folders (`viz.yaml`, `scripts/`, `layout/`, `data/`, `images/`) are the R-based pipeline that originally generated the contents of `dist/`. They are kept for provenance and are not maintained.

Note that `main` holds the pipeline as of the site's 2016 launch, which covered 1950–2010. The 2015 compilation was added to the site in June 2018 from the [`newData`](https://github.com/DOI-USGS/water-use/tree/newData) branch, and `dist/` reflects that later build.

## Viewing the website locally

Because the site is plain HTML/CSS/JS, no dependencies need to be installed. Clone the repo and serve the `dist/` directory with any static file server, for example:

```sh
cd dist

# Python 3
python3 -m http.server 8000

# or with Node
npx serve .
```

Then open [http://localhost:8000](http://localhost:8000) in your browser. Opening `index.html` directly from the filesystem will not work, because the scale factor data is loaded via an HTTP request.

## How the site was built

The site was built in 2016 with [vizlab](https://github.com/USGS-VIZLAB/vizlab) (v0.1.5), an R package developed by the USGS Vizlab team that assembled data visualization websites from a `viz.yaml` configuration. The `viz.yaml` at the root of this repo declares the fetch → process → visualize → publish pipeline, with the corresponding R scripts in `scripts/`. Running `vizlab::vizmake()` executed the pipeline, which:

1. **Fetched** State-level water use data: 1985 onward from the USGS National Water Information System via the [`wateRuse`](https://github.com/USGS-R/wateRuse) and [`dataRetrieval`](https://cran.r-project.org/web/packages/dataRetrieval/index.html) packages, and 1950–1980 from data transcribed from the [historical compilation reports](https://www.usgs.gov/mission-areas/water-resources/science/accessing-water-use-data) and [hosted on ScienceBase](https://www.sciencebase.gov/catalog/item/584f00cee4b0260a373819db).
2. **Processed** the data into national totals by category and the per-State scale factors in `scaleFactors.json`, and simplified State boundaries for the map.
3. **Visualized** the results by rendering the State outlines to an inline SVG and assembling the page from the templates in `layout/` and the narrative text in `data/siteText.yaml`.
4. **Published** the assembled site to a `target/` directory, which was then synced to the web server.

The pipeline depended on a pinned set of R packages (including `sp`, `rgeos`, and `maptools`, which have since been retired from CRAN), the vizlab framework (now archived), and internal USGS infrastructure that no longer exists. **It cannot be re-run**, which is why the built output is now committed in `dist/`.

The State-level water use data the pipeline fetched for 1985 onward can also no longer be retrieved with the `wateRuse` and `dataRetrieval` packages, as the USGS water use services those packages queried have been decommissioned. A USGS data release containing the compiled 1950–2015 water use data is forthcoming and will be linked here once it is published.

## Data sources

* Estimated use of water in the United States, various years, 1950–2015: [https://www.usgs.gov/mission-areas/water-resources/science/accessing-water-use-data](https://www.usgs.gov/mission-areas/water-resources/science/accessing-water-use-data)
* Maupin, M.A., Kenny, J.F., Hutson, S.S., Lovelace, J.K., Barber, N.L., and Linsey, K.S., 2014, Estimated use of water in the United States in 2010: U.S. Geological Survey Circular 1405, 56 p. [https://doi.org/10.3133/cir1405](https://doi.org/10.3133/cir1405)
* Dieter, C.A., Maupin, M.A., Caldwell, R.R., Harris, M.A., Ivahnenko, T.I., Lovelace, J.K., Barber, N.L., and Linsey, K.S., 2018, Estimated use of water in the United States in 2015: U.S. Geological Survey Circular 1441, 65 p. [https://doi.org/10.3133/cir1441](https://doi.org/10.3133/cir1441)

## Citation

Appling, A., Blodgett, D., Carr, L., DeCicco, L., Read, E., Read, J., Walker, J., Watkins, D., Wernimont, M., Nell, C., and Archer, A. 2016. U.S. Water Use from 1950-2015. U.S. Geological Survey software release. Reston, VA. https://github.com/DOI-USGS/water-use

## Contributors

Alison Appling, David Blodgett, Lindsay Carr, Laura DeCicco, Emily Read, Jordan Read, Jordan Walker, David Watkins, and Marty Wernimont built the original site in 2016. Cee Nell and Althea Archer updated it in 2026.

## Point of contact

Cee Nell ([cnell@usgs.gov](mailto:cnell@usgs.gov)), USGS Vizlab

## Additional information
* We welcome contributions from the community. See the [guidelines for contributing](CONTRIBUTING.md) to this repository.
* [Disclaimer](DISCLAIMER.md)
* [License](LICENSE.md)
