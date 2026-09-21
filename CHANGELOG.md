# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]

### Added
- Added a change log
- Added the deployed static site to `dist/`, as the original vizlab build pipeline can no longer be run

### Changed
- Replaced Google Tag Manager and the legacy USGS analytics script with the Vizlab GA4 tag and the federal DAP tag, matching `vue3-template`; converted the page's `ga()` event calls (which threw `ReferenceError` on the live site) to `gtag()`
- Replaced the "Get the Code / Contact Us" block with the Vizlab pre-footer links (See more visualizations / Get the code)
- Updated the USWDS banner, USGS header, and USGS footer markup and links to match `vue3-template`; upgraded bundled USWDS assets from v2.7 to v3.13 and deferred the USWDS script so it runs after the page body exists
- Restructured the end of the page into stacked Data Collection, References (alphabetical, name-based citations, now including the 2015 circular), and USGS Vizlab authorship sections, matching other Vizlab sites, with published and last-updated dates
- Noted on the page that the 1985–2015 State data came from NWIS, which no longer serves water-use data, and that a data release is forthcoming
- Moved the "In the map below, State size (area) is scaled…" subheading from the top of the page into a caption beneath the figure, in the caption style used on other Vizlab sites, and added the units (mgd) for the map and the bar chart
- Changed the category colors in `map.js` to the mid tones of the USGS water-use category color ramps (from the "Changes in Water Use Categories" page), with the ramps' light tints for the bars of non-selected years in place of 25% opacity
- Added inline color highlights where the category names are first mentioned in the text, tying them back to the map, bars, and legend: the ramp's light tint as the fill with the mid tone as an underline, so dark text meets WCAG AAA contrast
- Added Cee Nell and Althea Archer as contributors for the 2026 update, in the page's authorship section and structured data and in the README citation and contributors
- Switched the page font from Open Sans to Source Sans 3, made headings bold, and enlarged the page title in USGS blue
- Unified layout across the page: one 700px text column, one 800px figure width, consistent heading levels, type scale, paragraph rhythm, and section spacing
- Added the USGS "Changes in Water Use Categories" timeline image beneath the sentence about categories varying through time, with a caption linking to the source page
- Replaced retired `water.usgs.gov/watuse` links with their current `usgs.gov` locations (`50years.html` → Accessing Water-Use Data, `WU-Category-Changes.html` → Changes in Water Use Categories)
- Updated `og:url` and social media image URLs in `dist/index.html` for the new site URL and S3 image hosting
- Rewrote `README.md` to describe the `dist/` site, how to serve it locally, and how the site was originally built
- Updated `code.json` and `CONTRIBUTING.md` to point at the `DOI-USGS` GitHub organization and the new site URL, https://water.usgs.gov/vizlab/water-use-1950-2015

### Fixed
- Fixed the State hover tooltip, which was positioned against the wrong SVG (the padlock icon in the new USWDS banner) and often landed off the map; it now targets the map by id and flips below the cursor near the top edge instead of being clipped
- Fixed a `#nodataBar-201f` selector typo in `main.css` so the 2015 "no data" marker is positioned like the others

## [1.0.0] - 2016-12-13

### Added
- Initial public release of the U.S. Water Use from 1950-2015 data visualization at https://owi.usgs.gov/vizlab/water-use
