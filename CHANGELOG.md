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
- Updated `og:url` and social media image URLs in `dist/index.html` for the new site URL and S3 image hosting
- Rewrote `README.md` to describe the `dist/` site, how to serve it locally, and how the site was originally built
- Updated `code.json` and `CONTRIBUTING.md` to point at the `DOI-USGS` GitHub organization and the new site URL, https://water.usgs.gov/vizlab/water-use-1950-2015

### Fixed

## [1.0.0] - 2016-12-13

### Added
- Initial public release of the U.S. Water Use from 1950-2015 data visualization at https://owi.usgs.gov/vizlab/water-use
