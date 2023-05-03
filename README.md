# water-use
## A visualization using USGS water use data.

This repository contains code that fetches, processes, and creates visual representations of water use data.  

The visualization can be found at [https://owi.usgs.gov/vizlab/water-use](https://owi.usgs.gov/vizlab/water-use).  

The visualization is built using the [`vizlab` R package](https://github.com/USGS-VIZLAB/vizlab) which implements a framework for implementation of reproducable data visualizations.   

Data is retrieved using the [`wateRuse`](https://github.com/USGS-R/wateRuse) and [`dataRetrieval`](https://cran.r-project.org/web/packages/dataRetrieval/index.html) packages for pre 1980 and post 1985 data respectively. Pre 1980 data is based on transcribed water use compilation report data. [The reports can be found here.](http://water.usgs.gov/watuse/50years.html) and files containing transcribed data used by the visualization code are [hosted on sciencebase.gov here.](https://www.sciencebase.gov/catalog/item/584f00cee4b0260a373819db)  

Data processing is accomplished with a number of R packages, all available from the [CRAN](https://cran.r-project.org/): `dplyr`, `tidyr`, `sp`, `rgeos`, `maptools`, `maps`, `jsonlite`, `svglite`, and `xml2`.  
