# water-use
## A visualization using USGS water use data.

This repository contains code that fetches, processes, and creates visual representations of water use data.  

The visualization can be found at [https://owi.usgs.gov/vizlab/water-use](https://owi.usgs.gov/vizlab/water-use).  

The visualization is built using the [`vizlab` R package](https://github.com/USGS-VIZLAB/vizlab) which implements a framework for implementation of reproducable data visualizations.   

Data is retrieved using the [`wateRuse`](https://github.com/USGS-R/wateRuse) and [`dataRetrieval`](https://cran.r-project.org/web/packages/dataRetrieval/index.html) packages for pre 1980 and post 1985 data respectively. Pre 1980 data is based on transcribed water use compilation report data. [The reports can be found here.](http://water.usgs.gov/watuse/50years.html) and files containing transcribed data used by the visualization code are [hosted on sciencebase.gov here.](https://www.sciencebase.gov/catalog/item/584f00cee4b0260a373819db)  

Data processing is accomplished with a number of R packages, all available from the [CRAN](https://cran.r-project.org/): `dplyr`, `tidyr`, `sp`, `rgeos`, `maptools`, `maps`, `jsonlite`, `svglite`, and `xml2`.  

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


[
  ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
](http://creativecommons.org/publicdomain/zero/1.0/)
