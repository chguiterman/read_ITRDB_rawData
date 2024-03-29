
# read_ITRDB_rawData

[![DOI](https://zenodo.org/badge/625965443.svg)](https://zenodo.org/badge/latestdoi/625965443)
[![Hits](https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2Fchguiterman%2Fread_ITRDB_rawData&count_bg=%2379C83D&title_bg=%23555555&icon=&icon_color=%23E7E7E7&title=page+visits&edge_flat=false)](https://hits.seeyoufarm.com)

An R script to import all International Tree-Ring Data Bank (ITRDB) chronology sites and their associated raw data files from the World Data Service for Paleoclimatology at NOAA's National Centers for Environmental Information.

WARNING: This script relies heavily on internet connectivity between your computer and the NOAA NCEI servers. Any lags in quality or speed will make progress difficult. At best, the script takes several minutes to run.

The script executes a search for all tree-ring files on the ITRDB via the WDS-Paleo API, then builds a tidy data.frame of the metadata. It uses this site-level metadata to create a second metadata table for all of the raw measurement files (there could be >1 per site). Based on the raw files metadata, it uses `dplR::read.tuscon()` to import all raw measurement files (.rwl format).

It will return three data objects in your R console:
 * `itrdb_site_meta` Metadata for each site on the ITRDB
 * `itrdb_rawmeas_files` Metadata for each specific raw measurement file on the ITRDB
 * `all_rwl` A nested data.frame including the raw measurement files as read by `dplr::read.tucson()`
 
Users should note that the .rwl files are "nested" into a data.frame. If this object format is unfamiliar, please refer to https://bookdown.org/Maxine/r4ds/nesting.html and other online resources.

Contact me (christopher.guiterman [at] noaa.gov) with any questions for issues.
And, as you would your datasets, and packages, please cite this script if you're using it in your research! 

happy coding!

