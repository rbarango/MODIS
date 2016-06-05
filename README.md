# MODIS
Functions for processing and extracting the MODIS data products MOD09GQ and MYD09GA. Also for the calculation of vegetation and moisture indices from the MOD09GA dataproduct.

The function *getMOD09GQ* extracts daily Tile Surface Reflectance Bands 1-2 product, spatial resolution: 250m, 
for a particular extent and date interval
https://lpdaac.usgs.gov/products/modis_products_table/mod09gq


The function *getMYD09GA*, extracts daily Tile Surface Reflectance Bands 1-7, spatial resolution: 500/1000m,
for a particular extent and date interval
https://lpdaac.usgs.gov/products/modis_products_table/myd09ga


The function *getVI_MYD09GA* calculates vegetation and moisture indices from 
reflectivity bands 1 to 7 of the MOD09GA dataproduct
