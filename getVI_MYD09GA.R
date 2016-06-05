#################################################################
# Function to calculate vegetation and moisture indices from 
# reflectivity bands 1 to 7 of the MOD09GA dataproduct
#
# Parameters:
# band1: a list of reflectivity values from MOD09GA band 1
# band2: a list of reflectivity values from MOD09GA band 2
# band3: a list of reflectivity values from MOD09GA band 3
# band4: a list of reflectivity values from MOD09GA band 4
# band5: a list of reflectivity values from MOD09GA band 5
# band6: a list of reflectivity values from MOD09GA band 6
# band7: a list of reflectivity values from MOD09GA band 7
#
# Returns a data frame with the following indices:
#  NDVI 
#  NDVI.SCALED [0..255]
#  EVI
#  NDWI
#  NDI7
#  SIWSI
#  SWIRR
#  SRWI
#  MSI_1
#  MSI_2
#  GVMI
#
#################################################################

getVI_MYD09GA  <- function(band1, band2, band3, band4, band5, band6, band7){
  
  # NDVI Normalized Difference Vegetation Index
  #
  # The NDVI is a normalized transform of the NIR to red reflectance ratio, 
  # designed to standardize VI values to between -1 and 1
  
  ndvi  <-  (band2 - band1) / (band2 + band1) 
  names(ndvi)  <- "NDVI"
  # Scaled for values [0..255]
  
  ndvi.scaled  <- (ndvi+1)*127
  names(ndvi.scaled)  <- "NDVI.SCALED"
  
  # EVI Enhanced Vegetation Index
  #
  # Whereas the Normalized Difference Vegetation Index (NDVI) is chlorophyll sensitive, 
  # the EVI is more responsive to canopy structural variations, including leaf area index (LAI), 
  # canopy type, plant physiognomy, and canopy architecture. 
  # Very low values of NDVI (0.1 and below) correspond to barren areas of rock, sand, or snow. 
  # Moderate values represent shrub and grassland (0.2 to 0.3), while high values indicate temperate 
  # and tropical rainforests (0.6 to 0.8).
  L  <- 1
  C1 <- 6
  C2 <- 7.5
  G  <-  2.5
  sc  <- 0.0001 #Scale factor
  
  evi  <- G * sc * (band2 - band1) / (band2 * sc+ C1 * band1 * sc -C2 * band3 * sc + L)

  
  # NDWI Normalized Difference Water Index
  # 
  # Gao (1996) proposed that satellites could detect liquid water in vegetation through 
  # the normalized difference water 
  # http://www.gisagmaps.com/vegetation-water-content/
  
  ndwi  <- (band2 - band5)/(band2 + band5)

  
  # NDI7 Normalized Difference Water Index 7
  ndi7  <- (band2 - band7)/(band2 + band7)

  
  # SIWSI Shortwave Infrared Water Stress Index
  # An indicator of canopy water content
  SIWSI  <- (band2 - band6)/(band2 + band6)

  
  # SWIRR 
  SWIRR  <- band6 / band7
  
  # SRWI Shortwave Infrared Water Stress Index
  SRWI  <- band2 /band5
  
  # MSI Moisture Stress Index
  MSI_1  <- band6 /band2
  MSI_2  <- band7 /band2

  # GVMI Global Vegetation Moisture Index
  GVMI  <- ((band2 + 1) - (band6 + 0.02)) / ((band2 + 1) + (band6 + 0.02))
    
  MYD09GA  <-  data.frame(ndvi=ndvi, ndvi.scaled=ndvi.scaled, evi=evi
                     , ndwi=ndwi, SIWSI=SIWSI, SWIRR=SWIRR, SRWI=SRWI
                     , MSI_1=MSI_1,MSI_2=MSI_2
                     , GVMI=GVMI)
  
  names(MYD09GA) <- c("NDVI","NDVI.SCALED","EVI"
                      ,"NDWI", "SIWSI", "SWIRR", "SRWI"
                      ,"MSI_1", "MSI_2"
                      ,"GVMI")
                      
  return(MYD09GA)
}
