### ### ### ### ### ### ### ### ### ### ### ###

#Clip data for uas spectral variance
#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#4/6/23


#This script takes in: a polygon, and a folder of .tif or other raster files
#It will then clip the rasters to the polygon

#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

### ### ### ### ### ### ### ### ### ### ### ###

# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
list.of.packages <- c("terra","sf","here","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(tidyverse) #Tidyverse!
library(terra) #New raster data package, documentation pdf here:
#https://cran.r-project.org/web/packages/terra/terra.pdf
#When writing rasters, always specify data type to optimize storage & future computation:
#https://search.r-project.org/CRAN/refmans/raster/html/dataType.html
library(sf) #New vector data package
library(here) #Relative path best practices


## Clean workspace ----
rm(list=ls()) #Ensure empty workspace if running from the beginning
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)


## Set relative directories structure ----

# Set output directory & create if doesn't already exist
outDir <- here::here("data", "orthomosaics_clipped")
if (!dir.exists(outDir)){
  dir.create(outDir)
}


## Load data ----

# Set data directory
toClipDir <- here::here("data", "orthomosaics")

#load polygon
polygon <- sf::st_read(here::here("data", "polygons", "clip_to_general_area", "clip_to_general_area.shp")) 


#Get file names of severity data
toClipPaths <- list.files(path = toClipDir,
                          pattern =".tif$",  # $ = end of string
                          full.names = TRUE)
toClipNms <- list.files(path = toClipDir,
                        pattern =".tif$",  # $ = end of string
                        full.names = FALSE)


### ### ### ### ### ### ### ### ### ### ### ###

# SCRIPTED ANALYSIS ----

#Function to clip a raster to a vector, ensuring in same projection
careful.clip <- function(raster, vector) {
  if (sf::st_crs(vector) != terra::crs(raster)) { #if raster and vector aren't in same projection, change vector to match
    print("Projecting vector")
    vector <- sf::st_transform(vector, terra::crs(raster)) 
  } else {
    print("Vector already in raster CRS")
  }
  print("Clipping")
  r <- terra::crop(raster,
                   vector,
                   mask = TRUE) #crop & mask
  return(r)
}

#Function to run careful.clip on a set of data
clip.export <- function(filePath, fileNm, vector) {
  raster <- terra::rast(filePath) #load raster
  dt <- datatype(raster)[1] #get datatype of raster (the first band), to use in exporting
  rasterClipped <- raster %>% careful.clip(vector) #run function
  nm <- fileNm %>% substr(1, nchar(fileNm)-4) #get root of file name
  newNm <- paste(nm, "_clipped.tif", sep="") #create new name for file with _clipped added
  #Write the new raster!
  terra::writeRaster(rasterClipped,
                     filename = here::here(outDir, newNm),
                     overwrite = TRUE,
                     datatype = dt)
}

#Use mapply (multivariate version of sapply) to map apply over
#toClipNms & toClipPaths. This could be avoided by just applying over
#toClipPaths and doing more string manipulations
mapply(FUN = clip.export, toClipPaths, toClipNms, MoreArgs = list(vector = polygon))


#Done
