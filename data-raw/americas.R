

#------------------------------------------------------------------------------#
# 01_define_crs_and_clip.R
#
# Produces two files:
#  ./big_run_2/western_hemisphere.rds and 
#  ./big_run_2/americas_crs.wkt 
# to define the clipping polygon and CRS for the runs.
# 
#  Likely it won't need to be run again as those files are part of the
#  repository.
#
#  CRS = coordinate reference system
#  WKT = Well Known Text (a standard for defining CRSs)
# 
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# Setup
#------------------------------------------------------------------------------#
library(ggplot2)
library(sf)
library(rnaturalearth)
source("./data-raw/plot_americas.R")

#------------------------------------------------------------------------------#
# Trial plots to illustrate different options
#------------------------------------------------------------------------------#

if(FALSE) {
plot_americas(-65, 0)
plot_americas(-80, 0)
plot_americas(-85, 0)
plot_americas(-90, 0)
plot_americas(-95, 0)
plot_americas(-85, 10)
plot_americas(-85, 15)
plot_americas(-80, 10)
plot_americas(-85, 19)
plot_americas(-90, 30)
}

#------------------------------------------------------------------------------#
# Create PNGs of options
#------------------------------------------------------------------------------#
if(FALSE) { 
dir <- tempdir()  # png dir
dir.create(dir, showWarnings = FALSE)

# Define helper functions to start a png dev in either portrait or landscape 

# Portrait
pngp <- function(name){
  file <- file.path(dir, paste0(name, ".png"))
  ragg::agg_png(file , width = 6, height = 8, units = "in", res = 150)
}

# Landscape
pngl <- function(name){
  file <- file.path(dir, paste0(name, ".png"))
  ragg::agg_png(file, width = 8, height = 6, units = "in", res = 150)
}

# Make plots with various origins

# Define several North America Focused origin options
lon0s <- c(-95, -85, -80)
lat0s <- c(30, 19, 0)

# Plot
for(i in seq_along(lon0s)){
  lat0 <- lat0s[i]
  lon0 <- lon0s[i]
  
  name <- paste0("leaa", lon0, "_", lat0)
  pngp(name)
  print(plot_americas(lon0, lat0))
  dev.off()
  
  name <- paste0(name, "_usa")
  pngl(name)
  print(plot_americas(lon0, lat0, clip_to_mainland_us = TRUE))
  dev.off()
}

# Mollweide
name <- "moll"
pngl(name)
plot_americas(mol = TRUE)
dev.off()
name <- "moll_usa"
pngl(name)
plot_americas(mol = TRUE, clip_to_mainland_us = TRUE)
dev.off()
} 
#------------------------------------------------------------------------------#
# Set CRS parameters and generate WKT CRS
#------------------------------------------------------------------------------#

# These four parameters are the custom parameters that define our projection
# The first two are the origin.
# The second two are the offset to the false origin in meters -
# they make it so that our entire extent has positive coordinates

lat0 <- 30
lon0 <- -95
false_easting <- 5.5e6
false_northing <- 9.5e6
crs <-
  paste0(
    'PROJCRS["Custom_Lambert_Azimuthal",
    BASEGEOGCRS["WGS 84",
        DATUM["World Geodetic System 1984",
            ELLIPSOID["WGS 84",6378137,298.257223563,
                LENGTHUNIT["metre",1]],
            ID["EPSG",6326]],
        PRIMEM["Greenwich",0,
            ANGLEUNIT["Degree",0.0174532925199433]]],
    CONVERSION["unnamed",
        METHOD["Lambert Azimuthal Equal Area",
            ID["EPSG",9820]],
        PARAMETER["Latitude of natural origin",', lat0, ',
            ANGLEUNIT["Degree",0.0174532925199433],
            ID["EPSG",8801]],
        PARAMETER["Longitude of natural origin",', lon0,',
            ANGLEUNIT["Degree",0.0174532925199433],
            ID["EPSG",8802]],
        PARAMETER["False easting",', false_easting,',
            LENGTHUNIT["metre",1],
            ID["EPSG",8806]],
        PARAMETER["False northing",', false_northing, ',
            LENGTHUNIT["metre",1],
            ID["EPSG",8807]]],
    CS[Cartesian,2],
        AXIS["(E)",east,
            ORDER[1],
            LENGTHUNIT["metre",1,
                ID["EPSG",9001]]],
        AXIS["(N)",north,
            ORDER[2],
            LENGTHUNIT["metre",1,
                ID["EPSG",9001]]]]') |> sf::st_crs()

#------------------------------------------------------------------------------#gi
# Make Clipping polygon
#------------------------------------------------------------------------------#

a <- get_americas(include_hawaii = FALSE) |> sf::st_transform(crs)
buff <- sf::st_union(a) |>  sf::st_buffer(300000) 

orig <- data.frame(x = false_easting, y = false_northing)


if(!all(sf::st_bbox(buff) > 0))
  stop("Buffered extent has negative coordinates. Adjust false easting or northing")


orig <- data.frame(x = false_easting, y = false_northing)




p <- ggplot(buff) + 
  geom_sf() +  
  geom_point(aes(x = x, y = y), data = orig, inherit.aes = FALSE) + 
  geom_sf(data = a, inherit.aes = FALSE) + 
  theme(axis.title = element_blank())

p
#------------------------------------------------------------------------------#
# Save to files
#------------------------------------------------------------------------------#

saveRDS(buff, '~/BirdFlowWork/big_run_2/western_hemisphere.rds')

crs <-sf::st_crs(crs)$wkt
writeLines(crs, "~/BirdFlowWork/big_run_2/americas_crs.wkt")



americas_clip <- buff
americas_crs <- crs


usethis::use_data(americas_clip, overwrite = TRUE)
usethis::use_data(americas_crs, overwrite = TRUE)
