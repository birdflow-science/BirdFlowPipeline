
#' Function to plot the americas under different projections
#' 
#' Used to decide on projection for BirdFlowR big runs of north american species
#'
#' @param lon0 The longitude of origin (when `moll = FALSE`)
#' @param lat0 The latitude of origin 
#' @param clip_to_mainland_us if TRUE just plot continental US
#' @param moll If TRUE use the Mollweide projection that was used in the
#' BirdFlow shiny prototype.  `lon0` amd `lat0` will be ignored.
#'
#' @return A ggplot object with the plot
#' @export 
#' @examples
#' plot_americas_leaa(-95, 30)
plot_americas <- function(
    lon0 = -94, lat0 = 0, clip_to_mainland_us = FALSE, moll = FALSE){
  
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
        PARAMETER["False easting",0,
            LENGTHUNIT["metre",1],
            ID["EPSG",8806]],
        PARAMETER["False northing",0,
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
  
  #crs <- birdflow_crs
  
  if(moll){
    crs <- BirdFlowR::birdflow_crs
    title <- "Mollweide lon0 = -90"
    lat0 <- 0
    lon0 <- -90
  } else {
    title <- paste0(
      "Lambert Equal Area Azimuthal lat0 = ", lat0, ", ",
      "lon0 = ", lon0)
  }
  
  americas <- get_americas(clip_to_mainland_us)
  
  sf::sf_use_s2(FALSE)
  leaa <- sf::st_transform(americas, crs = crs)
  
  # origin point
  origin <- sf::st_as_sf(data.frame(x = lon0, y = lat0),
                         coords = c("x", "y"),
                         crs = "EPSG:4326")
  origin2 <-   sf::st_transform(origin, crs = crs)
  coords <- sf::st_coordinates(origin2) |> as.data.frame()
  
  
  plot <- ggplot(leaa) + geom_sf() + ggtitle(title)
  
  if(!clip_to_mainland_us)
    plot <- plot + geom_point(aes(x = X, y = Y), data = coords,
                              inherit.aes = FALSE)
  return(plot)
}


get_americas <- function(clip_to_mainland_us = FALSE, include_hawaii = FALSE){
  earth <- rnaturalearth::ne_countries(scale = 50)
  americas <- earth[grep("America", earth$continent), , drop = FALSE]
  if(clip_to_mainland_us){
    extent <- c(ymax = 50, ymin = 25, xmin = -130, xmax = -55 )
    americas <- sf::st_crop(americas, extent)
    americas <- americas[americas$name == "United States", , drop = FALSE]
  }
  # Drop Hawaii
  if(!include_hawaii){
    clip <- sf::st_bbox(c(ymax = 25, ymin = 15, xmin = -165, xmax = -150 )) |> sf::st_as_sfc()
    sf::st_crs(clip) <- "EPSG:4326"
    americas <- sf::st_difference(americas, clip)
  }
  americas
}