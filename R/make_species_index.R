if(FALSE){
hdf_path <- "/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/hdf/Americas/"
res <- 100


make_species_index(hdf_path, res)

index <- readRDS(file.path(hdf_path, "index.rds"))

}



#' Function to make an index file for output of batch_species()
#' 
#' Searches through a directory containing output from `batch_species()` and 
#' create an index file with  columns:
#' "species", "hdf5", "size", "breeding_quality", "nonbreeding_quality", 
#' "postbreeding_migration_quality", "prebreeding_migration_quality", 
#' "min_quality", "xmin", "xmax", "ymin", "ymax")
#'
#' @param hdf_path Path to the directory 
#' @param res Model resolution
#'
#' @return It invisibly returns the index
#' @export
make_species_index <- function(hdf_path, res){
  rhdf5::h5closeAll()
  models <- list.files(hdf_path, pattern = paste0(res, "km$"))
  species <- gsub("_.*$", "", models)
  hdf5s <- file.path(
    hdf_path, 
    paste0(species, "_", res, "km"),  
    paste0(species,"_2022_", res,
           "km_obs1.0_ent0.001924_dist0.008177_pow0.4167.hdf5"))
  
  if (!all(file.exists(hdf5s))) {
    sv <- file.exists(hdf5s)
    dropped <- species[!sv]
    species <- species[sv]
    models <- models[sv]
    hdf5s <- hdf5s[sv]
    warning("hdf directories exist for ", length(dropped), 
            " species that appear to be missing fitted model files")
  }
  
  index <- data.frame(species = species, hdf5 = hdf5s, 
                      size = sapply(hdf5s, file.size))
  rownames(index) <- NULL
  
  # Add season quality info
  get_hdf_species_info <- function(hdf5){
    rhdf5::h5read(hdf5, "species")
  }
  mdl <- lapply(index$hdf5, get_hdf_species_info)
  quality_items <- grep("quality", names(mdl[[1]]), value = TRUE)
  for(i in seq_along(quality_items)){
    item <- quality_items[i]
    index[[item]] <- sapply(mdl,  function(x) x[[item]])
  }
  index$min_quality <- apply(index[, quality_items], 1, min)
  
  # Add limits
  get_hdf_ext <- function(hdf5){
    rhdf5::h5read(hdf5, "geom/ext")
  }
  el <- lapply(index$hdf5, get_hdf_ext)  
  ext <- do.call(rbind, el) |> as.data.frame()
  
  ext_names <- c("xmin", "xmax", "ymin", "ymax")
  names(ext) <- ext_names
  index <- cbind(index, ext)
  
  crs <- rhdf5::h5read(index$hdf5[1], "geom/crs")
  
  extents <- vector(mode = "list", length = nrow(index))
  for(i in seq_len(nrow(index))){
    ext <- as.numeric(index[i, ext_names])
    names(ext) <- ext_names    
    bb <- sf::st_bbox(ext)
    sf::st_crs(bb) <- crs
    poly <- sf::st_as_sfc(bb)
    extents[[i]] <- poly
  }
  e2 <- do.call(c, extents)
  
  if(FALSE){
    # Plot extents as rectangles
    df <- index
    df$geom <- e2
    sf <-   sf::st_as_sf(df, sf_column_name = "geom")
    sf::st_crs(sf) <- crs
    coast <- get_coastline(sf)
    ggplot2::ggplot(sf) + ggplot2::geom_sf(fill = NA, col = rgb(0, 0, 0, .05)) + 
      geom_sf(data = coast, inherit.aes = FALSE)
  }
  
  rhdf5::h5closeAll()
  saveRDS(index, file.path(hdf_path, "index.rds"))
  readr::write_csv(index, file.path(hdf_path, "index.csv"))
  invisible(index)
}