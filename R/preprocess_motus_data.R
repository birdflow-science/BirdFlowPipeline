# pre-processing MOTUS data: split by species
library(dplyr)
# setwd('/home/yc85_illinois_edu/BirdFlowPipeline')
library(devtools)

# Load tag and deployment data
tag_deployment_data <- read.csv(BirdFlowPipeline:::the$motus_raw_path_tag_deployment_data)
recovery_data <- read.csv(BirdFlowPipeline:::the$motus_raw_path_recovery_data)

# Get the species list
unique_species_common_name <- recovery_data$speciesEnglish |> unique() |> na.omit()
eb_taxonomy <- read.csv('data-raw/banding_taxonomy/eBird_Taxonomy_v2021.csv')

# Loop, preprocess, and save to rds
sp_count <- 1
for (sp in unique_species_common_name){
  print(sp_count)
  # get species-specific data
  species_recovery <- recovery_data[recovery_data$speciesEnglish==sp,]
  species_id <- na.omit(species_recovery$speciesID)[1] # Get the only species id
  unique_deployment_id <- species_recovery$tagDeployID |> unique()
  
  route_id <- 1
  all_routes <- list()
  for (deployment_id in unique_deployment_id){
    sub_recovery_info <- species_recovery[species_recovery$tagDeployID==deployment_id,]
    sub_recovery_info <- sub_recovery_info %>% dplyr::filter(!is.na(tsEnd), !is.na(lat), !is.na(lon), !is.na(depLat), !is.na(depLon), !is.na(dt_start))
    
    if (nrow(sub_recovery_info) == 0){
      next
    }
    
    date_ <- as.POSIXct(sub_recovery_info[, 'tsEnd'], origin = "1970-01-01", tz = "UTC")
    date_ <- as.Date(date_)
    lon_ <- c(
      sub_recovery_info[1,'depLon'],
      sub_recovery_info[2:nrow(sub_recovery_info),'lon']
    )
    
    lat_ <- c(
      sub_recovery_info[1,'depLat'],
      sub_recovery_info[2:nrow(sub_recovery_info),'lat']
    )
    
    tagDeployID <- sub_recovery_info[1, 'tagDeployID']
      
    sub_route_df <- data.frame(
      route_id = rep(tagDeployID, length(date_)),
      date = date_,
      lon = lon_,
      lat = lat_,
      route_type = c("motus")
    )
    
    all_routes[[route_id]] <- sub_route_df
    route_id <- route_id + 1
  }
  
  all_routes <- do.call(rbind, all_routes)
  
  ## Not doing this -- Just save the dataframe format
  # species <- list(
  #   species_code = this_species_info$SPECIES_CODE,
  #   scientific_name = this_species_info$SCI_NAME,
  #   common_name = this_species_info$PRIMARY_COM_NAME
  # )
  # metadata <- list()
  # sources <- 'MOTUS'
  # routes_obj <- BirdFlowR::Routes(all_routes, species=species, metadata=metadata, source=sources)
  
  this_species_info <- eb_taxonomy[eb_taxonomy$PRIMARY_COM_NAME==sp,]
  saveRDS(all_routes, file = paste0(the$motus_rds_path, '/', this_species_info$SPECIES_CODE, '.rds'))
  sp_count <- sp_count + 1
}



