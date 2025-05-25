# pre-processing MOTUS data: split by species
# library(dplyr)
# setwd('/home/yc85_illinois_edu/BirdFlowPipeline')
# library(devtools)

process_motus_data <- function(){
  
  # Load tag and deployment data
  tag_deployment_data <- read.csv(the$motus_raw_path_tag_deployment_data) # but not used
  recovery_data <- read.csv(the$motus_raw_path_recovery_data)
  
  # Get the species list
  unique_species_common_name <- recovery_data$speciesEnglish |> 
    unique() |> na.omit()
  eb_taxonomy <- ebirdst::ebirdst_runs
  eb_taxonomy <- eb_taxonomy[eb_taxonomy$species_code!='yebsap-example',]
  
  # Taxonomy crosswalk
  db_namelist <- eb_taxonomy[
    c('species_code', 'scientific_name', 'common_name')
    ] |> unique()
  recovery_data <- recovery_data |> 
    merge(db_namelist[c('species_code', 'common_name')],
          by.x='speciesEnglish', by.y='common_name',all.x=T) |>
    dplyr::rename(species_code1=species_code) |>
    merge(db_namelist[c('species_code', 'scientific_name')], 
          by.x='speciesName', by.y='scientific_name',all.x=T) |>
    dplyr::rename(species_code2=species_code) |> 
    dplyr::filter(!(is.na(species_code1) & is.na(species_code2))) |>
    dplyr::mutate(species_code = dplyr::coalesce(
      .data[['species_code1']], .data[['species_code2']])
      ) |>
    merge(db_namelist[,c('species_code', 'common_name')], 
          by.x='species_code', by.y='species_code', all.x=T)
  
  unique_species_code <- recovery_data$species_code |> unique()
  
  # Loop, preprocess, and save to rds
  sp_count <- 1
  for (sp_code in unique_species_code){
    print(sp_count)
    # get species-specific data
    species_recovery <- recovery_data[recovery_data$species_code==sp_code,]
    species_id <- na.omit(species_recovery$speciesID)[1] # Get the only species id
    unique_deployment_id <- species_recovery$tagDeployID |> unique()
    
    route_id <- 1
    all_routes <- list()
    for (deployment_id in unique_deployment_id){
      sub_recovery_info <- species_recovery[
        species_recovery$tagDeployID==deployment_id,
        ]
      sub_recovery_info <- sub_recovery_info |> 
        dplyr::filter(!is.na(tsEnd), !is.na(lat), 
                      !is.na(lon), !is.na(depLat), !is.na(depLon), 
                      !is.na(dt_start))
      
      if (nrow(sub_recovery_info) == 0){
        next
      }
      
      date_ <- as.POSIXct(sub_recovery_info[, 'tsEnd'], 
                          origin = "1970-01-01", tz = "UTC")
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

    saveRDS(all_routes, file = paste0(the$motus_rds_path, '/', sp_code, '.rds'))
    sp_count <- sp_count + 1
  }
}
  
  
