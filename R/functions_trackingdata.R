#' @export
is_moving <- function(df) {
  coords <- df[, c("x", "y")]
  !all(apply(coords, 2, function(col) length(unique(round(col, 4))) == 1))
}

#' @export
filter_moving_routes <- function(route_df, n) {
  
  # identify all moving routes
  route_df_moving <- route_df %>%
    dplyr::group_by(route_id) %>%
    dplyr::mutate(moving = is_moving(dplyr::cur_data_all())) %>%
    dplyr::filter(moving)  |> 
    dplyr::ungroup()
  
  # randomly select one moving route_id per original track location
  route_df_select <- route_df_moving %>%
    dplyr::mutate(route_bin = ((route_id - 1) %/% n) + 1) |> 
    dplyr::distinct(route_id, route_bin) %>%
    dplyr::group_by(route_bin) %>%
    dplyr::slice_sample(n = 1) %>%
    dplyr::ungroup()
  
  # extract full trajectories for selected route_id
  final_routes <- dplyr::semi_join(route_df_moving, route_df_select, by = c("route_id"))
  
  return(final_routes)
}

#' @export
st_week_dates <- function(st_year) {
  srd_date_vec <- seq(from = 0, to = 1, length.out = 52 + 1)
  srd_date_vec <- (srd_date_vec[1:52] + srd_date_vec[2:(52 + 
                                                          1)])/2
  srd_date_vec <- round(srd_date_vec, digits = 4)
  year_seq <- 2015
  p_time <- strptime(x = paste(round(srd_date_vec * 366), year_seq), 
                     "%j %Y")
  week.dates <- ymd(paste(paste0(ebirdst_version),
                          formatC(p_time$mon + 1, width = 2, format = "d", flag = "0"), 
                          formatC(p_time$mday, width = 2, format = "d", flag = "0"), 
                          sep = "."))
  
  st_dates <- data.table(doy=yday(week.dates)+0.5,
                         st_week=1:length(week.dates),  # add half a day = noon
                         date=week.dates)
  st_dates$st_doy <- st_dates$doy
  
  return(st_dates)
}

#' @export
filter_argos <- function(data) {
  
  # https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2012.00245.x
  if ('manually-marked-outlier' %in% colnames(data))
    data <- data[is.na(`manually-marked-outlier`) | `manually-marked-outlier`==FALSE]
  
  if ('argos:lc' %in% colnames(data)) {
    start <- nrow(data)
    data <- data[!is.na(`argos:lc`) & nchar(`argos:lc`)>0]
    end <- nrow(data)
    cat("filtered out",start-end,"of",start,"rows that don't have argos:lc data\n")
    
    start <- nrow(data)
    data <- data[
      `argos:lc` %in% c("1","2","3")
    ]
    end <- nrow(data)
    cat("filtered out",start-end,"of",start,"rows that don't have argos:lc==1,2,3\n")
    
  } else {
    cat("WARNING - no argos:lc column")
  }
  
  # if ('argos:iq' %in% colnames(data)) {
  #   data <- data[
  #     nchar(as.character(`argos:iq`))==2 # two digits ensures X>0
  #   ][
  #     substring(as.character(`argos:iq`),2)!="0" # second digit is not zero (Y>0)
  #   ]
  # }
    
  # quality filtering to ensure X and Y error are both < 1500 m
  # browser()
  # data <- data[lc==""] # ??

  
  data <- data[,.(
    timestamp,
    lon=`location-long`,
    lat=`location-lat`,
    tag=`tag-local-identifier`,
    indiv=`individual-local-identifier`
    # iq=`argos:iq`,
    # lc=`argos:lc`
  )]
  
  
  # data[,st_week:=date_to_st_week(date(timestamp))]
  
  
  return(data)
  
}

#' @export
filter_gps <- function(data) {
  
  if ('manually-marked-outlier' %in% colnames(data))
    data <- data[is.na(`manually-marked-outlier`) | `manually-marked-outlier`==FALSE]
  
  data <- data[,.(
    timestamp,
    lon=`location-long`,
    lat=`location-lat`,
    tag=`tag-local-identifier`,
    indiv=`individual-local-identifier`
  )]
  
  return(data)
}

#' @export
filter_geo <- function(data,
                       remove.equinox=FALSE,
                       migration.only=FALSE,
                       max.daily.dist=1500) {
  
  data <- data[,.(
    timestamp,
    lon=`lon`,
    lat=`lat`,
    tag=`tag`,
    indiv=`indiv`
  )]
  
  # remove single dates
  # keep.going <- TRUE
  # while (keep.going) {
  #   data[,dist:=c(NA,head(geosphere::distGeo(cbind(lon,lat))/1e3,-1)),by=tag]
  #   if (max(data$dist,na.rm=T) < max.daily.dist) {
  #     keep.going <- FALSE
  #   }
  #   data <- data[dist<=max.daily.dist]
  # }
  
  # # remove whole track
  #  data[,dist:=c(NA,head(geosphere::distGeo(cbind(lon,lat))/1e3,-1)),by=tag]
  #  data[,max_dist:=max(dist,na.rm=T),by=tag]
  #  data <- data[max_dist<=max.daily.dist]
   
  
  if (migration.only) {
    data <- data[month(timestamp) %in% c(3,4,5,8,9,10,11)]  
  }
  
  
  if (remove.equinox) {
    data <- data[!close_to_equinox(data$timestamp,n.pad = 20)]
  }
  
  return(data)
  
}

#' @export
filter_data <- function(data) {
  
  # remove non-location data
  data <- data[!`sensor-type`=="accessory-measurements"]
  data <- data[!is.na(`location-long`) & !is.na(`location-lat`)]
  
  sensor.types <- unique(data$`sensor-type`)
  stopifnot(all(sensor.types %in% c("gps","argos-doppler-shift","solar-geolocator")))
  
  data.split <- split(data,data$`sensor-type`)
  
  if ("gps" %in% names(data.split)) {
    data.split$gps <- filter_gps(data.split$gps)
  }
  if ("argos-doppler-shift" %in% names(data.split)) {
    data.split$`argos-doppler-shift` <- filter_argos(data.split$`argos-doppler-shift`)
  }
  if ("solar-geolocator" %in% names(data.split)) {
    data.split$`solar-geolocator` <- filter_geo(data.split$`solar-geolocator`)
  }
  
  data.res <- rbindlist(data.split,fill=TRUE)
  
  return(data.res)
  
}

#' @export
join_nearest_st_date <- function(data,st_dates,
                                 nearest=TRUE) {
  # join based on timestamp column
  
  # add day of year column
  data[,doy:=yday(timestamp)+hour(timestamp)/24]
  
  setkey(st_dates,doy)
  setkey(data,doy)
  # rolling join by nearest time
  data <- st_dates[data,roll="nearest"]
  data[,st_diff:=st_doy-doy]
  data[,year:=year(timestamp)]
  
  if (nearest) {
    
    # pick the closest point for each st_week
    data <- data[,.SD[which.min(abs(st_diff))],by=.(indiv,year,st_week)]
    # remove any data points that are more than 4 days away from the st_date
    data <- data[abs(st_diff)<=4]
    
  } else { # average
    # average locations based on st_week
    
    data <- data[,.(
      lon=mean(lon),
      lat=mean(lat),
      timestamp=mean(timestamp)
    ),by=.(indiv,year,st_week)]
    
  }
  
  data <- data[order(timestamp)]
  
  return(data)
  
}

#' @export
map_lat_lon_to_cell <- function(data,rast,zero_index=T) {
  # browser()
  extr <- raster::extract(rast,
                         st_transform(st_as_sf(data[,.(lon,lat)],
                                               coords=c("lon","lat"),
                                               crs="+proj=longlat"),
                                      crs=crs(rast)),cellnumbers=T)
  
  # summary(tmp)
  
  data$cell <- extr[,"cells"]
  data$no_data <- is.na(extr[,2])
  
  data <- cbind(data,rowColFromCell(rast, data$cell))
  data <- cbind(data,xyFromCell(rast, data$cell))
  
  if (zero_index) {
    # convert to zero-indexing for Python
    data[,`:=`(
      cell=cell-1,
      row=row-1,
      col=col-1
    )][]
  }
 
  return(data)
}

#' @export
filter_tracks <- function(data,min_n_weeks=10) {
  
  data[,n_weeks_in_yr:=.N,by=.(year,indiv)]
  
  # hist(data$n_weeks_in_yr)
  # at least 10 weeks in a year
  data <- data[n_weeks_in_yr>=min_n_weeks]
  table(data$indiv)
  length(unique(data$indiv))
  
  data[,indiv_only:=indiv]
  data[,indiv:=paste(indiv_only,year,sep="_")]
  data <- data[order(timestamp)]
  
}

#' @export
transitions_from_tracks <- function(data) {
  df1 <- data[,.(indiv,year,st_week,lon,lat,cell,row,col,x,y,no_data,timestamp)]
  df2 <- copy(df1)[,st_week:=st_week-1]
  
  trans <- merge(df1,df2,by=c("st_week","indiv","year"),suffixes=c('.1','.2'))
  trans[,`:=`(st_week.1=st_week,
              st_week.2=st_week+1)]
  trans[,st_week:=NULL]
  trans[,no_data:=no_data.1|no_data.2]
  trans[,no_data.1:=NULL]
  trans[,no_data.2:=NULL]
  
  # calculate distance moved
  trans[,distance_moved_km:=geosphere::distGeo(p1=cbind(lon.1,lat.1),
                                               p2=cbind(lon.2,lat.2))/1e3]
  
  trans <- trans[order(indiv,timestamp.1)]
  
  return(trans)
  
}

##### plots #####

# base map

#' @export
make_base_map <- function(data) {
  the.map <- map_data("world")
  xlim.plot <- range(data$lon,na.rm=T)
  ylim.plot <- range(data$lat,na.rm=T)
  
  base <- ggplot() +
    theme_bw() +
    theme(panel.grid.major = element_blank()) +
    geom_polygon(data=the.map %>% filter(long>-170 & long < 0),
                 aes(long,lat,group=group),
                 # fill=NA,color=gray(.6)) +
                 fill=gray(.95),color=gray(.8),size=.3) +
    coord_map("mercator",#lat0=45,
              # xlim=c(-10,30),ylim=c(40,60)
              xlim=xlim.plot,
              ylim=ylim.plot
    ) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme(panel.background = element_rect(fill=gray(.8)))
  
  return(base)
}

#' @export
plot_latlon_scatter <- function(data) {
  data %>% 
    pivot_longer(c(lon,lat)) %>%
    ggplot(aes(x=timestamp,
               y=value,
               color=indiv)) +
    facet_wrap(~name, scales="free") +
    geom_point() +
    geom_line() +
    guides(color="none")
}

#' @export
plot_map_tracks <- function(data,base.map=base) {

  p <- base.map +
    geom_point(data=data,
               aes(x=lon,y=lat,color=indiv)) +
    geom_path(data=data[order(timestamp)],
              aes(x=lon,y=lat,color=indiv,group=indiv)) +
    guides(color="none")
  if ("no_data" %in% colnames(data)) {
    p <- p + geom_point(data=data[no_data==TRUE],
               aes(x=lon,y=lat),pch=13,color="black")
  }
  return(p)
   
  
}

#' @export
close_to_equinox <- function(datetime,n.pad=14) {
  datetime <- as_datetime(datetime)
  spr.eq <- fmdates:::equinox(year(datetime),season="mar",tz="UTC")
  fall.eq <- fmdates:::equinox(year(datetime),season="sep",tz="UTC")
  
  is.near.eq <- abs(as.numeric(difftime(datetime,spr.eq,tz="UTC",units="days")))<n.pad | 
    abs(as.numeric(difftime(datetime,fall.eq,tz="UTC",units="days")))<n.pad
  
  return(is.near.eq)
  
}

#' @export
preprocess_data <- function(data,source) {
  if (source=="movebank") {
    # do nothing - default
    return(data)
  } else if (source=="custom_stanley") {
    return(convert_stanley(data))
  } else {
    stop(paste("data source",source,"not recognized"))
  }
}

#' @export
convert_stanley <- function(data) {
  # browser()
  
  head(data)
  
  # process by UTM zone
  data.zone <- split(data,data$utmZone)
  
  library(foreach)
  library(sf)
  
  
  st.zone <- foreach(ddf=data.zone) %do% {
    the.zone <- unique(ddf$utmZone)
    
    st <- st_as_sf(ddf,coords = c("utmEasting","utmNorthing"),
             crs=paste0('+proj=utm +zone=',gsub("[[:alpha:]]","",the.zone),
                       ' +ellps=WGS84 +units=m +no_defs +type=crs'))
    return(st)
  }
  
  # st.zone[[3]] %>% filter(`event-id`==2316175613)
  
  
  st.latlon <- lapply(st.zone,st_transform,crs="+proj=longlat +datum=WGS84")
  st.latlon <- do.call(rbind,st.latlon)
  
  # st.latlon %>% filter(`event-id`==2316175613)
  
  res <- st.latlon %>% transmute(
    `sensor-type`="gps",
    timestamp=mdy_hm(timestamps),
    `location-long`=st_coordinates(.)[,1],
    `location-lat`=st_coordinates(.)[,2],
    `tag-local-identifier`=tagNo,
    `individual-local-identifier`=tagNo
  ) %>% st_drop_geometry
  
  return(res)

}

#' @export
raster2df <- function(rast) {
  rast_spdf <- as(rast,"SpatialPixelsDataFrame")
  rast_df <- as.data.frame(rast_spdf)
  colnames(rast_df) <- c("value", "x", "y")
  return(rast_df)
}

#' @export
raster_from_kweeks_prediction <- function(kweeks_matrix,template_raster,
                                          set_zero_to_na=TRUE,
                                          zero.epsilon=1e-3) {
  heatmp <- as.matrix(kweeks_matrix)
  heatmp.rast <- raster(heatmp,crs=template_raster@crs)
  extent(heatmp.rast) <- extent(template_raster)
  if (set_zero_to_na) {
    heatmp.rast[heatmp.rast<zero.epsilon] <- 0
    heatmp.rast <- reclassify(heatmp.rast,cbind(0,NA))
  }
  return(heatmp.rast)
}

#' @export
assign_timestep <- function(timestamps, dates_df) {
  stopifnot(inherits(bf, "BirdFlow"))
  dates <- bf$dates[1:52,]
  # Extract Julian day from timestamps
  julian_days <- as.numeric(format(as.POSIXct(timestamps, tz = "UTC"), "%j"))
  
  # Pre-allocate output
  assigned_timesteps <- integer(length(julian_days))
  
  for (i in seq_along(julian_days)) {
    jday <- julian_days[i]
    
    # Find the latest timestep with julian <= jday
    match_row <- dates |>
      dplyr::filter(julian <= jday) |>
      dplyr::slice_max(julian, n = 1)
    
    if (nrow(match_row) == 1) {
      assigned_timesteps[i] <- match_row$timestep
    } else {
      # If jday < min(dates$julian), wrap around to last timestep
      assigned_timesteps[i] <- max(dates$timestep)
    }
  }
  
  return(assigned_timesteps)
}
