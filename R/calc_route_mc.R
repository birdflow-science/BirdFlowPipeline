#' Calculate migratory connectivity from BirdFlowR routes.
#'
#' This function calculates migratory connectivity based on sample of routes
#' objects sampled either from a BirdFlowR model or from observational track data.
#'
#' It calculates the migratory connectivity metric MC as defined in Cohen 2018.
#' MC represents an abundance-weighted correlation that is calculated between the origin
#' locations and target locations, taking into account all grid transitions for the
#' specified time period.
#'
#' The route implementation does not correct for spatial sampling inbalances of the
#' provided routes, i.e. the transition matrix is calculated directly from the route
#' transitions.
#'
#' When sampling a high number of routes from a BirdFlow model, the output of
#' [calc_route_mc()] will become asymptotically identical to the output of
#' [calc_birdflow_mc()].
#'
#' @param rts Output from [BirdFlowR::route()]
#' @param bf The BirdFlow model used to make `rts`
#' @param exact logical. Whether to match route time steps exactly to period
#' requested with [BirdFlowR::lookup_timestep_sequence()] (TRUE)
#' or use the route's closest available time steps (FALSE).
#' @param delta_steps The number of time steps (weeks) you allow for the nearest
#' timestep search when exact = FALSE. Default value is 2.
#' @inheritDotParams BirdFlowR::lookup_timestep_sequence -x
#' @return migratory connectivity estimated from the `rts` object.
#' @export
#' @seealso [calc_birdflow_mc()]
#' @examples
#' bf <- BirdFlowModels::amewoo
#' # generate 100 synthetic routes
#' rts <- route(bf, 100, season = "prebreeding")
#' # calculate MC across prebreeding season
#' calc_route_mc(rts, bf, season="prebreeding")
#' # calculate MC across a subset of weeks:
#' calc_route_mc(rts, bf, start=10, end=20)
#' # set exact to false to not enforce exact matches of route timestamps and
#' # requested start and end weeks (in this example end week 30 is after the last
#' # timestamp of the input routes):
#' calc_route_mc(rts, bf, start=10, end=30, exact=FALSE)
#' @references
#' Cohen EB, Hostetler JA, Hallworth MT, Rushing CS, Sillett TS, Marra PP.
#' Quantifying the strength of migratory connectivity.
#' Methods in Ecology and Evolution. 2018 Mar;9(3):513-24.
#' \doi{10.1111/2041-210X.12916}
calc_route_mc <- function(rts, bf, exact=TRUE, delta_steps = 2, ...) {
  # Using MigConnectivity::estPSI treating the routes as tracking data
  stopifnot(is.logical(exact))
  
  ts <- lookup_timestep_sequence(bf, ...)
  origin_t <- ts[1]
  target_t <- ts[length(ts)]

  if(exact){
    origin <- rts$data[rts$data$timestep %in% origin_t, ]
    target <- rts$data[rts$data$timestep %in% target_t,]
  }
  else{
    origin <- time_filter(rts, bf, season = "postbreeding", delta_steps)$origin
    target <- time_filter(rts, bf, season = "postbreeding", delta_steps)$target
  }

  # Initialize origin and target distributions
  origin_abun <- target_abun <- rep(0, n_active(bf))
  # Tally the origin and target cells
  target_table <- table(target$i)
  origin_table <- table(origin$i)
  # Populate origin and target distributions
  target_abun[as.numeric(names(target_table))]=target_table/sum(target_table)
  origin_abun[as.numeric(names(origin_table))]=origin_table/sum(origin_table)

  # Calculate distance matrices
  dist <- great_circle_distances(bf)  # all active cells

  # matrix product to have all combinations for two samples for origin_abun
  # note that this outer product remains normalized, because origin_abun is normalized
  origin_abun_prod <- origin_abun %*% t(origin_abun)
  # mu_D effectively equals weighted.mean(origin_dist, origin_abun_prod)
  mu_D <- sum(dist * origin_abun_prod)
  # SD in origin distance between any two given pixels
  sd_D <- sqrt(sum((dist - mu_D)^2 * origin_abun_prod))

  # same calculation, now for target_abun
  target_abun_prod <- target_abun %*% t(target_abun)
  mu_V <- sum(dist * target_abun_prod)
  sd_V <- sqrt(sum((dist - mu_V)^2 * target_abun_prod))

  # construct transition matrix
  psi <- as.matrix(table(origin$i,target$i))
  # normalize the matrix
  psi <- psi/rowSums(psi)

  origin_abun <- origin_abun[as.numeric(rownames(psi))]
  target_abun <- target_abun[as.numeric(colnames(psi))]

  # multiply transition matrix and relative abundance
  psi_abun <- psi * origin_abun

  # standardizing origin distance matrix
  origin_std <- (dist[as.numeric(rownames(psi)), as.numeric(rownames(psi))] - mu_D) / sd_D
  target_std <- (dist[as.numeric(colnames(psi)), as.numeric(colnames(psi))] - mu_V) / sd_V

  # reduce the distance matrix and abundance vectors
  dist <- dist[as.numeric(colnames(psi)), as.numeric(rownames(psi))]

  dim(psi_abun)
  dim(origin_std)
  dim(target_std)

  # calculate MC
  #MC=sum(t(psi_abun) %*% origin_std %*% psi_abun * target_std)
  MC=sum(t(psi_abun) %*% origin_std %*% psi_abun * target_std)

  return(MC)
}

#' @export
get_circular_seq <- function(from, to, max_timestep = 52) {
  if (from <= to) {
    seq(from, to)
  } else {
    c(seq(from, max_timestep), seq(1, to))
  }
}

#' @export
circular_week_distance <- function(x, target, max_week = 52) {
  pmin(abs(x - target), max_week - abs(x - target))
}

#' @export
time_filter <- function(birdflow_routes_obj, bf, season, delta_steps){

    ts <- BirdFlowR::lookup_timestep_sequence(bf, season)
    origin_t <- ts[1]
    target_t <- ts[length(ts)]
    breeding_ts = BirdFlowR::lookup_timestep_sequence(bf, season = "breeding")[1]
    nonbreeding_ts = tail(BirdFlowR::lookup_timestep_sequence(bf, season = "nonbreeding"),1)
    ids <- unique(birdflow_routes_obj$data$route_id)
    
    # allow a track to start as early as breeding season, and end as late as nonbreeding season
    # but find the nearest to both ends of postbreeding season if the track is long 

    if (season == "postbreeding"){
      start_range = get_circular_seq(breeding_ts[1], origin_t+delta_steps)
      end_range = get_circular_seq(target_t-delta_steps, nonbreeding_ts[length(nonbreeding_ts)])
    }
    if (season == "prebreeding"){
      start_range = get_circular_seq(nonbreeding_ts[1], origin_t+delta_steps)
      end_range = get_circular_seq(target_t-delta_steps, breeding_ts[length(breeding_ts)])
    }
    
    table1 <- birdflow_routes_obj$data |>
      dplyr::distinct(.data[['route_id']], .data[['route_type']]) |>
      dplyr::count(.data[['route_type']], name = "n_routes")
    
    birdflow_routes_filtered <- birdflow_routes_obj$data |>
      dplyr::group_by(.data[['route_id']]) |>
      dplyr::arrange(.data[['date']]) |>
      dplyr::filter(
        any(.data[['timestep']] %in% start_range) &
          any(.data[['timestep']] %in% end_range)
      ) |>
      dplyr::mutate(track_length = dplyr::n()) |>
      dplyr::ungroup()
    
    # trim the track to be shorter (closer to start and end of the postbreeding period)

    origin <- birdflow_routes_filtered |>
      dplyr::group_by(.data[['route_id']]) |>
      dplyr::slice_min(order_by = circular_week_distance(.data[['timestep']], origin_t), n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
    
    target <- birdflow_routes_filtered |>
      dplyr::group_by(.data[['route_id']]) |>
      dplyr::slice_min(order_by = circular_week_distance(.data[['timestep']], target_t), n = 1, with_ties = FALSE) |>
      dplyr::ungroup()
    
    origin_target <- merge(origin, target, by = c("route_id", "route_type"), suffixes = c("1","2"))  |> 
      dplyr::filter(.data[['date2']]>.data[['date1']]) # this ensures the direction aligns with defined season

    print(paste("Original number of tracks:", length(ids), "   ","Tracks remaining:", nrow(origin_target)))
    
    table2 <- origin_target |>
      dplyr::distinct(.data[['route_id']], .data[['route_type']]) |>
      dplyr::count(.data[['route_type']], name = "n_routes") |> 
      dplyr::mutate(season = season)
    
    return(list(
      origin = origin,
      target = target,
      origin_target = origin_target,
      routes_meta_before = table1,
      routes_meta_after = table2
    ))
    
  }
  