
#' @export
calc_connectivity_metric <- function(birdflow_routes_obj, bf, n_boot=10) {
  # 
  # # # model
  # bf <- BirdFlowR::import_birdflow('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/amewoo/amewoo_150km/amewoo_2023_150km_obs1.0_ent0.000143_dist0.000858_pow0.8.hdf5')
  # # transitions
  # birdflow_routes_obj <- readRDS('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/model_output_hyperparams_distance_metric/amewoo/all_ground_truth_transitions_df.rds')
  # birdflow_routes_obj <- BirdFlowR::Routes(birdflow_routes_obj, species=bf$species, source=bf$source) |>
  #   BirdFlowR::as_BirdFlowRoutes(bf=bf)

  result <- list(
    obs_mc_prebreeding=NA_real_,
    sim_mc_prebreeding=NA_real_,
    sim_mc_rangewide_prebreeding=NA_real_,
    obs_mc_postbreeding=NA_real_,
    sim_mc_postbreeding=NA_real_,
    sim_mc_rangewide_postbreeding=NA_real_
  )
  
  if (is.null(birdflow_routes_obj)) {
    return(result)
  }
  
  if (nrow(birdflow_routes_obj$data)<=10) {
    return(result)
  }

  # Get BF intervals
  prebreeding_origin_target = time_filter(
    birdflow_routes_obj, bf, season = "prebreeding", delta_steps = 2
  )$origin_target |> dplyr::filter(i1!=i2)
  valid_rows <- is_location_valid(bf, x = prebreeding_origin_target$x1 , y = prebreeding_origin_target$y1, 
                                 date = lookup_timestep_sequence(bf, season = "prebreeding")[1])
  prebreeding_origin_target <- prebreeding_origin_target[valid_rows, ] # delete the row not within the dynamic mask
  
  postbreeding_origin_target = time_filter(
    birdflow_routes_obj, bf, season = "postbreeding", delta_steps = 2
    )$origin_target |> dplyr::filter(i1!=i2)
  valid_rows <- is_location_valid(bf, x = postbreeding_origin_target$x1 , y = postbreeding_origin_target$y1, 
                                  date = lookup_timestep_sequence(bf, season = "postbreeding")[1])
  postbreeding_origin_target <- postbreeding_origin_target[valid_rows, ] # delete the row not within the dynamic mask
  
  # Functions for interval mc
  bootstrap_mc <- function(i, origin_target, bf) {
    # Resample origin_target with replacement by route_id
    boot_sample <- origin_target |>
      dplyr::slice_sample(n = nrow(origin_target), replace = TRUE)
    
    # Run the interval calculation on the bootstrap sample
    calc_interval_mc(boot_sample, bf)
  }
  
  #
  get_mean_obs_mc <- function(origin_target, bf, n_boot) {
    boots <- purrr::map(1:n_boot, 
                        bootstrap_mc, 
                        origin_target = origin_target,
                        bf = bf)
    return(mean(unlist(boots)))
  }
  
  #
  get_mean_sim_mc <- function(origin_target, bf, n_boot, season) {
    n_routes_each_time <- 5
    res <- replicate(n_boot, {
      sim_rts <- BirdFlowR::route(bf, n = n_routes_each_time, 
                                  x_coord = origin_target$x1, 
                                  y_coord = origin_target$y1,
                                  season = season, from_marginals = TRUE)
      sim_rts$data <- filter_moving_routes(sim_rts$data, n = n_routes_each_time)
      sim_mc <- calc_route_mc(sim_rts, bf = bf, season = season) 
      return(sim_mc)
    })
    return(mean(res))
  }
  
  if(nrow(prebreeding_origin_target) > 10){
    result['obs_mc_prebreeding'] <- get_mean_obs_mc(prebreeding_origin_target, bf, n_boot)
    result['sim_mc_prebreeding'] <- get_mean_sim_mc(prebreeding_origin_target, bf, n_boot, 'prebreeding')
  }
  result['sim_mc_rangewide_prebreeding'] = calc_birdflow_mc(bf, season = "prebreeding")
  
  if(nrow(postbreeding_origin_target) > 10){
    result['obs_mc_postbreeding'] <- get_mean_obs_mc(postbreeding_origin_target, bf, n_boot)
    result['sim_mc_postbreeding'] <- get_mean_sim_mc(postbreeding_origin_target, bf, n_boot, 'postbreeding')
  }
  result['sim_mc_rangewide_postbreeding'] = calc_birdflow_mc(bf, season = "postbreeding")
  
  return(result)
}

