### AMEWOO Tracking ###

# birdflow for reference timesteps
bf <- BirdFlowR::import_birdflow(file.path(hdf_dir, 'amewoo_2021_58km_obs1.0_ent0.00011122_dist0.00088978_pow0.2.hdf5'))


lagtimes = c(1,2,4,8,16)

# Function to generate intervals df from obs df, using a given lag time
find_ts_pairs <- function(bf, obs, ts_diff) {
  output <- data.frame(from = integer(), to = integer())
  for (bird in unique(obs$bird_id)) {
    sub_df <- obs[obs$bird_id == bird, ]
    for (i in seq_len(nrow(sub_df))) {
      # Start timestep
      ts1 <- sub_df$ts[i]
      # End timestep
      ts2 <- lookup_timestep_sequence(bf, start = ts1, n = ts_diff) %>% (utils::tail)(1)
      # Check if there is data matching ts value
      match_rows <- sub_df[sub_df$ts == ts2, ]
      # only use the interval we want, not future years
      if (nrow(match_rows) > 1){
        match_rows <- dplyr::filter(match_rows, date == min(date) & as.numeric(date - sub_df$date[i]) < 365)
      }
      # If there is a match, add a row to the output if date 1 is after date 2
      if (nrow(match_rows) > 0) {
        output <- rbind(output, data.frame(from = sub_df$id[i], to = match_rows$id))
      }
    }
  }
  return(output)
}

# Function to get intervals df for multiple lagtimes
combine_ts_pairs <- function(bf, obs_df, ts_diffs){
  lapply(ts_diffs, find_ts_pairs, obs = obs_df, bf = bf) %>% rbindlist
}

obs_df <- fread('amwo/amewoo_USGS_Movebank_st2019_weekly_tracks-100.csv')
obs_df <- obs_df %>% select(bird_id = indiv_only,
                                      date = timestamp,
                                      lon = lon,
                                      lat = lat)
obs_df$date <- as.Date(obs_df$date)
obs_df <- obs_df %>% arrange(bird_id, date)
obs_df$id <- seq_len(nrow(obs_df))
obs_df$ts <- lookup_timestep(obs_df$date, bf)

int_df <- combine_ts_pairs(bf, obs_df, lagtimes)
obs_df$ts <- NULL

track_info <- list(obs_df = obs_df, int_df = int_df)
