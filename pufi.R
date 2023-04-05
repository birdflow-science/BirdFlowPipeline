library(BirdFlowR)
library(terra)
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(batchtools)

source('functions.R')
source('~/BirdFlowR/R/interval_log_likelihood.R')

dir  <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf'
file <- 'purfin_2021_89km_obs20.0_ent0.002_dist0.005_pow0.6.hdf5'

path <- file.path(dir, file)

#do_ll(path, season = 'prebreeding')

# my_colsums <- colSums(my_ll[,c('exclude', 'not_active', 'dynamic_mask', 'sparse', 'same_timestep', 'bad_date')])
# my_colsums
# round(my_colsums / nrow(my_ll), digits = 2)
# nrow(my_ll)

# inspect_flagged_tracks_sf(track_info, my_ll, true_column = 'dynamic_mask')

batch_likelihood(
  dir = '/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf',
  regex = '^purfin.*89km_.*\\.hdf5$',
  params)

my_results <- lapply(1:70, loadResult)

just_ll <- lapply(my_results, function(i){
  data.frame(model = i$model,
             ll = mean(i$ll$log_likelihood, na.rm = TRUE)
             )
  }) %>% rbindlist %>% as_tibble %>% arrange(-ll)
just_ll

