library(BirdFlowR)
library(terra)
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(batchtools)

## Import functions

source('functions.R')
source('interval_log_likelihood2.R')

dir <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf'

## Batch liklihood calculation using hdf5 directory, filename regex, and job parameters

## Annoying things:
# 1) setting params
# 2) getting species code and resolution from hdf5 folder


batch_likelihood(
  dir = '/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf',
  regex = '^buwtea.*114km_.*\\.hdf5$',
  season = 'postbreeding',
  params)

## Collect results

#loadRegistry()
my_results <- lapply(seq_len(nrow(getJobNames())), loadResult)

my_results[[1]]$ll %>% filter(!is.na(log_likelihood)) %>% nrow

just_ll <- lapply(my_results, function(i){
  data.frame(model = i$model,
             ll = sum(i$ll$log_likelihood, na.rm = TRUE)
             )
  }) %>% rbindlist %>% as_tibble %>% arrange(-ll)

# import_birdflow and sparsify for best model

just_ll$model[1]
bf <- import_birdflow(file.path(dir, just_ll$model[1]))
bf <- sparsify(bf, method = "state")

## check out map

rts <- route_migration(bf, 20, 'prebreeding')
plot(get_coastline(rts$lines))
plot(rts$lines, add = TRUE)
title(main = just_ll$model[1])

## Old code


# dir  <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf'
# file <- 'purfin_2021_89km_obs20.0_ent0.002_dist0.005_pow0.6.hdf5'
# 
# path <- file.path(dir, file)

#do_ll(path, season = 'prebreeding')

# my_colsums <- colSums(my_ll[,c('exclude', 'not_active', 'dynamic_mask', 'sparse', 'same_timestep', 'bad_date')])
# my_colsums
# round(my_colsums / nrow(my_ll), digits = 2)
# nrow(my_ll)

# inspect_flagged_tracks_sf(track_info, my_ll, true_column = 'dynamic_mask')
