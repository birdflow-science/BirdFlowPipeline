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


# season is 'prebreeding', 'postbreeding', 'all'
# intervals outside that season will get an NA for log likelihood
do_ll <- function(path, season){
  bf <- import_birdflow(path)
  bf <- sparsify(bf, method = "state")
  species_code <- BirdFlowR::species_info(bf)$species_code
  banding_df <- readRDS(file.path('rds', paste0(species_code, '.rds')))
  track_info <- make_tracks2(banding_df)
  my_ll <- interval_log_likelihood2(
    intervals = as.data.frame(track_info$int_df),
    observations = as.data.frame(track_info$obs_df),
    bf = bf,
    season = season)
  my_ll
  list(model = basename(path), obs = track_info$obs_df, int = track_info$int_df, ll = as_tibble(my_ll))
}

do_ll(path, season = 'prebreeding')

# my_colsums <- colSums(my_ll[,c('exclude', 'not_active', 'dynamic_mask', 'sparse', 'same_timestep', 'bad_date')])
# my_colsums
# round(my_colsums / nrow(my_ll), digits = 2)
# nrow(my_ll)

# inspect_flagged_tracks_sf(track_info, my_ll, true_column = 'dynamic_mask')


## use 'source' and 'packages' in makeRegistry (or configuration file)
# source a function file

batch_likelihood <- function(dir, regex, params = params){
  files <- list.files(path = dir, pattern = regex, full.names = TRUE)
  # See ?Registry for more info on configuration files, e.g., always loading
  # certain packages or starting in certain working directories
  reg <- makeRegistry(params$pp_reg,
                      packages = c('data.table', 'dplyr', 'tidyr', 'BirdFlowR'),
                      source = c('functions.R', '~/BirdFlowR/R/interval_log_likelihood.R'))
  # saveRegistry()
  # ?setDefaultRegistry
  # not needed because once we make registry, it stays for session as reg
  reg$cluster.functions <- makeClusterFunctionsSlurm(template = '~/batchtools_proj/sbatch_preprocess_species.tmpl',
                                                     array.jobs = params$array,
                                                     nodename = params$login)
  batchMap(fun = do_ll,
           path = files,
           season = 'prebreeding')
  rez <- list(walltime = params$wt_pp, ncpus = params$ncpu_pp, memory = params$mem_pp * 1000, partition = params$part_pp)
  submitJobs(resources = rez)
  waitForJobs()
}


batch_likelihood(
  dir = '/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf',
  regex = '^purfin.*89km_.*\\.hdf5$',
  params)

my_results <- lapply(1:70, loadResult)

just_ll <- sapply(my_results, function(i){mean(i$ll$log_likelihood, na.rm = TRUE)})
