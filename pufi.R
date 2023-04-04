library(BirdFlowR)
library(terra)
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(batchtools)
library(parallel)

source('functions.R')

dir  <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf'
file <- 'cintea_2021_92km_obs20.0_ent0.0_dist0.005_pow0.3.hdf5'

path <- file.path(dir, file)
bf <- import_birdflow(path)
bf <- sparsify(bf, method = "state")

plot(rast(bf, 1))

file <- 'rds/cintea.rds'
df <- readRDS(file)
species_code <- basename(file) %>% sub('\\.rds$', '', .)
tax_join <- fread(file.path('tax', 'eBird_Taxonomy_v2021.csv')) %>% select(SPECIES_CODE, PRIMARY_COM_NAME)
species_name <- tax_join[SPECIES_CODE == species_code,]$`PRIMARY_COM_NAME`


track_info <- make_tracks2(df)
my_ll <- BirdFlowR::interval_log_likelihood(
  intervals = as.data.frame(track_info$int_df),
  observations = as.data.frame(track_info$obs_df),
  bf = bf)
my_colsums <- colSums(my_ll[,c('exclude', 'not_active', 'dynamic_mask', 'sparse', 'same_timestep', 'bad_date')])
my_colsums
round(my_colsums / nrow(my_ll), digits = 2)
nrow(my_ll)

my_ll <- as_tibble(my_ll)
left_join(my_ll, track_info$obs_df, by = 'BAND_TRACK') %>%
  filter(dynamic_mask)

inspect_flagged_tracks_sf(track_info, my_ll, true_column = 'dynamic_mask')

files <- list.files('/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf', pattern = '^purfin.*89km_.*\\.hdf5$', full.names = TRUE)

get_cite_likelihood <- function(file){
  model_name <- basename(file)
  bf <- BirdFlowR::import_birdflow(file)
  bf <- BirdFlowR::sparsify(bf, method = "state")
  
  file <- 'rds/purfin.rds'
  df <- readRDS(file)
  species_code <- basename(file) %>% sub('\\.rds$', '', .)
  tax_join <- fread(file.path('tax', 'eBird_Taxonomy_v2021.csv')) %>% select(SPECIES_CODE, PRIMARY_COM_NAME)
  species_name <- tax_join[SPECIES_CODE == species_code,]$`PRIMARY_COM_NAME`
  
  track_info <- make_tracks2(df)
  my_ll <- BirdFlowR::interval_log_likelihood(
    intervals = as.data.frame(track_info$int_df),
    observations = as.data.frame(track_info$obs_df),
    bf = bf)
  mll <- mean(my_ll$log_likelihood, na.rm = TRUE)
  names(mll) <- model_name
  mll
}

## use 'source' and 'packages' in makeRegistry (or configuration file)
# source a function file

batch_cite_likelihood <- function(params = params){
  # See ?Registry for more info on configuration files, e.g., always loading
  # certain packages or starting in certain working directories
  reg <- makeRegistry(params$pp_reg,
                      packages = c('data.table', 'dplyr', 'tidyr'),
                      source = 'functions.R')
  # saveRegistry()
  # ?setDefaultRegistry
  # not needed because once we make registry, it stays for session as reg
  reg$cluster.functions <- makeClusterFunctionsSlurm(template = '~/batchtools_proj/sbatch_preprocess_species.tmpl',
                                                     array.jobs = params$array,
                                                     nodename = params$login)
  batchMap(fun = get_cite_likelihood,
           file = files
  )
  rez <- list(walltime = params$wt_pp, ncpus = params$ncpu_pp, memory = params$mem_pp * 1000, partition = params$part_pp)
  submitJobs(resources = rez)
  waitForJobs()
}

batch_cite_likelihood(params)
my_result <- sapply(1:70, loadResult)
aa <- data.frame(model = names(my_result), ll = unname(my_result))
aa <- aa %>% arrange(-ll)
saveRDS(aa, 'pufi.rds')

bf <- import_birdflow('/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf/purfin_2021_89km_obs20.0_ent0.002_dist0.005_pow0.6.hdf5')
