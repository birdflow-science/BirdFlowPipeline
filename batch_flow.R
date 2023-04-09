#Sys.setenv(DEBUGME = "batchtools")

library(batchtools)
library(BirdFlowR)
library(dplyr)

# load functions
source('batch_functions.R')

# batch preprocess species

my_dir <- "/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf"
dir.create(my_dir, showWarnings = FALSE)
my_suffix <- 'pp'
gpu_ram <- 8
walltime_min <- 3 # minutes
job_ram <- 4
my_species <- c('Hooded Warbler', 'Cerulean Warbler')

batchMap(fun = BirdFlowR::preprocess_species,
          species = my_species,
          out_dir = my_dir,
          gpu_ram = gpu_ram,
          reg = makeRegistry(paste(make_timestamp(), my_suffix, sep = '_'),
                             conf.file = 'batchtools.conf.R'))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = walltime_min,
                            memory = job_ram))
waitForJobs()
pp_info <- save_preprocessing_info()

# Batch fit models
source('params_mf.R')
params$species <- my_species
params$mem_mf <- gpu_ram

batchMap(fun = fit_model_container,
         args = setup_modelfit_arguments(
           mypy = "/work/pi_drsheldon_umass_edu/birdflow_modeling/birdflow/update_hdf.py",
           mydir = my_dir,
           mysp = pp_info$species,
           myres = pp_info$res,
           params = params,
           pp_info = pp_info),
         reg = makeRegistry(paste0(make_timestamp(), '_mf'), conf.file = 'batchtools.conf.R'))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = params$wt_mf,
                            ngpus = 1,
                            memory = gpu_ram + 1))
waitForJobs()
