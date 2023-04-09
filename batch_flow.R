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

batchMap(fun = fit_model_container,
         args = setup_modelfit_arguments(
           preprocess_list = list(
             mypy = "/work/pi_drsheldon_umass_edu/birdflow_modeling/birdflow/update_hdf.py",
             mydir = my_dir,
             mysp = pp_info$species,
             myres = pp_info$res
           ),
           grid_search_list = list(
             mf_dist_weight = 0.005,
             mf_ent_weight = seq(from = 0, to = 0.006, by = 0.001),
             mf_dist_pow = 0.5
           )
         ),
         reg = makeRegistry(paste0(make_timestamp(), '_mf'), conf.file = 'batchtools.conf.R'))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = 10,
                            ngpus = 1,
                            memory = gpu_ram + 1))
waitForJobs()
