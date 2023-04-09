#Sys.setenv(DEBUGME = "batchtools")

library(batchtools)
library(BirdFlowR)
library(dplyr)

# load functions
source('batch_functions.R')

# batch preprocess species

my_dir <- "/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf"
my_suffix <- 'pp'
gpu_ram <- 8
walltime_min <- 3 # minutes
job_ram <- 4

batchMap(fun = BirdFlowR::preprocess_species,
          species = c('American Crow', 'Cedar Waxwing', 'Cerulean Warbler'),
          out_dir = my_dir,
          gpu_ram = gpu_ram,
          reg = makeRegistry(paste(make_timestamp(), my_suffix, sep = '_'),
                             conf.file = file.path('conf', 'preprocess_species.batchtools.conf.R')))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = walltime_min,
                            ncpus = 1,
                            memory = job_ram,
                            partition = "cpu-preempt,cpu",
                            chunks.as.arrayjobs = TRUE,
                            max.arrayjobs = 3,
                            measure.memory = TRUE))
waitForJobs()
pp_info <- save_preprocessing_info()

# Batch fit models
source('params_mf.R')
batch_fit_models(params, pp_info)
