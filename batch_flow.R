library(batchtools)
library(BirdFlowR)
library(dplyr)

## Set batch parameters
params <- list()

#### COMMONLY CHANGED PARAMETERS

# species list
params$species <- c('American Crow')

# memory for model in GB to determine preprocess resolution
params$mem_mf <- 2

# modelfit distance weight ### a
params$mf_dist_weight <- 0.005

# modelfit entropy weight ### B
params$mf_ent_weight <- 0.01

# modelfit distance power ### E
params$mf_dist_pow <- 0.5

# preprocess CPU walltime in seconds
params$wt_pp <- 3 * 60

# modelfit GPU walltime in seconds
params$wt_mf <- 10 * 60

# hdf directory
params$dir <- file.path('/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf')
#params$dir <- file.path(getwd(), 'batch_preprocess')
dir.create(params$dir, showWarnings = FALSE)


#### LESS COMMONLY CHANGED PARAMETERS ####

# modelfit observation weight
params$mf_obs_weight <- 20

# modelfit learning rate
params$mf_learning_rate <- 0.1

# modelfit training steps
params$mf_training_steps <- 1500

# modelfit random seed
params$mf_rng_seed <- 17

# preprocess partition
params$part_pp <- 'cpu-preempt,cpu'

# preprocess memory in GB
params$mem_pp <- 4

# modelfit partition
params$part_mf <- 'gpu'

# login node to use via SSH
params$login <- 'login5'

# array jobs
params$array <- TRUE

# preprocess NCPUs
params$ncpu_pp <- 1

# time zone for file naming
params$tz <- "America/Los_Angeles"

# run datetime
params$datetime <- Sys.time()
params$datetime <- `attr<-`(params$datetime,"tzone", params$tz)
params$datetime <- format(params$datetime, "%Y-%m-%d_%H-%M-%S")

# pp registry name
params$pp_reg <- paste0(params$datetime, '_pp')

# mf registry name
params$mf_reg <- paste0(params$datetime, '_mf')

# model fitting .py path
params$mf_script <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/birdflow/update_hdf.py'

#### BATCH PREPROCESS SPECIES ####

# batch preprocess species function

batch_preprocess_species <- function(params = params){
  reg <- makeRegistry(params$pp_reg)
  reg$cluster.functions <- makeClusterFunctionsSlurm(template = 'sbatch_preprocess_species.tmpl',
                                                     array.jobs = params$array,
                                                     nodename = params$login)
  batchMap(fun = BirdFlowR::preprocess_species,
           args = expand.grid(
             species = params$species,
             out_dir = params$dir,
             gpu_ram = params$mem_mf,
             stringsAsFactors = FALSE)
  )
  rez <- list(walltime = params$wt_pp, ncpus = params$ncpu_pp, memory = params$mem_pp * 1000, partition = params$part_pp)
  submitJobs(resources = rez)
  #pp_status <- NA
  #pp_status <- waitForJobs()
  waitForJobs()
}

batch_preprocess_species(params)

save_preprocessing_info <- function(){
  my_result_list <- lapply(seq_len(nrow(getJobPars())), loadResult)
  lst <- list()
  lst$mem <- sapply(getJobPars()$job.pars, function(i){i$gpu_ram})
  lst$species <- sapply(my_result_list, function(i){i$species$species_code})
  lst$res <- sapply(my_result_list, function(i){i$geom$res[1]/1000})
  lst
}

pp_info <- save_preprocessing_info()

# my_memory_vector <- sapply(getJobPars()$job.pars, function(i){i$gpu_ram})
# my_result_list <- lapply(seq_len(nrow(getJobPars())), loadResult)
# my_species_vector <- sapply(my_result_list, function(i){i$species$species_code})
# my_resolution_vector <- sapply(my_result_list, function(i){i$geom$res[1]/1000})
# my_ebirdst_year_vector <- unlist(lapply(my_result_list, function(i){i$metadata$ebird_version_year}))
# 
# my_species_vector
# my_resolution_vector
# my_ebirdst_year_vector


## Model Fitting ##

reg <- reg <- makeRegistry(params$mf_reg)
reg$cluster.functions <- makeClusterFunctionsSlurm(template = 'sbatch_modelfit_container.tmpl', array.jobs = params$array, nodename = params$login)

## Possible way to get around the static resources issue??
# Note that all variables defined in a JobCollection can be used inside the
# template. If you need to pass extra variables, you can set them via the
# argument resources of submitJobs().
# 
# tmp = makeRegistry(file.dir = NA, make.default = FALSE, packages = "methods")
# batchMap(identity, 1:5, reg = tmp)
# 
# # resources are usually set in submitJobs()
# jc = makeJobCollection(1:3, resources = list(foo = "bar"), reg = tmp)
# ls(jc)
# jc$resources

fit_model <- function(mypy, mydir, mysp, myres, mymem){
  # not actually printed currently since not evaluated by R on remote
  # just need all arguments here for batch_map
  print(
    paste(
      mypy, mydir, mysp, myres, mymem
    )
  )
}

fit_model_container <- function(
    mypy,
    mydir,
    mysp,
    myres,
    mymem,
    mf_dist_weight,
    mf_ent_weight,
    mf_dist_pow,
    mf_obs_weight,
    mf_learning_rate,
    mf_training_steps,
    mf_rng_seed
){
  system2('python',
          args = c(
            mypy,
            mydir,
            mysp,
            myres,
            paste0('--dist_weight=', mf_dist_weight),
            paste0('--ent_weight=', mf_ent_weight),
            paste0('--dist_pow=', mf_dist_pow),
            paste0('--obs_weight=', mf_obs_weight),
            paste0('--learning_rate=', mf_learning_rate),
            paste0('--training_steps=', mf_training_steps),
            paste0('--rng_seed=', mf_rng_seed)
          ))
}

# organize grid expansion for arguments...
orig <- data.frame(mypy  = params$mf_script,
                   mydir = params$dir,
                   mysp  = pp_info$species,
                   myres = pp_info$res,
                   mymem = pp_info$mem + 1)
orig$id <- seq_len(nrow(orig))
expanded <- expand.grid(
  id = orig$id,
  mf_dist_weight = params$mf_dist_weight,
  mf_ent_weight = params$mf_ent_weight,
  mf_dist_pow = params$mf_dist_pow,
  mf_obs_weight = params$mf_obs_weight,
  mf_learning_rate = params$mf_learning_rate,
  mf_training_steps = params$mf_training_steps,
  mf_rng_seed = params$mf_rng_seed
)
mf_args <- left_join(orig, expanded, by = 'id')
mf_args$id <- NULL

batchMap(fun = fit_model_container, args = mf_args)

# resources the same across all jobs
static_rez <- list(walltime = params$wt_mf,
                   ncpus = 1,
                   ngpus = 1,
                   partition = params$part_mf,
                   mypy =  params$mf_script,
                   mydir = params$dir)

# Need to loop through here to adjust variable "resources" for each job,
# since species and resolution needs to be a static "resource"
jobinfo <- getJobPars()$job.pars
for (i in seq_along(jobinfo)){
  # variable rez here = Python arguments to be deparsed into sbatch file via brew
  rez       <- static_rez
  rez$mysp  <- jobinfo[[i]]$mysp
  rez$myres <- jobinfo[[i]]$myres
  rez$mymem <- jobinfo[[i]]$mymem
  rez$mf_dist_weight <- jobinfo[[i]]$mf_dist_weight
  rez$mf_ent_weight <- jobinfo[[i]]$mf_ent_weight
  rez$mf_dist_pow <- jobinfo[[i]]$mf_dist_pow
  rez$mf_obs_weight <- jobinfo[[i]]$mf_obs_weight
  rez$mf_learning_rate <- jobinfo[[i]]$mf_learning_rate
  rez$mf_training_steps <- jobinfo[[i]]$mf_training_steps
  rez$mf_rng_seed <- jobinfo[[i]]$mf_rng_seed
  
  stopifnot(all(lapply(rez, length) == 1))
  submitJobs(ids = i, resources = rez)
}
waitForJobs()

# This ends up just expiring, because no batchtools process in remote, so no reporting of results
# waitForJobs()

# Either install R/batchtools on remote or check for output files manually.

