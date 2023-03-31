library(batchtools)
library(BirdFlowR)

## Set batch parameters
params <- list()

#### COMMONLY CHANGED PARAMETERS

# species list
params$species <- c('rewbla', 'Hooded Warbler')

# memory for model in GB to determine preprocess resolution
params$mem_mf <- c(1,2)

# preprocess CPU walltime in seconds
params$wt_pp <- 3 * 60

# modelfit GPU walltime in seconds
params$wt_mf <- 10 * 60

# hdf directory
params$dir <- file.path(getwd(), 'batch_preprocess')
dir.create(params$dir, showWarnings = FALSE)




#### LESS COMMONLY CHANGED PARAMETERS ####

# preprocess partition
params$part_pp <- 'cpu-preempt,cpu'

# preprocess memory in GB
params$mem_pp <- 4

# modelfit partition
params$part_mf <- 'gpu-preempt,gpu'

# login node to use via SSH
params$login <- 'login2'

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
reg$cluster.functions <- makeClusterFunctionsSlurm(template = 'modelfit.tmpl', array.jobs = params$array, nodename = params$login)

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

batchMap(fun = fit_model,
         mypy  = params$mf_script,
         mydir = params$dir,
         mysp  = pp_info$species,
         myres = pp_info$res,
         mymem = pp_info$mem + 1)

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
  stopifnot(all(lapply(rez, length) == 1))
  submitJobs(ids = i, resources = rez)
}

# This ends up just expiring, because no batchtools process in remote, so no reporting of results
# waitForJobs()

# Either install R/batchtools on remote or check for output files manually.

