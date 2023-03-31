library(batchtools)
library(BirdFlowR)

## Set batch parameters
params <- list()

#### COMMONLY CHANGED PARAMETERS

# species list
params$species <- c('rewbla', 'Hooded Warbler')

# memory for model fit in GB
params$mem_mf <- c(1,2)

# preprocess CPU walltime in seconds
params$wt_pp <- 3 * 60

# hdf directory
params$dir <- file.path(getwd(), 'batch_preprocess')
dir.create(params$dir, showWarnings = FALSE)




#### LESS COMMONLY CHANGED PARAMETERS ####

# preprocess partition
params$part_pp <- 'cpu-preempt'

# preprocess memory in GB
params$mem_pp <- 4

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


#### BATCH PREPROCESS SPECIES ####

# batch preprocess species function

batch_preprocess_species <- function(params = params){
  reg <- makeRegistry(params$pp_reg)
  reg$cluster.functions <- makeClusterFunctionsSlurm(template = 'test_template.tmpl', array.jobs = params$array, nodename = params$login)
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

# my_result_list <- lapply(1:4, loadResult)
# 
# my_species_vector <- unlist(lapply(my_result_list, function(i){i$species$species_code}))
# my_resolution_vector <- unlist(lapply(my_result_list, function(i){i$geom$res[1]/1000}))
# my_ebirdst_year_vector <- unlist(lapply(my_result_list, function(i){i$metadata$ebird_version_year}))
# 
# my_species_vector
# my_resolution_vector
# my_ebirdst_year_vector