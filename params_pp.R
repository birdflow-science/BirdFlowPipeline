## Set batch parameters
params <- list()

#### COMMONLY CHANGED PARAMETERS

# species list
params$species <- c('American Crow')

# memory for model in GB to determine preprocess resolution
params$mem_mf <- 2

# preprocess CPU walltime in seconds
params$wt_pp <- 3 * 60

# hdf directory
params$dir <- file.path('/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf')
#params$dir <- file.path(getwd(), 'batch_preprocess')
dir.create(params$dir, showWarnings = FALSE)


#### LESS COMMONLY CHANGED PARAMETERS ####

# preprocess partition
params$part_pp <- 'cpu-preempt,cpu'

# preprocess memory in GB
params$mem_pp <- 4

# preprocess NCPUs
params$ncpu_pp <- 1