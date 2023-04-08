## Set batch parameters
params <- list()

#### COMMONLY CHANGED PARAMETERS

# species list
params$species <- c('American Crow')

# memory for model in GB to determine preprocess resolution
params$mem_mf <- 2

# modelfit distance weight ### a
## params$mf_dist_weight <- 0.005 ## paper ##
params$mf_dist_weight <- 0.005

# modelfit entropy weight ### B
## params$mf_ent_weight <- seq(from = 0, to = 0.006, by = 0.001) ## paper ##
params$mf_ent_weight <- 0.01

# modelfit distance power ### E
## params$mf_dist_pow <- seq(from = 0.1, to = 1.0, by = 0.1) ## paper ##
params$mf_dist_pow <- 0.5

# modelfit GPU walltime in seconds
params$wt_mf <- 10 * 60

# hdf directory
params$dir <- file.path('/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf')
#params$dir <- file.path(getwd(), 'batch_preprocess')
dir.create(params$dir, showWarnings = FALSE)


#### LESS COMMONLY CHANGED PARAMETERS ####

# modelfit observation weight
params$mf_obs_weight <- 1

# modelfit learning rate
params$mf_learning_rate <- 0.1

# modelfit training steps
params$mf_training_steps <- 1500

# modelfit random seed
params$mf_rng_seed <- 17

# preprocess partition
params$part_pp <- 'cpu-preempt,cpu'

# modelfit partition
params$part_mf <- 'gpu'

# model fitting .py path
params$mf_script <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/birdflow/update_hdf.py'

