#Sys.setenv(DEBUGME = "batchtools")

my_packages <- c('data.table', 'dplyr', 'tidyr', 'BirdFlowR', 'batchtools')
for (i in my_packages){
  library(i, character.only = TRUE)
}

# load functions
source('batch_functions.R')
source('functions.R')

# batch preprocess species

my_dir <- "/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf"
dir.create(my_dir, showWarnings = FALSE)

gpu_ram <- 10
my_species <- c('American Woodcock')

batchMap(fun = BirdFlowR::preprocess_species,
         species = my_species,
         out_dir = my_dir,
         gpu_ram = gpu_ram,
         reg = makeRegistry(paste(make_timestamp(), 'pp', sep = '_'),
                            conf.file = 'batchtools.conf.R',
                            packages = my_packages))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = 3L,
                            memory = 4L))
waitForJobs()
pp_info <- save_preprocessing_info()

# Batch fit models

batchMap(fun = birdflow_modelfit,
         args = birdflow_modelfit_args(
           preprocessed_list = list(
             mydir = my_dir,
             mysp = pp_info$species,
             myres = pp_info$res
           ),
           grid_search_list = list(
             dist_weight = 0.005,
             ent_weight = seq(from = 0, to = 0.006, by = 0.001),
             dist_pow = seq(from = 0.1, to = 1.0, by = 0.1)
           )
         ),
         reg = makeRegistry(paste0(make_timestamp(), '_mf'), conf.file = 'batchtools.conf.R'))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = 10,
                            ngpus = 1,
                            memory = gpu_ram + 1))
waitForJobs()

# Batch likelihoods

files <- list.files(path = my_dir, pattern = '^amewoo.*58km_.*\\.hdf5$', full.names = TRUE)

banding_df <- readRDS(file.path('rds', paste0('amewoo', '.rds')))
track_info <- make_tracks2(banding_df)

batchMap(do_ll_plain,
         files,
         more.args = list(track_info = track_info),
         reg = makeRegistry(paste0(make_timestamp(), '_ll'),
                            conf.file = 'batchtools.conf.R',
                            packages = my_packages,
                            source = 'functions.R'))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = 10,
                            memory = 8))
waitForJobs()

# btmapply version of the above
files <- list.files(path = my_dir, pattern = '^amewoo.*58km_.*\\.hdf5$', full.names = TRUE)
names(files) <- basename(files) %>% sub('\\.hdf5', '', .)
files <- files[1:2]

aa <- btmapply(
  do_ll_plain,
  files,
  more.args = list(track_info = track_info),
  reg = makeRegistry(paste0(make_timestamp(), '_btmapply_ll'),
                     conf.file = 'batchtools.conf.R',
                     packages = my_packages,
                     source = 'functions.R'),
  use.names = TRUE,
  resources = list(walltime = 10,
                   memory = 8),
  n.chunks = 1L
)
  


## from PUFI.R code ##


#loadRegistry()
my_results <- lapply(seq_len(nrow(getJobNames())), loadResult)

my_results[[1]]$ll %>% filter(!is.na(log_likelihood)) %>% nrow

just_ll <- lapply(my_results, function(i){
  data.frame(model = i$model,
             ll = sum(i$ll$log_likelihood, na.rm = TRUE)
  )
}) %>% rbindlist %>% as_tibble %>% arrange(-ll)

# import_birdflow and sparsify for best model

just_ll$model[1]
bf <- import_birdflow(file.path(my_dir, just_ll$model[1]))
bf <- sparsify(bf, method = "state")

## check out map

rts <- route_migration(bf, 10, 'prebreeding')
plot(get_coastline(bf, match_extent = TRUE))
plot(rts$lines, add = TRUE)
title(main = just_ll$model[1])

#readRDS('amwewoo_banding.rds')
