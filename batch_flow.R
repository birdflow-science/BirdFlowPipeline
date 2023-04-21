#Sys.setenv(DEBUGME = "batchtools")

my_packages <- c('data.table', 'dplyr', 'tidyr', 'BirdFlowR', 'batchtools', 'rgl')
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
             dist_weight = seq(from = 0.01, to = 0.51, length.out = 4),
             ent_weight = seq(from = 0.01, to = 0.11, length.out = 4),
             dist_pow = seq(from = 0.1, to = 0.7, length.out = 4)
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

hyperparams_ll_df_row <- function(i){
  mn <- i$model
  tibble(
    obs = sub('.*obs(.*?)_.*', '\\1', mn) %>% as.numeric,
    ent = sub('.*ent(.*?)_.*', '\\1', mn) %>% as.numeric,
    dist = sub('.*dist(.*?)_.*', '\\1', mn) %>% as.numeric,
    pow = sub('.*pow(.*?)\\.hdf5', '\\1', mn) %>% as.numeric,
    ll = sum(i$ll$log_likelihood, na.rm = TRUE)
  )
}

ll_df <- lapply(aa, hyperparams_ll_df_row) %>% bind_rows %>% arrange(-ll)
ll_df$color <- hcl.colors(15, rev = TRUE)[cut(ll_df$ll, 15)]

# Plot
plot3d( 
  x = ll_df$ent, y = ll_df$dist, z = ll_df$pow, 
  col = ll_df$color, 
  type = 's', 
  radius = .02,
  xlab="ent", ylab="dist", zlab="pow")

# To display in an R Markdown document:
# rglwidget()

# To save to a file:
htmlwidgets::saveWidget(rglwidget(width = 520, height = 520), 
                        file = "3dscatter.html",
                        libdir = "libs",
                        selfcontained = TRUE
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
