#Sys.setenv(DEBUGME = "batchtools")

my_packages <- c('data.table', 'dplyr', 'tidyr', 'BirdFlowR', 'batchtools', 'rgl')
for (i in my_packages){
  library(i, character.only = TRUE)
}

# load functions
source('batch_functions.R')
source('functions.R')

# directory settings

my_dir <- "/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf"
dir.create(my_dir, showWarnings = FALSE)

# main arguments

my_species <- c('American Woodcock')

gpu_ram <- 10

res <- 58
# res <- NULL

grid_search_list <- list(
  dist_weight = seq(from = 0.008, to = 0.018, length.out = 6),
  ent_weight = seq(from = 0.0015, to = 0.004, length.out = 6),
  dist_pow = seq(from = 0.1, to = .9, length.out = 5)
)

# preprocess species

pp_info <- preprocess_species_wrapper(
  species = my_species,
  out_dir = my_dir,
  gpu_ram = gpu_ram,
  res = res) %>% list %>% rbindlist %>% as.data.frame
# pipeline currently assumes just 1 species at a time, take only 1st row
pp_info <- pp_info[1,]

# batch preprocess species

# batchMap(fun = preprocess_species_wrapper,
#          species = my_species,
#          out_dir = my_dir,
#          gpu_ram = gpu_ram,
#          res = res,
#          reg = makeRegistry(paste(make_timestamp(), 'pp', sep = '_'),
#                             conf.file = 'batchtools.conf.R',
#                             packages = my_packages))
# submitJobs(mutate(findNotSubmitted(), chunk = 1L),
#            resources = list(walltime = 3L,
#                             memory = 4L))
# waitForJobs()
# pp_info <- reduceResultsList() |> rbindlist() |> as.data.frame()
# # pipeline currently assumes just 1 species at a time, take only 1st row
# pp_info <- pp_info[1,]

# Batch fit models

# delete existing modelfit files matching the output pattern
files <- list.files(path = my_dir,
                    pattern = paste0('^', pp_info$species, '.*', pp_info$res, 'km_.*\\.hdf5$'),
                    full.names = TRUE)
#file.remove(files)
batchMap(fun = birdflow_modelfit,
         args = birdflow_modelfit_args(
           preprocessed_list = list(
             mydir = my_dir,
             mysp = pp_info$species,
             myres = pp_info$res),
           grid_search_list = grid_search_list),
         reg = makeRegistry(paste0(make_timestamp(), '_mf'), conf.file = 'batchtools.conf.R'))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = 10,
                            ngpus = 1,
                            memory = gpu_ram + 1))
waitForJobs()

# Load track info

# Build joint tracking-banding datafile
#
# n_banding <- nrow(track_info_banding$obs_df)
# track_info_tracking$obs_df$id <- track_info_tracking$obs_df$id + n_banding
# track_info_tracking$int_df$from <- track_info_tracking$int_df$from + n_banding
# track_info_tracking$int_df$to <- track_info_tracking$int_df$to + n_banding
# all_obs <- bind_rows(track_info_banding$obs_df, track_info_tracking$obs_df)
# all_int <- bind_rows(track_info_banding$int_df, track_info_tracking$int_df)
# track_info <- list(
#   obs_df = all_obs,
#   int_df = all_int
# )

#banding_df <- readRDS(file.path('rds', paste0(pp_info$species, '.rds')))
#track_info <- make_tracks2(banding_df)
track_info <- readRDS('track_info_banding_tracking_combined.rds')

# Batch likelihoods

files <- list.files(path = my_dir,
                    pattern = paste0('^', pp_info$species, '.*', pp_info$res, 'km_.*\\.hdf5$'),
                    full.names = TRUE)
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
ll_results <- reduceResultsList()
ll_df <- lapply(ll_results, hyperparams_ll_df_row) %>% bind_rows %>% arrange(-ll)

# Set the plot colors

ll_df$color_ll <- hcl.colors(15, rev = TRUE)[cut(ll_df$ll, 15)]
ll_df <- ll_df %>% mutate(color_nll = if_else(ll < nll, '#ffffff', color_ll))
cor_breaks <- c(-Inf, 0.9, 0.95, 0.975, Inf)
cor_labels <- c("< 0.9", "0.9 to <0.95", "0.95 to <0.975", ">= 0.975")
cor_colors <- c('#FFFFFF', hcl.colors(3, rev = TRUE))
ll_df$color_cor <- cor_colors[cut(ll_df$mean_distr_cor, breaks = cor_breaks)]

# Plot likelihood results cube
make_3d_plot('color_ll', 'll')

# Plot null likelihood cube
make_3d_plot('color_nll', 'nll')

# Plot correlation cube
make_3d_plot('color_cor', 'cor')

# Visualize model with best LL

i <- 1

ll_df$model[i]
bf <- import_birdflow(file.path(my_dir, ll_df$model[i]))
# 
# ## Plot map route_migration spring msap
# 
rts <- route_migration(bf, 10, 'prebreeding')
plot(get_coastline(bf))
plot(rts$lines, add = TRUE)
title(main = ll_df$model[i])

# graph route migration for all models in parallel

batchMap(spring_migration_pdf,
         basename(files),
         more.args = list(my_dir = my_dir),
         reg = makeRegistry(paste0(make_timestamp(), '_pdf'),
                            conf.file = 'batchtools.conf.R',
                            packages = my_packages,
                            source = 'functions.R'))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = 10,
                            memory = 8))
waitForJobs()


### make coded list by drawing plots and waiting to user input

ll_df$visual <- NA_character_
for (i in 40:nrow(ll_df)){
  mname <- ll_df[i, 'model']
  bf <- import_birdflow(file.path(my_dir, mname))
  rts <- route_migration(bf, 10, 'prebreeding')
  plot(get_coastline(bf))
  plot(rts$lines, add = TRUE)
  my_input <- readline('Enter code: ')
  while (my_input == 'redo'){
    dev.off()
    rts <- route_migration(bf, 10, 'prebreeding')
    plot(get_coastline(bf))
    plot(rts$lines, add = TRUE)
    my_input <- readline('Enter code: ')
  }
  ll_df$visual[i] <- my_input
  dev.off()
}
