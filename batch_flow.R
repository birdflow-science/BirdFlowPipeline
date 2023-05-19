#Sys.setenv(DEBUGME = "batchtools")

my_packages <- c('data.table', 'dplyr', 'tidyr', 'BirdFlowR', 'batchtools', 'rgl', 'trajr', 'ggplot2', 'factoextra', 'gridExtra', 'ggfortify')
for (i in my_packages){
  suppressWarnings(
    suppressPackageStartupMessages(
      library(i, character.only = TRUE, warn.conflicts = FALSE)
    )
  )
}

# load functions
source('batch_functions.R')
source('functions.R')

# main settings

my_species <- 'American Woodcock'
gpu_ram <- 10
my_res <- 58

grid_search_type <- 'new'

grid_search_list <- list(
  c = c(2, 4, 8, 16),
  d = c(0.95, 0.975, 0.99, 0.999, 0.9999),
  dist_pow = seq(from = 0.2, to = 0.8, by = 0.15),
  dist_weight = NA_real_,
  ent_weight = NA_real_
)

output_folder <- 'amewoo_58km_new_gs_test'



# directory settings

my_dir <- file.path("/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf", output_folder)

# preprocess inputs

dir.create(my_dir, showWarnings = FALSE)
dir.create('output', showWarnings = FALSE)
dir.create(file.path('output', output_folder), showWarnings = FALSE)
my_species <- ebirdst::get_species(my_species)
saveRDS(grid_search_list, file.path('output', output_folder, 'grid_search_list.rds'))
saveRDS(grid_search_type, file.path('output', output_folder, 'grid_search_type.rds'))

# preprocess species

my_res <- preprocess_species_wrapper(
  species = my_species,
  out_dir = my_dir,
  gpu_ram = gpu_ram,
  res = my_res)

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
                    pattern = paste0('^', my_species, '.*', my_res, 'km_.*\\.hdf5$'),
                    full.names = TRUE)
#file.remove(files)
batchMap(fun = birdflow_modelfit,
         args = make_birdflow_modelfit_args_df(grid_search_type, grid_search_list),
         reg = makeRegistry(file.path('output', output_folder, paste0(make_timestamp(), '_mf')), conf.file = 'batchtools.conf.R'))
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

banding_df <- readRDS(file.path('rds', paste0(my_species, '.rds')))
track_info <- make_tracks2(banding_df)
#track_info <- readRDS('track_info_banding_tracking_combined.rds')

# Batch model evaluation

files <- list.files(path = my_dir,
                    pattern = paste0('^', my_species, '.*', my_res, 'km_.*\\.hdf5$'),
                    full.names = TRUE)
batchMap(evaluate_model,
         files,
         more.args = list(track_info = track_info),
         reg = makeRegistry(file.path('output', output_folder, paste0(make_timestamp(), '_ll')),
                            conf.file = 'batchtools.conf.R',
                            packages = my_packages,
                            source = 'functions.R'))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = 15,
                            memory = 8))
waitForJobs()
ll_df <- reduceResultsList() %>%
  lapply(function(i){i$df}) %>%
  rbindlist %>%
  as_tibble %>%
  arrange(-ll) %>%
  mutate(row_no = row_number())

# make PCA evaluation plot

model_evaluation_biplot(ll_df, file.path('output', output_folder, 'pca_evaluation.pdf'))

# Set the plot colors

ll_df$color_ll <- hcl.colors(15, rev = TRUE)[cut(ll_df$ll, 15)]
ll_df <- ll_df %>% mutate(color_nll = if_else(ll < nll, '#ffffff', color_ll))
cor_breaks <- c(-Inf, 0.9, 0.95, 0.975, Inf)
cor_labels <- c("< 0.9", "0.9 to <0.95", "0.95 to <0.975", ">= 0.975")
cor_colors <- c('#FFFFFF', hcl.colors(3, rev = TRUE))
ll_df$color_cor <- cor_colors[cut(ll_df$mean_distr_cor, breaks = cor_breaks)]

# save RDS
saveRDS(ll_df, file.path('output', output_folder, 'll_df.rds'))

# Plot likelihood results cube
make_3d_plot('color_ll', 'll')

# Plot null likelihood cube
make_3d_plot('color_nll', 'nll')

# Plot correlation cube
make_3d_plot('color_cor', 'cor')

# Visualize model with best LL

quick_visualize_routes(1)

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
