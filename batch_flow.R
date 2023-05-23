#Sys.setenv(DEBUGME = "batchtools")

my_packages <- c('data.table', 'dplyr', 'tidyr', 'BirdFlowR', 'batchtools', 'rgl', 'trajr', 'ggplot2', 'factoextra', 'gridExtra', 'ggfortify', 'desirability2')
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

my_species <- 'Kentucky Warbler'
gpu_ram <- 10
my_res <- 100
output_nickname <- 'testing_may22'

grid_search_type <- 'new'

grid_search_list <- list(
  c = c(2, 4, 8, 16),
  d = c(0.95, 0.975, 0.99, 0.999, 0.9999),
  dist_pow = seq(from = 0.2, to = 0.8, by = 0.15),
  dist_weight = NA_real_,
  ent_weight = NA_real_
)

# prepare for process_species

pp_dir <- "/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf/_preprocessing"
dir.create(pp_dir, showWarnings = FALSE)
my_species <- ebirdst::get_species(my_species)

# preprocess species

my_res <- preprocess_species_wrapper(
  species = my_species,
  out_dir = pp_dir,
  gpu_ram = gpu_ram,
  res = my_res)

# process output directories

output_fullname <- paste0(ebirdst::get_species(my_species), '_', my_res, 'km', '_', output_nickname)
hdf_dir <- file.path("/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf", output_fullname)
dir.create(hdf_dir, showWarnings = FALSE)
dir.create('output', showWarnings = FALSE)
output_path <- file.path('output', output_fullname)
dir.create(output_path, showWarnings = FALSE)

# move preprocessed file to modelfit directory

preprocessed_file <- list.files(path = pp_dir,
                                pattern = paste0('^', my_species, '.*', my_res, 'km.*\\.hdf5$'),
                                full.names = TRUE)
invisible(file.copy(preprocessed_file, hdf_dir))
if (file.exists(preprocessed_file)) invisible(file.remove(preprocessed_file))

# save objects

saveRDS(grid_search_list, file.path(output_path, 'grid_search_list.rds'))
saveRDS(grid_search_type, file.path(output_path, 'grid_search_type.rds'))
saveRDS(hdf_dir, file.path(output_path, 'hdf_dir.rds'))

# Batch fit models

# delete existing modelfit files matching the output pattern
files <- list.files(path = hdf_dir,
                    pattern = paste0('^', my_species, '.*', my_res, 'km_.*\\.hdf5$'),
                    full.names = TRUE)
#file.remove(files)
batchMap(fun = birdflow_modelfit,
         args = make_birdflow_modelfit_args_df(grid_search_type, grid_search_list),
         reg = makeRegistry(file.path(output_path, paste0(make_timestamp(), '_mf')), conf.file = 'batchtools.conf.R'))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = 15,
                            ngpus = 1,
                            memory = gpu_ram + 1))
waitForJobs()

# Load and save track info

track_info <- make_tracks2(file.path('rds', paste0(my_species, '.rds')))
saveRDS(track_info, file.path(output_path, 'track_info.rds'))

# Batch model evaluation

files <- list.files(path = hdf_dir,
                    pattern = paste0('^', my_species, '.*', my_res, 'km_.*\\.hdf5$'),
                    full.names = TRUE)
batchMap(evaluate_model,
         files,
         more.args = list(track_info = track_info),
         reg = makeRegistry(file.path(output_path, paste0(make_timestamp(), '_ll')),
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

# replace ll and nll with 0 if all NAs
if (all(is.na(ll_df$ll))) {ll_df$ll <- 0}
if (all(is.na(ll_df$nll))) {ll_df$nll <- 0}

# make PCA evaluation plot

model_evaluation_biplot(ll_df, file.path(output_path, 'pca_evaluation.pdf'))

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

# Do desirability rankings

ll_df <- ll_df %>%
  mutate(
    etc_d = d_max(end_traverse_cor, low = 0.9, use_data = TRUE),
    #stc_d = d_max(start_cor, low = 0.9, use_data = TRUE),
    str_d = d_max(straightness, low = 0.5, use_data = TRUE),
    #str_d = d_target(straightness, low = 0.5, target = 0.85, high = 1),
    #cor_d = d_max(mean_distr_cor, high = 1, low = 0.9, scale = exp(-1)),
    #ll_d  = d_max(ll, use_data = TRUE),
    #str_d = d_target(straightness, target = 0.85, low = 0.5, high = 1, scale_low = 1/2, scale_high = 1/2),
    #sin_d = d_max(sinuosity, use_data = TRUE),
    #dsp_d = d_max(displacement, low = 0.75 * max(displacement), high = max(displacement)),
    overall_des = d_overall(across(ends_with("_d")))
  ) %>% arrange(-overall_des)

# save model evaluation RDS
saveRDS(ll_df, file.path(output_path, 'll_df.rds'))

# plot most desirable models

for (i in 1:5){
  pdf(file.path(output_path, paste0('desirability', i, '.pdf')))
  quick_visualize_routes(i)
  dev.off()
}

# graph tradeoff

pdf(file.path(output_path, 'straightness_vs_end_traverse_cor.pdf'))
plot(ll_df$end_traverse_cor, ll_df$straightness, xlab = 'end traverse correlation', ylab = 'route straightness', main = my_species)
dev.off()

# Visualize model with best LL
# 
# quick_visualize_routes(1)
# 
# # graph route migration for all models in parallel
# batchMap(spring_migration_pdf,
#          basename(files),
#          more.args = list(hdf_dir = hdf_dir),
#          reg = makeRegistry(paste0(make_timestamp(), '_pdf'),
#                             conf.file = 'batchtools.conf.R',
#                             packages = my_packages,
#                             source = 'functions.R'))
# submitJobs(mutate(findNotSubmitted(), chunk = 1L),
#            resources = list(walltime = 10,
#                             memory = 8))
# waitForJobs()
