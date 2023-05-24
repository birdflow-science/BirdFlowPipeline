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
source(file.path('R', 'batch_functions.R'))
source(file.path('R', 'functions.R'))

params <- list(
  my_species = "Philadelphia Vireo",
  gpu_ram = 10,
  my_res = 100,
  output_nickname = as.character(Sys.Date()),
  grid_search_type = 'new',
  grid_search_list = list(
    c = c(2, 4, 8, 16),
    d = c(0.95, 0.975, 0.99, 0.999, 0.9999),
    dist_pow = seq(from = 0.2, to = 0.8, by = 0.15),
    dist_weight = NA_real_,
    ent_weight = NA_real_)
  )

# preprocess species and set up directories

params <- preprocess_species_wrapper(params)

# Batch fit models

batch_modelfit_wrapper(params)

# Load and save track info

track_info <- make_tracks2(file.path('rds', paste0(params$my_species, '.rds')))
saveRDS(track_info, file.path(params$output_path, 'track_info.rds'))

# Batch model evaluation

ll_df <- batch_evaluate_models(params, track_info)

## Plotting

# make PCA evaluation plot

model_evaluation_biplot(ll_df, params)

# Plot likelihood results cube
make_3d_plot('color_ll', 'll', ll_df, params)
# Plot null likelihood cube
make_3d_plot('color_nll', 'nll', ll_df, params)
# Plot correlation cube
make_3d_plot('color_cor', 'cor', ll_df, params)

# Do desirability rankings

ll_df <- ll_df %>%
  # remove any existing desirability columns (for interactive scripting)
  select(-ends_with("_d")) %>%
  # create new desirability columns
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
saveRDS(ll_df, file.path(params$output_path, 'll_df.rds'))

# plot most desirable models

for (i in 1:5){
  pdf(file.path(params$output_path, paste0('desirability', i, '.pdf')))
  quick_visualize_routes(i, df = ll_df, dir = params$hdf_dir)
  dev.off()
}

# graph tradeoff

pdf(file.path(params$output_path, 'straightness_vs_end_traverse_cor.pdf'))
plot(ll_df$end_traverse_cor, ll_df$straightness, xlab = 'end traverse correlation', ylab = 'route straightness', main = params$my_species)
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
#                             source = file.path('R', 'functions.R')))
# submitJobs(mutate(findNotSubmitted(), chunk = 1L),
#            resources = list(walltime = 10,
#                             memory = 8))
# waitForJobs()

## 3d plot of straightness, end_traverse_cor, and ll

plot3d(
  x = ll_df$ll, y = ll_df$straightness, z = ll_df$end_traverse_cor,
  col = 'gray',
  type = 's',
  xlab="ll", ylab="straightness", zlab="end_traverse_cor")
# To display in an R Markdown document:
# rglwidget()
#
# # To save to a file:
htmlwidgets::saveWidget(rglwidget(width = 520, height = 520),
                        file = file.path(params$output_path, "_3d_ll_straightness_traverse_cor.html"),
                        libdir = "libs",
                        selfcontained = TRUE
)
