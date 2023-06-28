#' Grid search, model selection, and model evaluation for one species on the cluster
#'
#' @param params a list of standard parameters, typically constructed by complex default argument.
#'  * example `old` grid search list:
#'  ```{r, eval = FALSE}
#' grid_search_list <- list(
#'   dist_weight = seq(from = 0.0008, to = 0.0018, length.out = 5),
#'   ent_weight = seq(from = 0.00015, to = 0.0004, length.out = 5),
#'   dist_pow = seq(from = 0.1, to = .9, length.out = 5)
#' )
#' ```
#' * example `new` grid search list:
#'  ```{r, eval = FALSE}
#' grid_search_list <- list(
#'   de_ratio = c(2, 4, 8, 16),
#'   obs_prop = c(0.95, 0.975, 0.99, 0.999, 0.9999),
#'   dist_pow = seq(from = 0.2, to = 0.8, by = 0.15),
#'   dist_weight = NA_real_,
#'   ent_weight = NA_real_
#' )
#' ```
#' @param one_species a character vector of length 1 that is a valid input to [ebirdst::get_species()], e.g., an eBird 6-letter code or a valid common name
#'
#' @returns function is used for its many side effects, according to configuration settings in `as.list(banding:::the)`
#'  * create destination directories as needed
#'  * write preprocessed hdf5 file
#'  * write modelfit hdf5 files
#'  * write output files, plots, and maps for model evaluation and visualization
#' @seealso [multiple_species_batch()]
#' @export
batch_flow <- function(
    one_species,
    params = list(
      my_species = character(0),
      gpu_ram = 10,
      my_res = 100,
      output_nickname = as.character(Sys.Date()),
      grid_search_type = 'new',
      grid_search_list = list(
        de_ratio = c(2, 4, 8, 16),
        obs_prop = c(0.95, 0.975, 0.99, 0.999, 0.9999),
        dist_pow = seq(from = 0.1, to = 0.9, by = 0.1),
        dist_weight = NA_real_,
        ent_weight = NA_real_),
      batch_hdf_path = the$batch_hdf_path,
      banding_output_path = the$banding_output_path,
      season = 'prebreeding',
      model_selection = 'str_etc'
    )
){
params$my_species <- one_species

# preprocess species and set up directories

params <- preprocess_species_wrapper(params)
saveRDS(params, file.path(params$output_path, 'params.rds'))

# Batch fit models

batch_modelfit_wrapper(params)

# Load and save track info

track_info <- make_tracks(file.path(the$banding_rds_path, paste0(params$my_species, '.rds')))
saveRDS(track_info, file.path(params$output_path, 'track_info.rds'))

# Batch model evaluation

ll_df <- batch_evaluate_models(params, track_info)

# Model selection and ranking with desirability

ll_df <- rank_models(ll_df, params)

## Plotting

# make PCA evaluation plot

model_evaluation_biplot(ll_df, params)

# Plot likelihood results cube
make_3d_plot('color_ll', 'll', ll_df, params)
# Plot null likelihood cube
make_3d_plot('color_nll', 'nll', ll_df, params)
# Plot correlation cube
make_3d_plot('color_cor', 'cor', ll_df, params)

# save model evaluation RDS
saveRDS(ll_df, file.path(params$output_path, 'll_df.rds'))

# plot most desirable models

for (i in 1:5){
  pdf(file.path(params$output_path, paste0('desirability', i, '.pdf')))
  quick_visualize_routes(i, df = ll_df, dir = params$hdf_dir, season = params$season)
  dev.off()
}

# graph tradeoff

pdf(file.path(params$output_path, 'straightness_vs_end_traverse_cor.pdf'))
plot(ll_df$end_traverse_cor, ll_df$straightness, xlab = 'end traverse correlation', ylab = 'route straightness', main = params$my_species)
dev.off()

# Visualize model with best LL
# 
# quick_visualize_routes(1)

## 3d plot of straightness, end_traverse_cor, and ll

rgl::plot3d(
  x = ll_df$ll, y = ll_df$straightness, z = ll_df$end_traverse_cor,
  col = 'gray',
  type = 's',
  xlab="ll", ylab="straightness", zlab="end_traverse_cor")
# To display in an R Markdown document:
# rglwidget()
#
# # To save to a file:
htmlwidgets::saveWidget(rgl::rglwidget(width = 520, height = 520),
                        file = file.path(params$output_path, "_3d_ll_straightness_traverse_cor.html"),
                        libdir = "libs",
                        selfcontained = TRUE
)

## 3d plot of stopover, straightness, and end_traverse_cor

rgl::plot3d(
  x = ll_df$n_stopovers, y = ll_df$straightness, z = ll_df$end_traverse_cor,
  col = 'gray',
  type = 's',
  xlab="n_stopovers", ylab="straightness", zlab="end_traverse_cor")
# To display in an R Markdown document:
# rglwidget()
#
# # To save to a file:
htmlwidgets::saveWidget(rgl::rglwidget(width = 520, height = 520),
                        file = file.path(params$output_path, "_3d_n_stopovers_straightness_traverse_cor.html"),
                        libdir = "libs",
                        selfcontained = TRUE
)

} # big function end

#' Grid search, model selection, and model evaluation for multiple species on the cluster, with error handling
#' @param multispecies_vector a character vector that contains a valid inputs to [ebirdst::get_species()], e.g., eBird 6-letter codes or valid common names.
#' @returns function is used for its many side effects, according to configuration settings in `as.list(banding:::the)`
#'  * create destination directories as needed
#'  * write preprocessed hdf5 files
#'  * write modelfit hdf5 files
#'  * write output files, plots, and maps for model evaluation and visualization
#' @seealso [batch_flow()] for doing this for one species
#' @export
multiple_species_batch <- function(multispecies_vector) {
  for (species_i in multispecies_vector) {
    tryCatch({
      batch_flow(species_i)
    }, error = function(e) {
      cat("ERROR:", conditionMessage(e), "\n")
    })
  }
}
