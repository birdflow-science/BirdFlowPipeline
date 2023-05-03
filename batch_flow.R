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

grid_search_list <- list(
  dist_weight = c(0.01, 0.025, 0.05, 0.10, 0.15),
  ent_weight = seq(from = 0.001, to = 0.021, by = 0.005),
  dist_pow = seq(from = 0.0, to = 0.8, by = 0.2)
)

# batch preprocess species

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
             myres = pp_info$res),
           grid_search_list = grid_search_list),
         reg = makeRegistry(paste0(make_timestamp(), '_mf'), conf.file = 'batchtools.conf.R'))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = 10,
                            ngpus = 1,
                            memory = gpu_ram + 1))
waitForJobs()

# Batch likelihoods

files <- list.files(path = my_dir,
                    pattern = paste0('^', pp_info$species, '.*', pp_info$res, 'km_.*\\.hdf5$'),
                    full.names = TRUE)

banding_df <- readRDS(file.path('rds', paste0(pp_info$species, '.rds')))
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
ll_results <- reduceResultsList()
ll_df <- lapply(ll_results, hyperparams_ll_df_row) %>% bind_rows %>% arrange(-ll)
ll_df$color <- hcl.colors(15, rev = TRUE)[cut(ll_df$ll, 15)]

# Plot the grid search likelihood results
plot3d( 
  x = ll_df$ent, y = ll_df$dist, z = ll_df$pow, 
  col = ll_df$color, 
  type = 's', 
  radius = .02,
  xlab="ent", ylab="dist", zlab="pow")

# To display in an R Markdown document:
# rglwidget()
# 
# # To save to a file:
htmlwidgets::saveWidget(rglwidget(width = 520, height = 520),
                        file = "3dscatter.html",
                        libdir = "libs",
                        selfcontained = TRUE
)

# Visualize model with best LL

ll_df$model[1]
bf <- import_birdflow(file.path(my_dir, ll_df$model[1]))
# 
# ## Plot map route_migration spring msap
# 
# rts <- route_migration(bf, 10, 'prebreeding')
# plot(get_coastline(bf, match_extent = TRUE))
# plot(rts$lines, add = TRUE)
# title(main = ll_df$model[1])

# graph route migration for all models in parallel

spring_migration_pdf <- function(filename, my_dir){
  bf <- import_birdflow(file.path(my_dir, filename))
  rts <- route_migration(bf, 10, 'prebreeding')
  pdf(file.path('output', 'maps', paste0(filename, '.pdf')))
  plot(get_coastline(bf, match_extent = TRUE))
  plot(rts$lines, add = TRUE)
  title(main = filename)
  dev.off()
}

batchMap(spring_migration_pdf,
         ll_df$model,
         more.args = list(my_dir = my_dir),
         reg = makeRegistry(paste0(make_timestamp(), '_pdf'),
                            conf.file = 'batchtools.conf.R',
                            packages = my_packages,
                            source = 'functions.R'))
submitJobs(mutate(findNotSubmitted(), chunk = 1L),
           resources = list(walltime = 10,
                            memory = 8))
waitForJobs()
