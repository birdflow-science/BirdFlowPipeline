Sys.setenv(DEBUGME = "batchtools")
library(batchtools)

# See ?Registry for more info on configuration files, e.g., always loading
# certain packages or starting in certain working directories

reg <- makeRegistry('test_registry')
reg$cluster.functions <- makeClusterFunctionsSlurm(template = 'test_template.tmpl', array.jobs = TRUE, nodename = 'login2')

# Note that all variables defined in a JobCollection can be used inside the
# template. If you need to pass extra variables, you can set them via the
# argument resources of submitJobs().

# saveRegistry()

# ?setDefaultRegistry
# not needed because once we make registry, it stays for session as reg

dir <- 'batch_preprocess'
dir.create(dir, showWarnings = FALSE)
batchMap(fun = preprocess_species,
         species = c('rewbla', 'Hooded Warbler', 'amewoo', 'killde'),
         out_dir = dir,
         gpu_ram = 1)

rez <- list(walltime = 180, ncpus = 2, memory = 4000, partition = 'cpu-preempt', max.concurrent.jobs = 4)

submitJobs(resources = rez)
waitForJobs()

my_result_list <- lapply(1:4, loadResult)

my_species_vector <- unlist(lapply(my_result_list, function(i){i$species$species_code}))
my_resolution_vector <- unlist(lapply(my_result_list, function(i){i$geom$res[1]/1000}))
my_ebirdst_year_vector <- unlist(lapply(my_result_list, function(i){i$metadata$ebird_version_year}))

my_species_vector
my_resolution_vector
my_ebirdst_year_vector

#getJobTable()
