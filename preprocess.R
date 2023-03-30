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
         species = c('rewbla', 'Hooded Warbler'),
         out_dir = dir,
         gpu_ram = 1)



rez <- list(walltime = 180, ncpus = 2, memory = 4000, partition = 'cpu-preempt')

submitJobs(resources = rez)
waitForJobs()
lapply(1:2, loadResult)

#getJobTable()
