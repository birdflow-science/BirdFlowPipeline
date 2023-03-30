Sys.setenv(DEBUGME = "batchtools")
library(batchtools)

# See ?Registry for more info on configuration files, e.g., always loading
# certain packages or starting in certain working directories

reg <- makeRegistry('test_registry')
reg$cluster.functions <- makeClusterFunctionsSlurm(template = 'modelfit.tmpl', array.jobs = TRUE, nodename = 'login2')

# Note that all variables defined in a JobCollection can be used inside the
# template. If you need to pass extra variables, you can set them via the
# argument resources of submitJobs().

# saveRegistry()

# ?setDefaultRegistry
# not needed because once we make registry, it stays for session as reg

dir <- file.path(getwd(), 'batch_preprocess')
dir.create(dir, showWarnings = FALSE)
myres <- 184
mymem <- 8

fit_model <- function(mypy, mydir, mysp, myres){
  system2(
    command = 'python3',
    args = c(mypy, mydir, mysp, myres)
  )
}

batchMap(fun = fit_model,
         mypy  = '/work/pi_drsheldon_umass_edu/birdflow_modeling/birdflow/update_hdf.py',
         mydir = dir,
         mysp  = 'rewbla',
         myres = myres)

rez <- list(walltime = 600,
            ncpus = 2,
            ngpus = 1,
            memory = gpu_ram,
            partition = 'gpu',
            max.concurrent.jobs = 4,
            mypy = '/work/pi_drsheldon_umass_edu/birdflow_modeling/birdflow/update_hdf.py',
            mydir = dir,
            mysp  = 'rewbla',
            myres = myres,
            mymem = mymem)

submitJobs(resources = rez)
waitForJobs()