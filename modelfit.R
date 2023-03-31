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
myres <- c(184,126)
mymem <- 8

fit_model <- function(mypy, mydir, mysp, myres){
  # not actually printed currently since not evaluated by R on remote
  # just need all arguments here for batch_map
  print(
    paste(
      mypy, mydir, mysp, myres
    )
  )
}

batchMap(fun = fit_model,
         mypy  = '/work/pi_drsheldon_umass_edu/birdflow_modeling/birdflow/update_hdf.py',
         mydir = dir,
         mysp  = 'rewbla',
         myres = myres)

# Need to loop through here to adjust resources for each job,
# since species and resolution needs to be a static "resource"
jobinfo <- getJobPars()$job.pars
for (i in seq_along(jobinfo)){
  # resources here includes the Python arguments to be deparsed into sbatch file via brew
  rez <- list(walltime = 600,
              ncpus = 2,
              ngpus = 1,
              partition = 'gpu',
              mypy = '/work/pi_drsheldon_umass_edu/birdflow_modeling/birdflow/update_hdf.py',
              mydir = dir,
              mysp  = jobinfo[[i]]$mysp,
              myres = jobinfo[[i]]$myres,
              mymem = mymem)
  submitJobs(ids = i, resources = rez)
}

# This ends up just expiring, because no batchtools process in remote, so no reporting of results
# waitForJobs()

# Either install R/batchtools on remote or check for output files manually.

