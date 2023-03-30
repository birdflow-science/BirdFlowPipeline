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

piApprox = function(n) {
  nums = matrix(runif(2 * n), ncol = 2)
  d = sqrt(nums[, 1]^2 + nums[, 2]^2)
  4 * mean(d <= 1)
}
set.seed(42)
#piApprox(1000)

batchMap(fun = piApprox, n = rep(1e5, 2))



rez <- list(walltime = 180, ncpus = 1, memory = 4000, partition = 'cpu-preempt')

submitJobs(resources = rez)
waitForJobs()
sapply(1:2, loadResult)

#getJobTable()
