cluster.functions <- makeClusterFunctionsSlurm(template = system.file('slurm.tmpl', package = 'banding'),
                                               array.jobs = TRUE,
                                               nodename = 'login1')
default.resources <- list(
  ncpus = 1,
  chunks.as.arrayjobs = TRUE,
  max.arrayjobs.gpu = 200,
  max.arrayjobs.cpu = 250,
  partition.gpu = 'gpu-preempt,gpu',
  partition.cpu = 'cpu-preempt,cpu',
  constraint.gpu = '2080|2080ti|v100'
)

# broader: constraint.gpu = '2080|2080ti|rtx8000|v100'
# but RTX 8000 seems a little slower sometimes
# benchmarks:
# ials 2 minutes
# 1080ti 3:30 to 5:00
# titanx 5:25
# v100 1:30
# rtx8000 2:20 - 3 min
