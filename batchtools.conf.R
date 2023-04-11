cluster.functions <- makeClusterFunctionsSlurm(template = 'slurm.tmpl',
                                               array.jobs = TRUE,
                                               nodename = 'login5')
default.resources <- list(
  ncpus = 1,
  chunks.as.arrayjobs = TRUE,
  max.arrayjobs.gpu = 48,
  max.arrayjobs.cpu = 200,
  partition.gpu = 'gpu-preempt,gpu',
  partition.cpu = 'cpu-preempt,cpu',
  constraint.gpu = '2080|2080ti|v100'
)
