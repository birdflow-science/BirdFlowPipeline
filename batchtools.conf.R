cluster.functions <- makeClusterFunctionsSlurm(template = 'slurm.tmpl',
                                               array.jobs = TRUE,
                                               nodename = 'login5')
default.resources <- list(
  ncpus = 1,
  chunks.as.arrayjobs = TRUE,
  max.arrayjobs.gpu = 48,
  max.arrayjobs.cpu = 200,
  partition.gpu = 'gpu',
  partition.cpu = 'cpu-preempt,cpu'
)
