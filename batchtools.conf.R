cluster.functions <- makeClusterFunctionsSlurm(template = 'slurm.tmpl',
                                               array.jobs = TRUE,
                                               nodename = 'login5')
default.resources <- list(
  ncpus = 1
)
