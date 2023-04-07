cluster.functions <- makeClusterFunctionsSlurm(template = file.path('tmpl', 'sbatch_modelfit_container_gpu.tmpl'),
                                               array.jobs = TRUE,
                                               nodename = 'login5')