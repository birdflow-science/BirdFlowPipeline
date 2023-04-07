cluster.functions <- makeClusterFunctionsSlurm(template = file.path('tmpl', 'sbatch_preprocess_species.tmpl'),
                                               array.jobs = TRUE,
                                               nodename = 'login5')