# See here for system.file() modifications in next line:
# http://www.dahl-jacobsen.dk/data/2019/04/03/the-inst-folder-in-other-r-packages/
cluster.functions <- makeClusterFunctionsSlurm(template = system.file('slurm.tmpl', package = 'BirdFlowPipeline', lib.loc = .libPaths()[1], mustWork = TRUE),
                                               array.jobs = TRUE,
                                               nodename = BirdFlowPipeline:::the$login_node)
default.resources <- list(
  ncpus = 1,
  chunks.as.arrayjobs = TRUE,
  max.arrayjobs.gpu = 200,
  max.arrayjobs.cpu = 250,
  partition.gpu = 'gpu-preempt,gpu',
  partition.cpu = 'cpu-preempt,cpu',
  constraint.gpu = 'rtx8000|1080ti',
  prefer.gpu = '2080|2080ti|v100'
)

# scheduler gives you gpu nodes first because it's a higher priority partition, which has fast ials_gigabyte_gpu_2020|v100|a100 nodes
# then it gives you gpu-preempt partition. Here, prefer 2080/2080ti, but accept rtx8000/1080ti

# broader: constraint.gpu = '2080|2080ti|rtx8000|v100'
# but RTX 8000 seems a little slower sometimes
# benchmarks:
# ials 2 minutes
# 1080ti 3:30 to 5:00
# titanx 5:25
# v100 1:30
# rtx8000 2:20 - 3 min
