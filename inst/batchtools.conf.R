# See here for system.file() modifications in next line:
# http://www.dahl-jacobsen.dk/data/2019/04/03/the-inst-folder-in-other-r-packages/\
# EBP - I dropped the [1] after .libPaths() as it wasn't finding the 
# package on the processing nodes.  I think BirdFlowPipeline will only ever
# be installed in the library location that's in the users home directory
# so there's not much benefit of telling it which library to install from.

cluster.functions <- 
  makeClusterFunctionsSlurm(template = system.file('slurm.tmpl', 
                                                   package = 'BirdFlowPipeline', 
                                                   lib.loc = .libPaths(), 
                                                   mustWork = TRUE),
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

# Constraints and preferences when lots of memory is required
# Ethan 2024-06-11
# I've added code to slurm.tmpl to set the vram[X] constraint whenever
# GPU runs are launched. It's set to the lowest [X] value that's higher than 
# resources$memory.  resources$memory doesn't have a default value here, 
# so is always set from the calling batch function.
# By trial and error I found that jobs that needed lots of memory were 
# running out of memory, and that the problem appears that the constrain.gpu 
# and prefer.gpu  values above take precedence, resulting in running on GPUs
# that can't meet the constraint=vram[x] GPU ram requirement. 
# In batch_species therefore, I pass NULL to both contraint.gpu and prefer.gpu 
# so the vram constrain is met. 
#
# I left Dave's batch functions as is but to tune a really large
# model we'd have to change those - possibly just to NULL, but adding:
#    prefer.gpu = NULL,  constraint.gpur = NULL to modelfit_resources.





