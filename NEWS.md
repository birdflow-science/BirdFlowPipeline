# BirdFlowPipeline 0.0.0.9004
2024-05-31

* Adapt to Unity updates. Changed the slurm template file sligtly to avoid 
triggering an erronious error "Request a CPU Partition for CPU jobs".
* Switch report logo source to github so it doesn't depend on a local file - 
which was deleted. 
* Rollback change in `batch_flow()` so that hdf files are copied and then 
deleted.  Moving doesn't work across drives and casused an error in the unit
tests.
* Export`batch_flux()

# BirdFlowPipeline 0.0.0.9003
Batch Flux

New functions
* `batch_flux()`  Launch flux on slurm in parallel via batchtools
* `process_flux()` Wrap `BirdFlowR::flux()` and add importing the 
 model from a file and writing the result to a file.
* `make_flux_index()`  Create an html index to a directory of flux reports.
* `make_flux_report()` Create an html flux report.  
* `get_job_efficiency()` Get the efficiency of a completed slurm job.

# BirdFlowPipeline 0.0.0.9002

*Note* for existing old output to work with the changes below we'll have to 
update file names:   
  * "ll_df.rds" to "eval_metrics.rds"
  
Add `batch_species()` and `preprocess_and_fit()` for fitting species to fixed
hyperparameters. 

Vectorize `refactor_hyperparams()` and add test for it.

Add `set_pipeline_params()` function to set parameters for `batch_flow()` and
`multi_species_batch()`. This achieves two things:
  1. It makes it easier to set just one parameter to a non-default value while
  leaving the rest at the default.
  2. Adds documentation once in `set_pipeline_parameters()` that
  is inherited by `batch_flow()` and `multispecies_batch()`.

Standardized and updated naming: 
* "ll_df" -> "eval_metrics"
* "my_sp"", "my_species"", and "one_species" -> "species"
* "my_res" -> "res"
* "output_nickname" -> "suffix"
* "mydir", "my_dir" -> "dir" -- in the context of calls to python

In `params` object.
* break `output_dir` into two:
  *  `base_output_path` the parent directory to `output_dir`
  * `output_path` the output directory where results should be written for the 
  model currently being turned; set by `preprocess_species_wrapper()`.
  Previously `output_dir` was equivalent to `base_output_dir` until 
  `preprocess_species_wrapper()` is called and `output_dir` after.
* add `output_full_name`= NULL at start - final value is set by 
  `preprocess_species_wrapper()`

Vectorize  `birdflow_modelfit_args_df()` and add test to make sure end result
is the same. 


## BirdFlowPipeline 0.0.0.9001
Feb 9, 2024

* Add NEWS.md
* Add schema outlining major package functions, how they call each other,
and what files they write.

* Add `gpu_ram()` to calculate the GB of GPU ram needed to fit a model



