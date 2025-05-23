# BirdFlowPipeline 0.0.0.9013
2025-05-23

Update `rts_stats()` it now works with `BirdFlowRoutes` objects as well
as the data frame precursor to those objects.

Update functions and tests for ebirdst 2.2023.0.
Tests that depend on the **ebirdst** package version are also now skipped if an
older version is installed.

Clean up some other testing issues.

Resolve warnings and notes from package check.
* Cross references missing package anchors
* Functions not imported from Utils



# BirdFlowPipeline 0.0.0.9012
2024-10-03

Add license to footer. Three variables optionally defined in the parent 
.Rmd affect the licence.
* `is_html`  controls whether the license is defined as html or markdown,
   html version is fancier.  It will default to `TRUE` if not defined.
* `focal_species` For species specific version of the license this should 
be the common name of the species.
* `doc_url` The URL (in final location) of the current document.  


# BirdFlowPipeline 0.0.0.9011
2024-10-02

Add `title` argument to `build_collection_index()` and drop the default
description from the document when a custom description is supplied.
Default description is now in `rmd/collection_description.Rmd`.

# BirdFlowPipeline 0.0.0.9010
2024-09-26

Add `build_collection_index()`

* Move `build_collection_index()` from **BirdFlowR** to **BirdFlowPipeline**. 
* Add `description` argument to allow inserting collection specific
  markdown.
* Switch to the standard footer:  `rmd/footer.Rmd`

# BirdFlowPipeline 0.0.0.9009
2024-09-25

* Minor updates to model reports. This was in an attempt to adapt the model
reports for tuned models to work for models fit with averaged hyperparameters.
Ultimately these changes weren't used but I cleaned up some documentation and
added comments in the process so decided to keep the changes. 

# BirdFlowPipeline 0.0.0.9008
2024-08-15

* Add `show_progress` argument to `batch_species()` and `batch_flux()` if
`FALSE` the functions execute immediately after launching.  This facilitates
programmatically calling them in a loop to adjust computing resources to 
varied run times and memory requirements.

* New `inst/rmd/footer.Rmd` has links to BirdFlow and eBird and the
eBird disclaimer in a standard format.  It is inserted in the end of  
`flux_index.Rmd`, `flux_report.Rmd`, and`model_report.Rmd`.  Most of these
documents had the links, none previously had the disclaimer.  


# BirdFlowPipeline 0.0.0.9007
2024-08-14

Add handling of `trim_quantile` argument to `BirdFlowR::preprocess_species()` 
to the pipeline functions. The default of `NULL` (no trimming) is set in
`set_pipeline_parameters()` but can be overridden by setting another value
when calling `batch_species()` or `batch_flow()` 


# BirdFlowPipeline 0.0.0.9006
2024-06-12

Update how resources are requested on slurm to fit large GPU RAM models

* Add `#SBATCH --constraint vram[X]` where `[X]` is various levels of 
GB GPU RAM, to `slurm.tmpl`. 
The `vram[X]` constraint is only included when running with GPUs and the level of 
`[X]` is automatically set to the lowest level that is higher than the memory 
requested for the job in `modelfit_resources`. 
For a list of the supported levels see the 
[unity constraint list.](https://docs.unity.rc.umass.edu/documentation/cluster_specs/features/).
* If `resources$prefer.gpu` or `resources$constraint.gpu` are set to `NULL` or 
`NA` than the corresponding lines are dropped in `slurm.tmpl`, these constraints
were taking preference over the `vram[X]` constraints causing jobs to run
on nodes that didn't support the requested GPU RAM.
* `batch_species()` now sets `prefer.gpu` and `constraint.gpu` to `NULL`. 
The other `batch_` functions are not changed but in the future 
setting `modelfitresources$` `contraint.gpu` and `prefer.gpu` to `NULL` would 
allow for fitting larger models with those functions, but lose the GPU 
preferences Dave had set.
* `batch_flux()` now will  request the `cpu-preemt` and `cpu-long` partitions
if the walltime requested exceeds 24 hours. Previously you could request more
time with walltime but if the job ended up on the `cpu` queue it would still 
timeout after 24 hours. 


# BirdFlowPipeline 0.0.0.9005
2024-06-07

Add `prev` and `next` buttons to flux model reports.

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



