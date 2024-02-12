# BirdFlowPipeline (development version)

*Note* for existing old output to work with the changes below we'll have to 
update file names:   
  * "ll_df.rds"" to "eval_metrics.rds"

Standardized and updated naming conventions 
* "ll_df" -> "eval_metrics"
* "my_sp"", "my_species"", and "one_species" -> "species"
* "my_res" -> "res"
* "output_nickname" -> "suffix"

In `params` break `output_dir` into two:
*  `base_output_dir` the parent directory to
* `output_dir` the output directory where results should be written for the 
model currently being turned; set by `preprocess_species_wrapper()`.

Currently `output_dir` is equivallent to `base_output_dir` until 
`preprocess_species_wrapper()` is called and `output_dir` after.



## BirdFlowPipeline 0.0.9001
Feb 9, 2024

* Add NEWS.md
* Add schema outlining major package functions, how they call each other,
and what files they write.

* Add `gpu_ram()` to calculate the GB of GPU ram needed to fit a model



