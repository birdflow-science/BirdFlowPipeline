# create environment to store some configuration settings

the <- new.env(parent = emptyenv())

the$tz <- 'America/Los_Angeles'
the$login_node <- 'login1'
the$banding_raw_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/banding/raw'
the$banding_rds_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/banding/rds'
the$motus_raw_path_tag_deployment_data <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/motus/raw/tag-deployments.csv'
the$motus_raw_path_recovery_data <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/motus/raw/filtered_mbi_recoveries.csv'
the$motus_rds_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/motus/rds'
the$output_path <-      '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/output'
the$hdf_path <-         '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/hdf'
the$python_repo_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/BirdFlowPy'
the$tracking_data_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/tracking'
the$model_release_staging_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/model_release_staging'
the$standard_crs <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/crs_and_clip/americas_crs.wkt'
the$standard_range_clip <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/crs_and_clip/western_hemisphere.rds'
