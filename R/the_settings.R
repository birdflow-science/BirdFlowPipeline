# create environment to store some configuration settings

the <- new.env(parent = emptyenv())

the$tz <- 'America/Los_Angeles'
the$login_node <- 'login1'

the$output_path <-      '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/output'
the$hdf_path <-         '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/hdf'
the$python_repo_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/BirdFlowPy'
the$model_release_staging_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/model_release_staging'
the$standard_crs <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/crs_and_clip/americas_crs.wkt'
the$standard_range_clip <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/crs_and_clip/western_hemisphere.rds'

## About raw data
the$banding_raw_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/raw_data/banding/raw'
the$banding_rds_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/raw_data/banding/rds'
the$motus_raw_path_tag_deployment_data <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/raw_data/motus/raw/tag-deployments.csv'
the$motus_raw_path_recovery_data <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/raw_data/motus/raw/filtered_mbi_recoveries.csv'
the$motus_rds_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/raw_data/motus/rds'
the$tracking_raw_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/raw_data/tracking/raw'
the$tracking_rds_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/raw_data/tracking/rds'
the$tracking_data_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/raw_data/tracking'

the$st_download_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/raw_data/status_and_trends'

## About combined data
the$combined_data_path_routes <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/combined_data/Routes'
the$combined_data_path_birdflowroutes <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/combined_data/BirdFlowRoutes'
the$combined_data_path_birdflowintervals <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/combined_data/BirdFlowIntervals'

## Others
Sys.setenv(EBIRDST_DATA_DIR = the$st_download_path)



