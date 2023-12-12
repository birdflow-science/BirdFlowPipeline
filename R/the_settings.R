# create environment to store some configuration settings

the <- new.env(parent = emptyenv())

the$tz <- 'America/Los_Angeles'
the$login_node <- 'login1'
the$banding_raw_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/banding_data'
the$banding_rds_path <- '/home/dslager_umass_edu/banding_raw_rds'
the$banding_output_path <- '/home/dslager_umass_edu/banding_output'
the$batch_hdf_path <- '/work/pi_drsheldon_umass_edu/birdflow_modeling/dslager_umass_edu/batch_hdf'
the$python_repo_path <- '/home/dslager_umass_edu/BirdFlowPy'
