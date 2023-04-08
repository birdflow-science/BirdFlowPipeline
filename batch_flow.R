#Sys.setenv(DEBUGME = "batchtools")

library(batchtools)
library(BirdFlowR)
library(dplyr)

# load functions
source('batch_functions.R')

# batch preprocess species
source('params.R')
batch_preprocess_species(params)
pp_info <- save_preprocessing_info()

# Batch fit models
batch_fit_models(params, pp_info)
