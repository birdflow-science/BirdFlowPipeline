# BirdFlowPipeline

[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://birdflow-science.github.io/BirdFlowPipeline/)

This package contains functions to fit, select, and evaluate BirdFlow models on the Unity cluster. 

This README contains basic setup instructions, which should be sufficient for other users within the `pi_drsheldon_umass_edu` Unity group getting started using this package.

This package is mainly for internal use and is highly adapted to the Unity system. We do not expect outside users to apply it.


## 1. Setup a passwordless SSH connection to the Unity login node

This is necessary for automated submission of sbatch jobs to Unity from within R, when R is running on a compute node (e.g., in Rstudio)

First, we need to [set up the SSH keys](https://www.redhat.com/sysadmin/passwordless-ssh).

Open up a [Unity terminal window](https://ood.unity.rc.umass.edu/pun/sys/shell/ssh/login-node-round-robin.unity.rc.umass.edu).

You might be able to re-use an existing SSH key if you already did this but it's probably recommended unless you were using it for a similar purpose. Check for existing keys:

```
ls -l ~/.ssh
```

To create a new key, enter this command and hit enter. Hit enter again when asked if you want to create a passphrase. It's easier if we don't use one, and this is all occurring within Unity and within your home directory, which is already fairly secure, but your mileage may vary.

Should note that before calling ssh-keygen, you should make sure you're on login1 and ssh login1 if not. Otherwise, you can end up creating a key for the wrong login node.

```
ssh-keygen
```

This creates a public/private key pair in your home directory on Unity. Your home directory is on a network file system that is accessible from all Unity login and compute nodes.

Next, create a shortcut to SSH into Unity more easily:

Add the following to the file `~/.ssh/config`, removing all angled brackets:

```
Host login1
HostName login1.unity.rc.umass.edu
User <yourusername_umass_edu>
IdentityFile ~/.ssh/<your_unity_private_key_filename_created_in_last_step>
```

Now, copy over the SSH public key to the server with the following command, removing all angled brackets. You'll need to customize the command if you were not using the default key name.

```
ssh-copy-id i ~/.ssh/<your_unity_private_key_filename_created_in_last_step> <yourusername_umass_edu>@login1
```

Try to make sure which key is the right one and copy the right one.

Now, try SSHing into the login node from a compute node. First, get a command line on a compute node by entering this on a login node terminal:

```
salloc -c 2 -p cpu
```
The terminal prompt should change and show that you are on a compute node. Now, try SSHing back into the login node. If you are asked whether to want to add the server to known hosts, type `yes` and hit enter.

```
ssh login1
```

You might need to enter your Unity password the first time. If your prompt now changes again to say you're on the login node, it means your compute nodes should now be able to submit jobs to the login node in an automated fashion, with keys only and no passwords required.

## 2. Setup binary R package installation on Unity

Ensure that your `~/.Rprofile` file contains the following lines, so that binary packages are attempted to be installed first via [Posit Public Package Manger](https://packagemanager.posit.co/client/#/repos/cran/setup), followed by installing source packages on CRAN. This will greatly speed up package installs. The last line avoids a warning about X11 from the rgl package in Rstudio.

```
options(repos = c(
  RSPM = paste0(
    'https://packagemanager.posit.co/cran/__linux__/',
    system2('lsb_release', c('-c', '-s'), stdout = TRUE),
    '/latest'
  ),
  CRAN = 'https://cloud.r-project.org'
))
options(HTTPUserAgent = sprintf(
  "R/%s R (%s)",
  getRversion(),
  paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
))
options(rgl.useNULL = TRUE)
```

## 3. Set up eBird Status and Trends API key

Either [request a new key](https://ebird.github.io/ebirdst/#data-access),
or run the command below in R on your local machine to retrieve an existing key.

```
Sys.getenv("EBIRDST_KEY")
```

Add the following to your `~/.Renviron` file so that the `ebirdst` package recognizes you as a known user for downloading eBird Status and Trends data

```
EBIRDST_KEY='your_ebirdst_api_key'
```

Additionally, if you are on Unity, we have a common shared source of downloaded eBird Status and Trends data, so:
```
Sys.setenv(EBIRDST_DATA_DIR = BirdFlowPipeline:::the$st_download_path)
```

You may need to restart R and/or RStudio for this to start working.


## 4. Set up your [GitHub Personal Access Token (PAT)](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens) 

This step is optional, but will mean you are not required to always enter your GitHub password.

Follow the instructions here for setting up a [GitHub PAT](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens), and then add the following line to your `~/.bashrc` file:

```
export GITHUB_PAT=your_github_PAT_alphanumeric
```

Now source your `.bashrc` by running this in the Unity terminal:

```
source ~/.bashrc
```

Keep in mind that for some eventual scenarios, you might want/need to put this in your `~/.Renviron` file if the shell environment variable doesn't pass through to Rstudio:

```
GITHUB_PAT='your_github_PAT_alphanumeric'
```

## 5. Configure git

This step is also optional, for streamlining your use of git/github. You can view your current git configuration by using this command at the Unity command line:

```
git config --list --global
```

At a minimum, you'll probably want to set your [name](https://docs.github.com/en/get-started/getting-started-with-git/setting-your-username-in-git), [GitHub user email](https://docs.github.com/en/account-and-profile/setting-up-and-managing-your-personal-account-on-github/managing-email-preferences/setting-your-commit-email-address), and the [credential helper store](https://stackoverflow.com/questions/35942754/how-can-i-save-username-and-password-in-git) so that GitHub knows who you are and so that you don't need to continually enter your GitHub username and password. Use these commands:

```
git config --global credential.helper store
git config --global user.email='<your_github_username>@users.noreply.github.com'
git config --global user.name='Your Name'
```

Verify the info by running this command:

```
git config --list --global
```

Try your initial GitHub login by attempting to git clone a private GitHub repository that you have access to. When it asks you for your password, don't use your actual GitHub password.  Use your GitHub PAT from the above step. This can be tricky to set up, but if it works, you will no longer need to enter your GitHub password when using git.

## 6. Install and test BirdFlowPipeline

First, clone BirdFlowPipeline into your home directory, then enter the BirdFlowPipeline directory

```
cd ~
git clone https://github.com/birdflow-science/BirdFlowPipeline
cd BirdFlowPipeline
```

Launch Rstudio Server from the BirdFlowContainer image, following the [instructions in the README](https://github.com/birdflow-science/BirdFlowContainer) for BirdFlowContainer

In Rstudio, open the R Project file located at `~/BirdFlowPipeline/BirdFlowPipeline.Rproj`

Run this in the R console to check that the Unity tests all pass without error:

```
devtools::test(filter = 'unity')
```

Install BirdFlowPipeline and any necessary dependencies. Some restarts of R or the RStudio container may be required if package installation does not all progress in one step.

```
devtools::install(dependencies = TRUE)
```

If you have the BirdFlowPipeline installed for the old version for whatever reasons. Consider removing them.
```
rm -rf /home/yc85_illinois_edu/R/x86_64-pc-linux-gnu-library/4.3/BirdFlowPipeline
```

When all packages are successfully installed, you should be able to load the package like this without error:

```
library(BirdFlowPipeline)
```

Run the complete test suite for BirdFlowPipeline.  This will submit test jobs to Unity and check that output files are successfully created in a temporary directory:

```
devtools::test()
```



## 7. (Optional) Tips for developing for Python/GPU in Rstudio using the container

(First time only) make sure this line is in your `~/.Renviron` file on Unity:
```
RETICULATE_PYTHON='/conda/bin/python'
```

Go to [Unity OnDemand](https://ood.unity.rc.umass.edu/pun/sys/dashboard/batch_connect/sessions) and select RStudio Session. Use the following settings, and set your walltime as appropriate.


1. maximum job duration (walltime): as needed
2. partition: `gpu` (use `gpu,gpu-preempt` if under 2 hours walltime)
3. memory: `11`
4. gpu count: `1`
5. [Advanced] Override Rstudio image location: `/work/pi_drsheldon_umass_edu/birdflow_modeling/BirdFlowContainer45/BirdFlowContainer.sif`
6. Use default or blank values for the other fields

Once your session is active, click the blue button to connect to Rstudio server. Once in Rstudio, you can go to File > New Project, and select (or create) a directory in which to work. For example you might want to select the top level of a clone of Miguel's birdflow repo. The bottom right should now show the files in the directory you selected. The container should already include all the needed python libraries to run `update_hdf.py`.

You can get a python console by issuing the `reticulate::repl_python()` in the R console, which will turn it into a Python console. This will also happen if you run any python code from the source window. To get back to R, type `exit` in the Python console.

Very soon, Unity OnDemand will offer a way to use Jupyter Lab instead of Rstudio to work interactively in Python on a container.


## Usage example:


### **See vignettes!**


### Standard workflow:

#### 1. Model fitting (training):

```r
my_batch_trainer <- BatchBirdFlowTrainer(sp,
                                  res = resolution,
                                  gpu_ram = 10, 
                                  hdf_path = sp_output_path, 
                                  base_output_path = sp_output_path, 
                                  suffix='test_batch_model')
```

`BatchBirdFlowTrainer` is an object that setups up a training procedure, it stored most of the parameters, like RAM use, output path, species, crs.

After initiation, `fit` function fit all the models specified in `BatchBirdFlowTrainer`:

```r
my_batch_trainer <- fit(my_batch_trainer) 
# use test_one_fit=TRUE if don't want to submit to slurm but sample one model to test the fitting process on this local session
```

Now we have 225 models saved on Unity.

#### 2. Loading transitions (general mark-resight data):

Next, load the transition data (mark-resight data that combines tracking, banding, and Motus):

```r
data_loader <- TransitionsLoader(my_batch_trainer)
data_loader <- data_loader |> load_data(loading_function=get_transitions) # Here you can customize the loading_function for the transition data
```

Train-test split:

```r
train_test_data <- data_loader |> 
  split_data(splitting_function=train_test_split, seed=42) # Here you can customize the train_test_split function
```

#### 3. Calculate scores on training set

Now we evaluate each model using the "training_data" saved in train_test_data:


```r
my_evaluator <- BatchBirdFlowEvaluator(my_batch_trainer)
eval_res_train <- evaluate(my_evaluator, 
                           data=train_test_data$training_data, 
                           evaluation_function=evaluate_model)
# Use test_one_evaluate=TRUE if want to test your evaluation_function on this local session and not pushing it to slurm. In that case the function will return your validation result of a single model.
```

If you want the metric for each transition:

```r
# Save metrics for each transition each model
all_transitions <- list()
for (this_model in eval_res_train) {
  this_transition_df <- this_model$metric_for_each_transition
  this_transition_df$model <- this_model$df$model
  all_transitions[[length(all_transitions) + 1]] <- this_transition_df
}

all_transitions <- do.call(rbind, all_transitions)
```
This can be useful if you want to do additional evaluation based on a new dataset, and combine the result from different data sources to take an average.

But usually we only care about one-value summary for each model:

```r
# Get one score summary for each model
eval_res_train <- eval_res_train |> lapply(function(i){i$df}) |>
  data.table::rbindlist(fill = TRUE) |>
  tibble::as_tibble() |>
  dplyr::arrange(dplyr::desc(.data$mean_ll)) # Here we sort by mean log-likelihood
```

#### 4. Calculate scores on test set

Assuming now we have ranked models, we would also want to evaluate their performance on a test set, to prevent the case that the chosen best models are overfit to the training data but perform poorly on the hold-out dataset:

```r
eval_res_test <- evaluate(my_evaluator, data=train_test_data$test_data, evaluation_function=evaluate_model)

eval_res_test <- eval_res_test |> lapply(function(i){i$df}) |>
  data.table::rbindlist(fill = TRUE) |>
  tibble::as_tibble() |>
  dplyr::arrange(dplyr::desc(.data$mean_ll))
```

#### 5. Select one best model


Now, we select a best model based on the training set metrics, and see it performances on the test set.

```r
best_model <- eval_res_train |> 
  dplyr::filter(mean_dist_cor_whole_year>0.98) |> # Apply your own model selection criteria
  dplyr::arrange(-weighted_mean_ll) |> 
  dplyr::slice_head(n=1) |> 
  dplyr::select(model)
best_model <- best_model$model
score_best_model <- eval_res_test[eval_res_test$model==best_model,]

print(glue::glue('Best model: {best_model}, score: '))
print(c(score_best_model))

```

#### 6. Customize this process

##### 06.01: functions

There are three functions that can be customized:

1. `loading_function` (data loading function)
2. `splitting_function` (data train-test split function)
3. `evaluation_function` (the function to evaluate models)

If you change the `loading_function`, you probably also need to accordingly change the `splitting_function` and `evaluation_function`

##### 06.02: parameters

The input for `BatchBirdFlowTrainer`:

Some of the default arguments setup:

- `gpu_ram` and `res` get passed to `BirdFlowR::preprocess_species` for determining the raster resolution.
  - The default is 150 km because it seems to accommodate even species that are widespread across the Western Hemisphere, while still maintaining a fairly high resolution
  - `res` overrides `gpu_ram` in `BirdFlowR::preprocess_species`, but setting `gpu_ram` at 10 or 11 would get it close to the upper limit of available Unity GPUs.
- `season` is always (and usually only) used for model evaluation. For example, in the standard pipeline, if we're using 'prebreeding' we want to use spring tracking data, simulate spring tracks, and check status and trends end correlation for the spring season. This is related to the fact that BirdFlow is currently intended to be used within a single migration season. Full-year models are still generally produced, but when `truncate_season` is TRUE, then models are truncated to `season` during preprocessing. In other words, if `truncate_season` is FALSE and `season` is 'prebreeding', then season = 'all' will be passed to `BirdFlowR::preprocess_species` but a particular season will still be used for model selection and evaluation.
- `clip` is passed to `BirdFlowR::preprocess_species`, and has been used for trimming all species to a Western Hemisphere shape object before preprocessing. Be careful about assumptions when doing this.  For example, Northern Wheatears cross hemispheres during migration, and Long-tailed Jaegers become pelagic during the non-breeding season. Both would be filtered out if cropping to the current Western Hemisphere shapefile.
- `skip_quality_checks` is also passed to `BirdFlowR::preprocess_species`
- The `*path` items control where files are stored and currently get pulled from the package internal environment `the`. See `R/the_settings.R` in the package for these defaults, which also include things like login node name to use (depending on your `~/.ssh/config`) and default time zone for naming model run directories. Some functions still need to have their default arguments updated to use the appropriate `the$` entry.



# Transition data saved on Unity

1. Most of the time, you should use the preprocessed combined dataset saved on the disk: `BirdFlowPipeline:::the$combined_data_path_routes`, `BirdFlowPipeline:::the$combined_data_path_birdflowroutes` and `BirdFlowPipeline:::the$combined_data_path_birdflowintervals`. These data are preprocessed using function `BirdFlowPipeline::combine_data_for_all_sp`. With these sources, you can create a function like 

```r
  get_transitions_prepared <- function(loader) {
    BirdFlowPipeline::validate_TransitionsLoader(loader)
    
    ## Read the combined data
    combined_interval_path <- file.path('/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/combined_data/BirdFlowIntervals/100km', paste0(loader$batch_trainer$params$species, '.hdf5'))
    interval_obj <- BirdFlowR::read_intervals(combined_interval_path)
    # Filter intervals to ask at least one leg in the migration season
    target_timesteps <- c(BirdFlowR::lookup_season_timesteps(loader$batch_trainer$bf, season='prebreeding'), 
                          BirdFlowR::lookup_season_timesteps(loader$batch_trainer$bf, season='postbreeding'))
    interval_obj$data <- interval_obj$data[(interval_obj$data$timestep1 %in% target_timesteps) | (interval_obj$data$timestep2 %in% target_timesteps),]
    
    ## Get the one-week transition
    interval_one_week_obj <- interval_obj
    interval_one_week_obj$data <-interval_one_week_obj$data[interval_one_week_obj$data$timestep2 - interval_one_week_obj$data$timestep1 == 1,]
    interval_one_week_obj$data <- interval_one_week_obj$data[(interval_one_week_obj$data$timestep1 %in% target_timesteps) | (interval_one_week_obj$data$timestep2 %in% target_timesteps),]
    
    return(list(interval_obj=interval_obj,
                interval_one_week_obj=interval_one_week_obj))
  }
```

and pass this function as the data loading function: `data_loader |> load_data(loading_function=get_transitions_prepared)`. For details of the data combination, see `/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline/ground_truth_data/README.md`.

2. If you want to use data from separate data sources instead of using the combined one, never, ever, use raw CSV files for model tuning/validation. Read the rds instead. They are in `the$banding_rds_path`, `the$motus_rds_path`, and `the$tracking_rds_path`. They are standardized format with columns `[date, lat, lon, route_id, route_type]`, although the banding data rds has slightly different column names but contain all the information. But to load them easier, instead of querying the rds files manually, use `load_banding_df`, `load_motus_df`, and `load_tracking_df` functions so that the output is always `[date, lat, lon, route_id, route_type]`.
3. If new data are included for banding, motus, or tracking, make sure to run the following functions correspondingly to generate those rds files: `preprocess_banding_data_to_rds()`, `preprocess_tracking_data_to_rds()`, `process_motus_data_to_rds()`


# About banding data

## 1. Functions for preprocessing banding data from USGS BBL

The package file `R/banding_data.R`contains several functions for preprocessing raw banding data from USGS. Here's an overview of what these functions can accomplish:
  - Download raw banding data once (it's dozens of GB of data for hundreds of taxa)
  - Do basic preprocessing quality checks on the data
  - Crosswalk the USGS taxonomy to the eBird Status and Trends taxonomy (see details below)
  - Output a standardized RDS file for each eBird species
    - This goes into a special directory on Unity known to `BirdFlowPipeline` via the `the` environment within the package.
  - The output RDS file can then be consistently used downstream, e.g. by BirdFlowPipeline functions or other analyses.

The code is a bit old (circa spring 2023), but this is how it works:
- The package contains the HTML source (`data-raw/htmlpage.htm`) of the [USGS webpage](https://dx.doi.org/10.5066/P9BSM38F) that contains the URLs for downloading the actual raw banding data files . As of January 2024, looks like this DOI/webpage is no longer the most recent version, so this could probably use an update.
- During package development, the script `data-raw/banding_raw_data` was run. This script web scrapes the HTML file for the actual download URLs, and saves those URLs as package internal data object `BirdFlowPipeline::banding_raw_file_urls`. That object was then committed into the package.
- The function `download_banding_files()` reads that data object with the URLs, and then downloads the banding data files, with restarts, to the banding raw data path specified in the package-internal `the` environment (see `R/the_settings.R`)
- The function `combine_together_list()` allows you to specify which raw banding files need to be processed at the same time.  This is the case when 2 or more subspecies are in *different* raw banding files, and these subspecies need to be merged together into the same ebirdst species.
  - Some USGS banding data is reported as subspecies
  - need to merge multiple subspecies of data into one eBird species
  - need to specify which taxon combinations these are
      - sometimes, the taxa which need to be merged into one are even in separate raw datafiles
      - in other words, some groups of raw datafiles need to be `rbind`ed together before processing
  - The output of this function is a list of integer vectors.  Most of the list elements are just a single file number, but some list elements are vectors of 2 file numbers that need to be processed together.
- The function `process_file_set()` reads in the output from the previous step, and processes each raw banding file (or sometimes set of raw banding files).
  - Multiple raw banding files are rbinded first, if necessary
  - Next, basic QC/exclusions are performed using several helper functions
    - These exclusions are based on what we want to use the data for, and the metadata explanations available on the USGS download website.
  - Now a join is performed using the taxonomy crosswalk file and the RDS file is saved.
  - More on the taxonomy crosswalk:
    - USGS uses a different taxonomy than eBird, so need to do a taxonomic crosswalk
      - see also the subspecies combining problem, addressed in `combine_together_list()`
    - Caveat: some taxonomies are not even possible to reconcile.
      - for example, old banding data for Canada Goose includes data for Cackling Goose because these were long considered the same species.
        - pers. comm. with USGS staff
    - Caveat #2: Currently (January 2024) the taxonomic crosswalk used is for the 2021 ebirdst taxonomy. ebirdst has now been bumped to use the 2022 taxonomy.  So the taxonomy crosswalk should be re-calculated.
    - The taxonomy crosswalk is stored as an internal data object `taxonomy_crosswalk` in the BirdFlowPipeline package.
      - The crosswalk was created during development by running the script `data-raw/create_taxonomy_crosswalk.R` and committing the results to the package.
        - Upon looking at this script in January 2024, it is a bit rough. It's something I used to run this process once, and could use some code review/cleanup.
        - The file `bbl_species_file` is the lookup file that comes with the USGS banding dataset. The other two starting files are in the repo.
        - The file `taxonomic_join_overrides.csv` was created somewhat iteratively and manually, using Dave's knowledge of bird taxonomy changes, until all the conflicts were accounted for.
  - Performing all these raw to RDS conversions serially took too long, because the dataset is fairly large (I think ~60 GB or so). The solution was to batch map these on the cluster using the function `batch_preprocess_raw_files_to_rds`.
    - The default arguments for this function (and functions generally in this R file) should be updated, as needed to use the default paths in the package built-in `the` environment.
