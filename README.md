This package contains functions to fit, select, and evaluate BirdFlow models on the Unity cluster. This README contains basic setup instructions, which should be sufficient for other users within the `pi_drsheldon_umass_edu` Unity group getting started using this package.

## 1. Setup a passwordless SSH connection to the Unity login node

This is necessary for automated submission of sbatch jobs to Unity from within R, when R is running on a compute node (e.g., in Rstudio)

First, we need to [set up the SSH keys](https://www.redhat.com/sysadmin/passwordless-ssh).

Open up a [Unity terminal window](https://ood.unity.rc.umass.edu/pun/sys/shell/ssh/login-node-round-robin.unity.rc.umass.edu).

You might be able to re-use an existing SSH key if you already did this but it's probably recommended unless you were using it for a similar purpose. Check for existing keys:

```
ls -l ~/.ssh
```

To create a new key, enter this command and hit enter. Hit enter again when asked if you want to create a passphrase. It's easier if we don't use one, and this is all occurring within Unity and within your home directory, which is already fairly secure, but your mileage may vary.

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
ssh-copy-id <yourusername_umass_edu>@login1
```

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

Add the following to your `~/.Renviron` file so that the `ebirdst` package recognizes you as a known user for downloading eBird Status and Trends data

```
EBIRDST_KEY='your_ebirdst_api_key'
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
devtools::install()
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
5. [Advanced] Override Rstudio image location: `/work/pi_drsheldon_umass_edu/birdflow_modeling/BirdFlowContainer/BirdFlowContainer.sif`
6. Use default or blank values for the other fields

Once your session is active, click the blue button to connect to Rstudio server. Once in Rstudio, you can go to File > New Project, and select (or create) a directory in which to work. For example you might want to select the top level of a clone of Miguel's birdflow repo. The bottom right should now show the files in the directory you selected. The container should already include all the needed python libraries to run `update_hdf.py`.

You can get a python console by issuing the `reticulate::repl_python()` in the R console, which will turn it into a Python console. This will also happen if you run any python code from the source window. To get back to R, type `exit` in the Python console.

Very soon, Unity OnDemand will offer a way to use Jupyter Lab instead of Rstudio to work interactively in Python on a container.

## Description of what happens when you run `batch_flow('amewoo')`

This command kicks off the "standard pipeline" for fitting a BirdFlow model using tracking data, developed by Dave Slager in 2023. Much of the computation happens in parallel via batch jobs on the Unity cluster. See also: `?BirdFlowPipeline::batch_flow`

The `batch_flow()` function has two arguments: 1) The species to use, and 2) a parameters list (`params`).  The `params` list gets carried forward throughout `batch_flow()`, mostly using the original params list given. The exception is that `params` is modified during the preprocessing step, because some new information is produced during preprocessing. Once `params` is fleshed out after preprocessing, it is written to file in the output directory as `params.rds`, which is useful for debugging intermediate steps and documenting what you did in a particular run. `params` gets used as input by most functions in `batch_flow`.

### The default arguments for `params` exist as follows for the following reasons:
- `gpu_ram` and `my_res` get passed to `BirdFlowR::preprocess_species` for determining the raster resolution.
  - The default is 150 km because it seems to accommodate even species that are widespread across the Western Hemisphere, while still maintaining a fairly high resolution
  - `res` overrides `gpu_ram` in `BirdFlowR::preprocess_species`, but setting `gpu_ram` at 10 or 11 would get it close to the upper limit of available Unity GPUs.
- `season` is always (and usually only) used for model evaluation. For example, in the standard pipeline, if we're using 'prebreeding' we want to use spring tracking data, simulate spring tracks, and check status and trends end correlation for the spring season. This is related to the fact that BirdFlow is currently intended to be used within a single migration season. Full-year models are still generally produced, but when `truncate_season` is TRUE, then models are truncated to `season` during preprocessing. In other words, if `truncate_season` is FALSE and `season` is 'prebreeding', then season = 'all' will be passed to `BirdFlowR::preprocess_species` but a particular season will still be used for model selection and evaluation.
- `clip` is passed to `BirdFlowR::preprocess_species`, and has been used for trimming all species to a Western Hemisphere shape object before preprocessing. Be careful about assumptions when doing this.  For example, Northern Wheatears cross hemispheres during migration, and Long-tailed Jaegers become pelagic during the non-breeding season. Both would be filtered out if cropping to the current Western Hemisphere shapefile.
- `skip_quality_checks` is also passed to `BirdFlowR::preprocess_species`
- When `fit_only` is set to TRUE, it will skip all the model evaluation and exit `batch_flow()` after doing the Python modelfit. This was implemented for doing a large run to fit single averaged hyperparameter models for all migratory species.
- The `model_selection` argument controls the behavior of the model desirability rankings in the `rank_models()` function, which is run later in `batch_flow` when selecting among multiple models of a hyperparameter grid search. See `rank_models()` for current options. The standard pipeline as of January 2024 uses the `real_tracking` option. The `averaged_hyperparameters` option is being used as of January 2024 to mostly bypass model ranking when only 1 set of hyperparameters is available per model, like when averaged hyperparameters are specified directly (basically, a grid search of size 1)
- The `*path` items control where files are stored and currently get pulled from the package internal environment `the`. See `R/the_settings.R` in the package for these defaults, which also include things like login node name to use (depending on your `~/.ssh/config`) and default time zone for naming model run directories. Some functions still need to have their default arguments updated to use the appropriate `the$` entry.
- The `grid_search*` items control how hyperparameters are selected for a grid search. There are two grid search types supported.
  - For `old`, you specify all the Python hyperparameters directly.
  - For `new`, you supply dist_pow, obs_weight, and de_ratio. These are passed to the `refactor_hyperparameters` function, which recalculates dist_pow and ent_weight before passing these to Python as hyperparameters. 'new' is the current default behavior because the ratio between distance weight and entropy weight heavily determines the behavior of the model, and because these hyperparameters are highly sensitive, and refactoring them in this way makes it easier to intuit about and appears to create better coverage and performance of the "grid" search.
  
### Steps of `batch_flow`

The first thing that happens is preprocessing. The `preprocess_species_wrapper` function also sets up directories and updates the `my_res` and `ebirdst_year` items in `params`. The wrapper saves the `hdf` to a temporary directory before copying it over to the actual directory, because `res` needs to be calculated by preprocess_species before the final item can be named appropriately.

The next thing that happens is the Python modelfits. These will happen in parallel on the cluster, which is all set up by `batch_modelfit_wrapper()`. The function `birdflow_modelfit_args_df()` does grid-expand of all the modelfit parameters for the grid search, checks the hdf directory to see if any hdf files are already present, and deletes unneeded hdf files and pares down the list of models yet to fit as necessary. This reduces the number of GPU jobs used, especially during troubleshooting and debugging of larger grid searches. The output of `birdflow_modelfit_args_df()` is mapped by row to `birdflow_modelfit()` by `batchtools::batchMap`, which maps the grid search entries to an Sbatch array job to be sent to the cluster. The `birdflow_modelfit()` function is just an R wrapper to call Python, with a little bit of error handling so that a Python error also throws an R error.
  - During building this array job, `batchtools::batchMap` references the package configuration file `batchtools.conf.R` which specifies the best cluster resources to use as of January 2024, and also the package slurm template `slurm.tmpl` which is what will become the actual Sbatch script used for each job. The `slurm.tmpl` is set up to use different defaults depending on whether a GPU/Python job or a CPU/R job is requested, but it will use the same BirdFlowContainer image for both.
The last part of `batch_modelfit_wrapper()` submits the Python modelfit jobs to the cluster, waits for the jobs to complete, retries any failed jobs up to 3 times, and throws an error if not all the jobs completed. If there are errors, troubleshooting can be accompolished by checking the `*_mf` directory (= batchtools registry) inside the output directory, particularly the `logs` and `jobs` subfolders.

The next step creates "tracks" from banding data for use in log likelihood calculations. This is an older part, since the log likelihood calculations from banding are not currently (January 2024) a main consideration in model desirability. It tries to pull the banding data from a default banding data path. This path is assumed to be populated by RDS files created earlier and interactively using the functions in `R/banding_data.R`. Those functions facilitate downloading raw banding data from USGS, crosswalking the USGS taxonomy to the eBird S&T taxonomy, and writing standardized RDS files of banding data. The taxonomy crosswalk is currently outdated and needs to be updated, because it's for eBird S&T v. 2021. The point of `combine_together_list()` and `process_file_set()` is to process certain sets of raw USGS files at the same time, because the taxonomy crosswalk needs to combine multiple taxa into 1 S&T taxon, and the target taxa unfortunately sometimes span >1 raw USGS file. If there's no banding data, you get a blank template object for later handling. The variable `track_info` is very confusing here because it is actually banding data, and this variable should be renamed. The `track_info` object will eventually get used by `BirdFlowR::interval_log_likelihood` within `evaluate_model`, creating likelihood-related columns in `ll_df`, which was originally just a "log likelihood data.frame", but became the data.frame for all model evaluation metrics.  Also a candidate for renaming!

The next step is evaluating each of the models in the grid search. This happens in parallel on the cluster in an sbatch array job, just like for the python modelfit, except this time it's run on a CPU node and it's all in R. The function that does it all is `batch_evaluate_models()`, which starts by globbing the hdf directory for relevant hdf files. Then it batch-maps the fairly well named `import_birdflow_and_evaluate()` function to each of those hd5 files. It's just a wrapper that imports the birdflow object and then runs `evaluate_model()` on the birdflow object.  The overall `batch_evaluate_models()` function retries failed jobs a few times if necessary, and then if all the jobs returned results, consolidates those results into a data.frame, `ll_df`, which has 1 row for each model in the grid search, and columns for each model evaluation metric returned.

The `evaluate_model()` function itself calculates a bunch of things for each model. It runs `BirdFlowR::interval_log_likelihood()` using the banding data, simulates some routes, and calculates movement ecology metrics like straightness on those simulated routes. It also checks the tracking data path for a transitions file.  These files were provided by Benjamin via Benjamin's code, and include info on all the 1-week cell transitions represented in the tracking data available for a particular species. This needs to be cleaned up and scaled up, by using the new `BirdflowR` function to ingest raw tracking data, and an as yet unavailable (January 2024) function to convert from cleaned up tracking data to 1-week transitions data. If tracking transitions data is found, PIT calbiration is conducted. Otherwise, a default blank PIT result is returned. PIT data and PIT plots are stored in the output directory for use in model reports. It returns a data.frame and some log likelihood info, which goes through some final processing steps once returned to `batch_evaluate_models()`.

Now the `ll_df` which contains all the model evaluation info gets passed to `rank_models` for model selection. First, any existing desirability columns are deleted. The model selection method depends on what was specified in `params`. For `avaraged_hyperparameters`, effectively there is no model selection because it's only 1 model. For `real_tracking` (the standard pipeline), the first thing that happens is that we calculate the movement ecology stats based o the real tracking data, using `real_track_stats()`.
- `real_track_stats()` searches a default path for one of Benjamin's `*tracks*` files, but once there's more tracking data this needs to be updated to use tracking data cleaned by the new `BirdFlowR` function instead.
- It then applies `tracks_to_rts()` to the tracking data, which creates a `BirdFlowR` routes-like object from the tracking data, from which tracking stats can be calculated in the same way as they were for the simulated routes.
The last part of `rank_models()` for the `real_tracking` method calculates desirability scores for individual metrics, including trying to target models where BirdFlow simualted routes give similar metrics to real tracking routes, and models with good PIT calibration and with good end traverse correlation with the S&T weekly distributions, as calcualted via `BirdFlowR`. Regardless of model selection method, the very last part of `rank_models()` calculates the overall desirability score from the chosen single-metric desirability scores and arranges the data.frame so that the highest desirability models are at the top. This provides some flexibility in returning similar results structures regardless of which model selection criteria are used. For more info on desirability, see the manual for the `desirability2` R package.

The rest of `batch_flow()` (described below) is currently skipped if there's less than 2 models.

The main thing that happens is that HTML model reports are rendered to the output folder for the top 5 models, using `model_report.Rmd`.

These model reports can then be examined for quality control prior to releasing a model to the public BirdFlow collection.
- `stage_model_release()` is a helper to move a user-selected model report to the release staging folder to be put into the public model collection.
- `rebuild_model_reports()` is a helper to rebuild the model reports, for example, if you've actively working on changing the model report RMD file.
Ethan currently (January 2024) has another codebase that refreshes the Birdflow model collections website when the model_release_staging folder is changed.
