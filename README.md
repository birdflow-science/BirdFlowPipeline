# Add the following to your ~/.Rprofile to enable binary package installation in Rstudio

options(repos = c(CRAN = paste0('https://packagemanager.posit.co/cran/__linux__/', system2('lsb_release', c('-c', '-s'), stdout = TRUE), '/latest')))
options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])))

# Add the following to your ~/.ssh/config to enable passwordless SSH from the Rstudio OnDemand compute node to the login2 node
# (batchtools needs to SSH into the login node to execute sbatch from the login node)

Host login2
HostName login2.unity.rc.umass.edu
User <yourusername_umass_edu>
IdentityFile ~/.ssh/<your_unity_private_key.file>

# In the Rstudio terminal, test this by running ssh login2, and answer "yes" to add the server to known hosts if needed
# This step might not be required?

# Launch an Rstudio 4.2.3 container on Unity OnDemand, and install packages using the order in the Dockerfile instructions
# https://ood.unity.rc.umass.edu/pun/sys/dashboard/

# Try the test_batchtools.R script

# set EBIRDST_API_KEY environment variable


## Developing on python/GPU in rstudio

(First time only) make sure this line is in your `~/.Renviron` file:
```
RETICULATE_PYTHON='/conda/bin/python'
```

Go to [Unity OnDemand](https://ood.unity.rc.umass.edu/pun/sys/dashboard/batch_connect/sessions) and select RStudio Session. Use the following settings, and set your walltime as appropriate.

maximum job duration (walltime): as needed
partition: `gpu` (use `gpu,gpu-preempt` if under 2 hours walltime)
memory: `11`
gpu count: `1`
[Advanced] Override Rstudio image location: `/work/pi_drsheldon_umass_edu/birdflow_modeling/python_R_container/dslager_cuda11.8.sif`
Use default or blank values for the other fields


Once your session is active, click the blue button to connect to Rstudio server. Once in Rstudio, you can go to File > New Project, and select (or create) a directory in which to work. For example you might want to select the top level of a clone of Miguel's birdflow repo. The bottom right should now show the files in the directory you selected. The container should already include all the needed python libraries to run update_hdf.py.

You can get a python console by issuing the `reticulate::repl_python()` in the R console, which will turn it into a Python console. This will also happen if you run any python code from the source window. To get back to R, type `exit` in the Python console.

Very soon, Unity OnDemand will offer a way to use Jupyter Lab instead of Rstudio to work interactively in Python on a container in a more Pythonic way.