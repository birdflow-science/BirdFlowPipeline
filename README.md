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
