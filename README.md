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
