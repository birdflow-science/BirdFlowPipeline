test_that("unity configuration works if on unity", {
  # skip if not linux
  skip_on_os(
    c("windows", "mac", "solaris")
  )
  # skip if not on Unity
  skip_if_not(
    system2('hostname', '-d', stdout = TRUE) == "unity.rc.umass.edu"
  )
  # passwordless ssh connection to login node
  test_ssh_command <- paste('ssh -q', the$login_node, 'exit')
  expect_equal(system(test_ssh_command), 0)
  # repos are set to install binary packages from PPPM
  # and otherwise install source packes from CRAN, from ~/.Rprofile
  skip_if_not(interactive())
  repos_option <- getOption('repos')
  repos_expected <- c(
    RSPM = paste0(
      'https://packagemanager.posit.co/cran/__linux__/',
      system2('lsb_release', c('-c', '-s'), stdout = TRUE),
      '/latest'
    ),
    CRAN = 'https://cloud.r-project.org'
  )
  expect_equal(repos_option, repos_expected)
  http_option <- getOption('HTTPUserAgent')
  http_expected <-
    sprintf("R/%s R (%s)",
            getRversion(),
            paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"]))
  expect_equal(http_option, http_expected)
  # test that EBIRDST_KEY environment variable is correctly set
  expect_true(Sys.getenv('EBIRDST_KEY') != "")
  # test that warning about X11 from rgl package in rstudio is being avoided
  expect_true(getOption('rgl.useNULL'))
})
