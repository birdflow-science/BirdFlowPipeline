test_that("unity configuration works if on unity", {
  # skip if not linux
  skip_on_os(
    c("windows", "mac", "solaris")
  )
  # skip if not on Unity
  skip_if_not(
    system2('hostname', '-d', stdout = TRUE) == "unity.rc.umass.edu"
  )
  # test ssh connection to chosen login node
  test_ssh_command <- paste('ssh -q', the$login_node, 'exit')
  expect_equal(system(test_ssh_command), 0)
})
