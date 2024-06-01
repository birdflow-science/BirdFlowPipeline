
#' get the job efficiency of a completed job from slurm
#'
#' `get_job_efficiency()` calls the shell command `"seff"` on the job ID
#' via [batchtools::runOSCommand()], which uses ssh to connect to a login
#' node before running the command.  The resulting output is then parsed 
#' into an R list.
#' @param id The slurm job ID including an array suffix e.g. "_1" if
#'  appropriate.
#'
#' @return A list with items:
#' \item{`job_id`}{slurm job id - without array suffix}
#' \item{`array_job_id`}{slurm job id with array suffix}
#' \item{`state`}{The job state}
#' \item{`cores`}{How many cores did the job use.}
#' \item{`cpu_utilized`}{How much CPU time was used.}
#' \item{`cpu_efficiency`}{CPU efficiency}
#' \item{`walltime`}{ Job wall-clock time (ellapsed time) in h:m:s format}
#' \item{`memory_utilized`}{Memory utilized in human readible format with (varying) units}
#' \item{`memory_efficiency`}{Memory efficiency.}
#' \item{`mem_gb`}{Utilized memory in GiB (bytes * 1024^3)}
#' \item{`wall_min`}{Job wall-clock time in decimal minutes}
#' @export
#' @seealso [batchtools::findJobs()]
get_job_efficiency <- function(id) {
  stopifnot(length(id) == 1)
  cmd <- paste("seff ", id, sep = "")
  a <- batchtools::runOSCommand(cmd, nodename = the$login_node)
  if(a$exit.code != 0) {
    stop("seff command failed")
  }
  
  b <- a$output
  c <- yaml::read_yaml(text = paste(b, collapse = "\n"))
  
  # Cleanup names
  names(c) <- gsub("[[:blank:]/]+", "_", names(c))
  names(c) <- tolower(names(c))
  c$cluster <- NULL
  c$user_group <- NULL
  names(c)[names(c) == "job_wall-clock_time"] <- "walltime"
  
  
  bytes <- fs::fs_bytes(c$memory_utilized) |> as.numeric() 
  c$mem_gb <- bytes / 1024^3
  c$wall_min <- hms::parse_hms(c$walltime) |> as.numeric() / 60
  return(c) 

}