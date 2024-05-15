

#' Make html index file for flux reports
#' 
#' Note: I ran this on unity after using `devtools::load_all()` it 
#' seemed to be writing temporary files to the inst/rmd directory.  This 
#' suggests that it might not work with an installed version of BirdFlowPipeline.
#' If that's the case we'd have to copy the Rmd from the package to a temorary
#' location before knitting. 
#' 
#'
#' @param index Data frame with columns:
#' \describe{
#' \item{`species`}{eBird species code}
#' \item{`common_name`}{Species common name}
#' \item{`scientific`}{Scientific name}
#' \item{`report_exists`}{TRUE if an html report exists for the species.}
#' \item{`rel_link`}{The relative link to the species report from the index location (`html`)}
#' }
#' @param html path to output html index file.
#' @return Nothing is returned, a report is created at `html`
#' @export
#'
#' @examples
#' \dontrun{
#' index <- readRDS(system.file("rmd/flux_index_example.Rds", package = "BirdFlowPipeline"))
#' html <- file.path(tempdir(), "index.html")
#' make_flux_index(index, html)
#' file.exists(html)
#' file.remove(html)
#'}
make_flux_index <- function(index, html) {
  
  rmd_file <- system.file("rmd/flux_index.Rmd", package = "BirdFlowPipeline")
  
  suppressMessages({
    rmarkdown::render(
      input = rmd_file,
      output_file = html,
      params = list(index = index),
      quiet = !BirdFlowR::birdflow_options("verbose"))
  })
  
}