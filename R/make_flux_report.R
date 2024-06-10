

#' Make flux report
#' 
#' Note: I ran this on unity after using `devtools::load_all()` it 
#' seemed to be writing temporary files to the inst/rmd directory.  This 
#' suggests that it might not work with an installed version of BirdFlowPipeline.
#' If that's the case we'd have to copy the Rmd from the package to a temorary
#' location before knitting. 
#' 
#'
#' @param hdf path to HDF5 model file
#' @param flux path to flux RDS file
#' @param anim path to abundance and flux animation GIF
#' @param html path to output html file
#' @param prevlink relative path to the previous species report or NULL if 
#' there is none.
#' @param nextlink relative path to the next species report or NULL if none.
#'
#' @return Nothing is returned, a report is created at `html`
#' @export
#'
#' @examples
#' \dontrun{
#' base <- "/work/pi_drsheldon_umass_edu/birdflow_modeling/pipeline"
#' dir <- file.path(base, "/americas/acafly_100km")
#' html_dir <-file.path(base, "model_release_staging/flux/")
#' hdf <- file.path(
#'   dir, 
#'   "acafly_2022_100km_obs1.0_ent0.001924_dist0.008177_pow0.4167.hdf5")
#' flux <- file.path(dir, "acafly_flux.rds")
#' anim <- file.path(dir, "acafly_anim.gif")
#' html <- file.path(html_dir, "acafly.html")
#' prevlink <- NULL
#' nextlink <- "./adafly.html"
#' make_flux_report(hdf = hdf), 
#'                 flux = flux, 
#'                 anim = anim, 
#'                 html = html,
#'                 prevlink = prevlink, 
#'                 nextlink = nextlink)
#'}
make_flux_report <- function(hdf, flux, anim, html, prevlink, nextlink) {
  
  rmd_file <- system.file("rmd/flux_model_report.Rmd", package = "BirdFlowPipeline")
  
  suppressMessages({
    rmarkdown::render(
      input = rmd_file,
      output_file = html,
      params = list(hdf = hdf, 
                    flux = flux, 
                    anim = anim, 
                    prevlink = prevlink, 
                    nextlink = nextlink),
      quiet = !BirdFlowR::birdflow_options("verbose"))
  })
 
}