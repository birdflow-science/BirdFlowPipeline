#' Make flux report
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