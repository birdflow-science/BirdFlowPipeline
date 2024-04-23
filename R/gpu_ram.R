#' Calculate the GPU RAM needed for a model
#'
#' @param bf A BirdFlow model object 
#' @param max_param_per_gpu_gb The number of parameters that can be fit with 
#' each GB of GPU ram. If `NULL`, the default the value from
#' [BirdFlowR::birdflow_options()]` is used. 
#' The only reason to set it here is if that value is empirically wrong. 
#'
#' @return The GB of GPU RAM that was or will be required to fit `bf`
#' @export
#'
#' @examples
#' bf <- BirdFlowModels::amewoo
#' gpu_ram(bf)
gpu_ram <- function(bf, max_param_per_gpu_gb = NULL){
  if(is.null(max_param_per_gpu_gb)){
    max_param_per_gpu_gb <- BirdFlowR::birdflow_options()$max_param_per_gpu_gb
  }
  stopifnot(is.numeric(max_param_per_gpu_gb), 
            length(max_param_per_gpu_gb) == 1, 
            !is.na(max_param_per_gpu_gb))
  
  
  return(BirdFlowR::n_parameters(bf) / max_param_per_gpu_gb)
}
