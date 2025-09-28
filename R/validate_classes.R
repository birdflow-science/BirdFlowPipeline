
#' Validate a BatchBirdFlowTrainer object
#'
#' Ensures that the input is a [BatchBirdFlowTrainer()] and that it
#' contains the required attributes.
#'
#' @param trainer An object to validate.
#'
#' @return Invisible `NULL`. Throws an error if validation fails.
#' @details
#' Checks:
#' \itemize{
#'   \item Class must include `"BatchBirdFlowTrainer"`.
#'   \item Must contain element `"params"`.
#' }
#'
#' @seealso [BatchBirdFlowTrainer()], [validate_BatchBirdFlowEvaluator()],
#'   [validate_TransitionsLoader()]
#' @export
#' @examples
#' \dontrun{
#' validate_batch_trainer(batch_trainer("amewoo"))
#' }
validate_BatchBirdFlowTrainer <- function(trainer) {
  
  if (!'BatchBirdFlowTrainer' %in% class(trainer)) {
    stop(sprintf("Expecting BatchBirdFlowTrainer class as input; got '%s'", class(trainer)))
  }
  
  names <- c("params")
  for (name in names) {
    if (!name %in% names(trainer)) {
      stop(sprintf("'%s' attribute is not found in the input batch trainer", name))
    }
  }
}



#' Validate a TransitionsLoader object
#'
#' Ensures that the input is a [TransitionsLoader()] and that it
#' contains the required attributes.
#'
#' @param loader An object to validate.
#'
#' @return Invisible `NULL`. Throws an error if validation fails.
#' @details
#' Checks:
#' \itemize{
#'   \item Class must include `"TransitionsLoader"`.
#'   \item Must contain element `"batch_trainer"`.
#' }
#'
#' @seealso [TransitionsLoader()], [validate_split_data()]
#' @export
#' @examples
#' \dontrun{
#' tl <- transitions_loader(BatchBirdFlowTrainer("amewoo"))
#' validate_TransitionsLoader(tl)
#' }
validate_TransitionsLoader <- function(loader) {
  
  if (!'TransitionsLoader' %in% class(loader)) {
    stop(sprintf("Expecting TransitionsLoader class as input; got '%s'", class(loader)))
  }
  
  names <- c("batch_trainer")
  for (name in names) {
    if (!name %in% names(loader)) {
      stop(sprintf("'%s' attribute is not found in the input loader", name))
    }
  }
  
}


#' Validate split data structure
#'
#' Ensures that a split dataset produced by [split_data.TransitionsLoader()]
#' or [train_test_split()] has the expected form.
#'
#' @param split_data A list object to validate.
#'
#' @return Invisible `NULL`. Throws an error if validation fails.
#' @details
#' Checks:
#' \itemize{
#'   \item Must be a list.
#'   \item Must contain elements `"training_data"` and `"test_data"`.
#' }
#'
#' @seealso [split_data.TransitionsLoader()], [train_test_split()]
#' @export
#' @examples
#' \dontrun{
#' parts <- train_test_split(load(transitions_loader(trainer)))
#' validate_split_data(parts)
#' }
validate_split_data <- function(split_data) {
  if (!inherits(split_data, 'list')) {
    stop(sprintf("The split_data should be a list! Got '%s'", class(split_data)))
  }
  
  for (name in c('training_data', 'test_data')) {
    if (!name %in% names(split_data)) {
      stop(sprintf("'%s' attribute is not found in the split_data", name))
    }
  }
}


#' Validate a BatchBirdFlowEvaluator object
#'
#' Ensures that the input is a [BatchBirdFlowEvaluator()] and that it
#' contains the required attributes.
#'
#' @param evaluator An object to validate.
#'
#' @return Invisible `NULL`. Throws an error if validation fails.
#' @details
#' Checks:
#' \itemize{
#'   \item Must inherit from both `"list"` and `"BatchBirdFlowEvaluator"`.
#'   \item Must contain element `"batch_trainer"`.
#' }
#'
#' @seealso [BatchBirdFlowEvaluator()], [validate_BatchBirdFlowTrainer()]
#' @export
#' @examples
#' \dontrun{
#' ev <- BatchBirdFlowEvaluator(batch_trainer("amewoo"))
#' validate_BatchBirdFlowEvaluator(ev)
#' }
validate_BatchBirdFlowEvaluator <- function(evaluator) {
  if (!inherits(evaluator, c('list', 'BatchBirdFlowEvaluator'))) {
    stop(sprintf("The evaluator should be a list and BatchBirdFlowEvaluator! Got '%s'", class(evaluator)))
  }
  
  
  for (name in c('batch_trainer')) {
    if (!name %in% names(evaluator)) {
      stop(sprintf("'%s' attribute is not found in the evaluator", name))
    }
  }
}
  
  
  
  
  
