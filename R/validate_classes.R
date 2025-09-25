
#' What a batch trainer should have:
#' 1. params
#' @export
validate_batch_trainer <- function(trainer) {
  
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

#' @export
validate_transition_loader <- function(loader) {
  
  if (!'TransitionsLoader' %in% class(loader)) {
    stop(sprintf("Expecting TransitionsLoader class as input; got '%s'", class(loader)))
  }
  
  names <- c("bf", "params")
  for (name in names) {
    if (!name %in% names(loader)) {
      stop(sprintf("'%s' attribute is not found in the input loader", name))
    }
  }
  
}


#' @export
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

#' @export
validate_batch_evaluator <- function(evaluator) {
  if (!inherits(evaluator, c('list', 'BatchBirdFlowEvaluator'))) {
    stop(sprintf("The evaluator should be a list and BatchBirdFlowEvaluator! Got '%s'", class(evaluator)))
  }
  
  
  for (name in c('batch_trainer')) {
    if (!name %in% names(evaluator)) {
      stop(sprintf("'%s' attribute is not found in the evaluator", name))
    }
  }
}
  
  
  
  
  
