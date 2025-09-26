##
run_example <- function(){
  
  ## There are three functions that can be customized:
  ## 1. loading_function (data loading function)
  ## 2. splitting_function (data train-test split function)
  ## 3. evaluation_function (the function to evaluate models)
  
  resolution <- 150
  sp = 'amewoo'
  
  sp_output_path <- paste0(glue::glue('/project/pi_drsheldon_umass_edu/birdflow/batch_model_validation/test_resolution_{resolution}km'),'/',sp)
  if (!dir.exists(sp_output_path)){dir.create(sp_output_path, recursive = TRUE)}
  
  ## 02. Fit batch models
  my_batch_trainer <- batch_trainer(sp,
                                    res = resolution,
                                    gpu_ram = 10, hdf_path = sp_output_path, base_output_path = sp_output_path,
                                    suffix='test_batch_model')
  
  my_batch_trainer <- fit(my_batch_trainer) #force_refit=TRUE) # Fit the model with parameter grid
  saveRDS(my_batch_trainer$params, file.path(my_batch_trainer$params$output_path, 'params.rds'))
  write.csv(as.data.frame(batchtools::getJobStatus()), paste0(sp_output_path, '/', 'training_JobStatus.csv'))
  
  ## 03. Import transition data
  data_loader <- transitions_loader(my_batch_trainer)
  data_loader <- data_loader |> load(loading_function=get_transitions) # Here you can customize the loading_function for the transition data
  # Save the transition data
  saveRDS(data_loader$transitions, file.path(data_loader$batch_trainer$params$output_path, 'transitions.rds'))
  
  ## 04. Train test split
  split_data <- data_loader |> 
    split(splitting_function=train_test_split, seed=42) # Here you can customize the train_test_split function
  
  ## 05. Evaluate training set
  my_evaluator <- batch_evaluator(my_batch_trainer)
  eval_res_train <- evaluate(my_evaluator, data=split_data$training_data, evaluation_function=evaluate_model)
  
  # Save metrics for each transition each model
  all_transitions <- list()
  for (this_model in eval_res_train) {
    this_transition_df <- this_model$metric_for_each_transition
    this_transition_df$model <- this_model$df$model
    all_transitions[[length(all_transitions) + 1]] <- this_transition_df
  }
  
  all_transitions <- do.call(rbind, all_transitions)
  saveRDS(all_transitions, file.path(my_evaluator$batch_trainer$params$output_path, glue::glue('Training_each_transition_evaluation.rds')))
  
  # Get one score summary for each model
  eval_res_train <- eval_res_train |> lapply(function(i){i$df}) |>
    data.table::rbindlist(fill = TRUE) |>
    tibble::as_tibble() |>
    dplyr::arrange(dplyr::desc(.data$mean_ll))
  saveRDS(eval_res_train, file.path(my_evaluator$batch_trainer$params$output_path, glue::glue('eval_res_train.rds')))
  
  ## 06. Evaluate test set
  eval_res_test <- evaluate(my_evaluator, data=split_data$test_data, evaluation_function=evaluate_model)
  
  # Save metrics for each transition each model
  all_transitions <- list()
  for (this_model in eval_res_test) {
    this_transition_df <- this_model$metric_for_each_transition
    this_transition_df$model <- this_model$df$model
    all_transitions[[length(all_transitions) + 1]] <- this_transition_df
  }
  
  all_transitions <- do.call(rbind, all_transitions)
  saveRDS(all_transitions, file.path(my_evaluator$batch_trainer$params$output_path, glue::glue('Test_each_transition_evaluation.rds')))
  
  # Get one score summary for each model
  eval_res_test <- eval_res_test |> lapply(function(i){i$df}) |>
    data.table::rbindlist(fill = TRUE) |>
    tibble::as_tibble() |>
    dplyr::arrange(dplyr::desc(.data$mean_ll))
  saveRDS(eval_res_test, file.path(my_evaluator$batch_trainer$params$output_path, glue::glue('eval_res_test.rds')))
  
  ## 07. Get the best model and its evaluation
  best_model <- eval_res_train |> 
    dplyr::filter(mean_dist_cor_whole_year>0.98) |>
    dplyr::arrange(-weighted_mean_ll) |> 
    dplyr::slice_head(n=1) |> 
    dplyr::select(model)
  best_model <- best_model$model
  
  score_best_model <- eval_res_test[eval_res_test$model==best_model,]
  print(c(score_best_model))
}




