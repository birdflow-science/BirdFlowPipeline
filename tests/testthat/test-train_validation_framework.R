test_that("train-validation framework works", {

  # # test fails using devtools::check(), for confusing reasons
  # # this line makes this test run with devtools::test() but not devtools::check()
  # skip_if_not(interactive())
  # # skip if not linux
  # skip_on_os(
  #   c("windows", "mac", "solaris")
  # )
  # # skip if not on Unity
  # skip_if_not(
  #   system2('hostname', '-d', stdout = TRUE) == "unity.rc.umass.edu"
  # )
  
  # test batch_flow()
  test_species <- "Wood Thrush"
  test_species <- ebirdst::get_species(test_species)
  
  test_res <- 700
  tempdir1 <- system('mktemp -d --tmpdir=$HOME .tmpdir-XXXXXXXX', intern = TRUE)
  tempdir2 <- system('mktemp -d --tmpdir=$HOME .tmpdir-XXXXXXXX', intern = TRUE)
  on.exit({
    if (file.exists(tempdir1)) unlink(tempdir1, recursive = TRUE)
    if (file.exists(tempdir2)) unlink(tempdir2, recursive = TRUE)
  })
  test_hdf_dir <- tempdir1
  test_output_path <- tempdir2
  
  ## 02. Fit batch models
  my_batch_trainer <- BatchBirdFlowTrainer(test_species,
                                    res = test_res,
                                    gpu_ram = 1, hdf_path = test_hdf_dir, base_output_path = test_output_path,
                                    suffix='TEST')
  
  my_batch_trainer <- fit(my_batch_trainer) #force_refit=TRUE) # Fit the model with parameter grid
  saveRDS(my_batch_trainer$params, file.path(my_batch_trainer$params$output_path, 'params.rds'))
  write.csv(as.data.frame(batchtools::getJobStatus()), paste0(sp_output_path, 'training_JobStatus.csv'))
  
  ## 03. Import transition data
  data_loader <- TransitionsLoader(my_batch_trainer)
  data_loader <- data_loader |> load(loading_function=purrr::partial(get_transitions, max_n_intervals = 100)) # Here you can customize the loading_function for the transition data
  # Save the transition data
  saveRDS(data_loader$transitions, file.path(data_loader$batch_trainer$params$output_path, 'transitions.rds'))
  
  ## 04. Train test split
  split_data <- data_loader |> 
    split(splitting_function=train_test_split, seed=42) # Here you can customize the train_test_split function
  
  ## 05. Evaluate training set
  my_evaluator <- BatchBirdFlowEvaluator(my_batch_trainer)
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
  
  
  # suppressWarnings(
  #   expect_no_error(
  #     batch_flow(
  #       species = test_species,
  #       gpu_ram = 10,
  #       res = test_res,
  #       suffix = 'TEST',
  #       grid_search_type = 'new',
  #       grid_search_list = list(
  #         de_ratio = 8,
  #         obs_prop = c(0.95, 0.99),
  #         dist_pow = seq(from = 0.2, to = 0.8, by = 0.15),
  #         dist_weight = NA_real_,
  #         ent_weight = NA_real_),
  #       hdf_path = test_hdf_dir,
  #       base_output_path = test_output_path,
  #       season = 'prebreeding',
  #       truncate_season = FALSE,
  #       model_selection = 'real_tracking',
  #       clip = NULL,
  #       skip_quality_checks = FALSE,
  #       fit_only = FALSE
  #     )))
  
  expect_true(file.exists(test_hdf_dir))
  expect_true(file.exists(test_output_path))
})



