pit_calibration <- function(bf, transitions, params) {
  
  # one_hot vector of starting cell
  one_hot <- function(i,len) {
    stopifnot(!is.na(i))
    x <- rep(0,len); x[i] <- 1
    return(x)
  }

  # only from t to t+1
  transitions <- transitions[transitions$timestep2 - transitions$timestep1 == 1,]

  # Remove out-of-season transitions
  my_season_timesteps <- BirdFlowR::lookup_season_timesteps(bf, season = params$season)
  transitions <- dplyr::filter(transitions,
                               .data$timestep1 %in% my_season_timesteps & .data$timestep2 %in% my_season_timesteps)
  if (nrow(transitions)==0){
    return(list(
      res = NULL,
      D_row = NA,
      PIT_row_p = NA,
      D_col = NA,
      PIT_col_p = NA
    ))
  }
  # assign column names to the matrix
  my_colnames <- c("j", "wk1", "wk2", "i1", "i2", "x1", "x2", "y1", "y2", "p_cell", "p_95", "in_95_set", "max_cell_pred_value", "sum_leq", "pit_row", "pit_col", "i1_is_valid", "i2_is_valid")
  # initialize an empty matrix to store the results
  res_matrix <- matrix(NA, nrow = nrow(transitions), ncol = length(my_colnames))
  colnames(res_matrix) <- my_colnames

  # loop over the rows of transitions
  for (j in 1:nrow(transitions)) {

    lon1 <- transitions$lon1[j]
    lat1 <- transitions$lat1[j]
    i1 <- transitions$i1[j]
    x1 <- transitions$x1[j]
    y1 <- transitions$y1[j]
    wk1 <- transitions$timestep1[j]
    
    lon2 <- transitions$lon2[j]
    lat2 <- transitions$lat2[j]
    i2 <- transitions$i2[j]
    x2 <- transitions$x2[j]
    y2 <- transitions$y2[j]
    wk2 <- transitions$timestep2[j]

    point_df_initial <- data.frame(x = lon1, y = lat1)
    point_df_final   <- data.frame(x = lon2, y = lat2)
    
    # birdflow one-hot distributions for banding and encounter locations
    d_initial <- BirdFlowR::as_distr(x = point_df_initial, bf = bf, crs = 'EPSG:4326')
    
    # Project forward from this location
    f <- stats::predict(bf,
                 distr = d_initial,
                 start = wk1,
                 end = wk2,
                 direction = "forward")
    
    fdist <- f[,2]
    r <- BirdFlowR::rasterize_distr(fdist, bf)
    
    x_colsums <- as.vector(colSums(terra::as.array(r),na.rm=T))
    y_rowsums <- as.vector(rowSums(terra::as.array(r),na.rm=T))
    
    the_row <- BirdFlowR::i_to_row(i2,bf)
    # plot(y_rowsums); abline(v=the_row)
    pit_row <- sum(y_rowsums[1:(the_row-1)]) + stats::runif(1)*y_rowsums[the_row]
    
    the_col <- BirdFlowR::i_to_col(i2,bf)
    # plot(x_colsums); abline(v=the_col)
    pit_col <- sum(x_colsums[1:(the_col-1)]) + stats::runif(1)*x_colsums[the_col]
    
    p2 <- fdist[i2]
    # where does this fall in the distribution?
    sum_leq <- sum(fdist[fdist<=p2])
    
    ord_indx <- order(-fdist)
    fdist_ordered <- fdist[ord_indx]
    idx_95_ordered <- suppressWarnings({
      min(which(cumsum(fdist_ordered)>0.95))
    })
    idx_95 <- ord_indx[idx_95_ordered]
    
    # probability above which lies >=0.95 of probability mass
    p95 <- fdist[idx_95]
    
    res_j <- c(j=j,
               wk1=wk1,
               wk2=wk2,
               i1=i1,
               i2=i2,
               x1=x1,
               x2=x2,
               y1=y1,
               y2=y2,
               p_cell=p2,
               p_95=p95,
               in_95_set=p2>=p95,
               max_cell_pred_value=max(fdist),
               sum_leq=sum_leq,
               pit_row=pit_row,
               pit_col=pit_col,
               i1_is_valid=TRUE,
               i2_is_valid=TRUE)
    
    # store the result in the matrix
    res_matrix[j,] <- res_j
    
  }
  # return the matrix
  rownames(res_matrix) <- NULL
  
  res_matrix <- as.data.frame(res_matrix)
  
  ks_row <- suppressWarnings(
    stats::ks.test(res_matrix$pit_row,"punif",0,1)
  )
  ks_row
  D_row <- ks_row$statistic 
  D_row # want to minimize this for given N
  PIT_row_p <- ks_row$p.value
  
  ks_col <- suppressWarnings(
    stats::ks.test(res_matrix$pit_col,"punif",0,1)
  )
  ks_col
  D_col <- ks_col$statistic 
  D_col # want to minimize this for given N
  PIT_col_p <- ks_col$p.value
  
  list(
    res = res_matrix,
    D_row = as.numeric(D_row),
    PIT_row_p = as.numeric(PIT_row_p),
    D_col = as.numeric(D_col),
    PIT_col_p = as.numeric(PIT_col_p)
  )
  
}

pit_plots <- function(pit_calibration_obj, params, modelname){
  res <- pit_calibration_obj$res
  if (is.null(res)){
    return()
  }
  
  output_plot <- function(plot_function, object_to_plot, modelname, filename, ..., out_dir = params$output_path){
    dir.create(file.path(out_dir, 'pit_plots'), showWarnings = FALSE)
    subfolder <- sub('\\.hdf5$', "", modelname)
    dest_dir <- file.path(out_dir, 'pit_plots', subfolder)
    dir.create(dest_dir, showWarnings = FALSE)
    outfile <- file.path(dest_dir, filename)
    pdf(outfile, 7, 7)
    plot_function(object_to_plot, ...)
    dev.off()
  }
  
  output_plot(plot_function = graphics::hist, object_to_plot = res$pit_row, modelname, filename = 'pit_score_rows.pdf', breaks=50,
              main=modelname, xlab = 'PIT score (rows)')
  output_plot(plot_function = graphics::hist, object_to_plot = res$pit_col, modelname, filename = 'pit_score_cols.pdf', breaks=50,
              main=modelname, xlab = "PIT score (columns)")
  output_plot(plot_function = graphics::hist, object_to_plot = res$p_cell, modelname, filename = 'pit_transition_prob_obs.pdf', breaks=50,
              main=paste(modelname, "Transition prob for obs movement", sep = '\n'), xlab = 'p_cell')
  output_plot(plot_function = graphics::barplot, object_to_plot = sum(res$in_95_set,na.rm=T)/length(res$in_95_set), modelname, filename = 'pit_prop_in_95ci.pdf',
              ylim = c(0, 1), ylab = "proportion of obs in 95% confidence set",
              main = modelname)
  output_plot(plot_function = graphics::hist, object_to_plot = res$sum_leq, modelname, filename = 'pit_sumfpwk2.pdf', breaks = 50,
              main = paste(modelname, "sum(f[f<=p_wk2])", sep = '\n'), xlab = 'sum_leq')
}

pit_plots_report <- function(pit_calibration_obj, params){
  res <- pit_calibration_obj$res
  
  output_plot <- function(plot_function, object_to_plot, ...){
    plot_function(object_to_plot, ...)
  }
  
  output_plot(plot_function = graphics::hist, object_to_plot = res$pit_row, breaks=50,
              main = '', xlab = 'PIT score (rows)')
  output_plot(plot_function = graphics::hist, object_to_plot = res$pit_col, breaks=50,
              main = '', xlab = "PIT score (columns)")
  output_plot(plot_function = graphics::hist, object_to_plot = res$p_cell, breaks=50,
              main="Transition prob for obs movement", xlab = 'p_cell')
  # output_plot(plot_function = graphics::barplot, object_to_plot = sum(res$in_95_set,na.rm=T)/length(res$in_95_set),
  #             ylim = c(0, 1), ylab = "proportion of obs in 95% confidence set")
  # output_plot(plot_function = graphics::hist, object_to_plot = res$sum_leq, modelname, filename = 'pit_sumfpwk2.pdf', breaks = 50,
  #             main = paste(modelname, "sum(f[f<=p_wk2])", sep = '\n'), xlab = 'sum_leq')
}
