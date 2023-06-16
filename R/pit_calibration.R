pit_calibration <- function(bf, transitions) {
  
  # one_hot vector of starting cell
  one_hot <- function(i,len) {
    x <- rep(0,len); x[i] <- 1
    return(x)
  }
  
  # assign column names to the matrix
  my_colnames <- c("j", "wk1", "wk2", "i1", "i2", "x1", "x2", "y1", "y2", "p_cell", "p_95", "in_95_set", "max_cell_pred_value", "sum_leq", "pit_row", "pit_col", "i1_is_valid", "i2_is_valid")
  # initialize an empty matrix to store the results
  res_matrix <- matrix(NA, nrow = nrow(transitions), ncol = length(my_colnames))
  colnames(res_matrix) <- my_colnames

  
  remove_invalid_transitions <- function(transitions){
    xy1 <- with(transitions,
                BirdFlowR::latlon_to_xy(lat.1,lon.1,bf))
    transitions$i1 <- with(xy1,
                           BirdFlowR::xy_to_i(x, y, bf))
    xy2 <- with(transitions,
                BirdFlowR::latlon_to_xy(lat.2,lon.2,bf))
    transitions$i2 <- with(xy2,
               BirdFlowR::xy_to_i(x, y, bf))
    transitions$i1_is_valid <- with(transitions,
                                    BirdFlowR::is_location_valid(bf, i=i1, timestep=st_week.1))
    transitions$i2_is_valid <- with(transitions,
                                    BirdFlowR::is_location_valid(bf, i=i2, timestep=st_week.2))
    transitions$valid_transitions <- transitions$i1_is_valid & transitions$i2_is_valid
    out <- transitions[transitions$valid_transitions,]
    out
  }
  
  transitions <- remove_invalid_transitions(transitions)
  
  # loop over the rows of transitions
  for (j in 1:nrow(transitions)) {

    lon1 <- transitions$lon.1[j]
    lat1 <- transitions$lat.1[j]
    wk1 <- transitions$st_week.1[j]
    
    lon2 <- transitions$lon.2[j]
    lat2 <- transitions$lat.2[j]
    wk2 <- transitions$st_week.2[j]
    
    xy1 <- BirdFlowR::latlon_to_xy(lat1,lon1,bf)
    i1 <- BirdFlowR::xy_to_i(xy1$x,xy1$y, bf)
    # must be an easier way than to use my custom function through 3 lines...
    stopifnot(!is.na(i1))
    loc1 <- one_hot(i1,BirdFlowR::n_active(bf))
    
    xy2 <- BirdFlowR::latlon_to_xy(lat2,lon2,bf)
    i2 <- BirdFlowR::xy_to_i(xy2$x,xy2$y, bf)
    # if (is.na(i2)){
    #   print(transitions[j,])
    # }
    stopifnot(!is.na(i2))
    # loc2 <- one_hot(i2,BirdFlowR::n_active(bf))
    
    i1_is_valid <- BirdFlowR::is_location_valid(bf, i=i1, timestep=wk1)
    i2_is_valid <- BirdFlowR::is_location_valid(bf, i=i2, timestep=wk2)

    # Project forward from this location
    f <- stats::predict(bf,
                 distr = loc1,
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
               x1=xy1$x,
               x2=xy2$x,
               y1=xy1$y,
               y2=xy2$y,
               p_cell=p2,
               p_95=p95,
               in_95_set=p2>=p95,
               max_cell_pred_value=max(fdist),
               sum_leq=sum_leq,
               pit_row=pit_row,
               pit_col=pit_col,
               i1_is_valid=i1_is_valid,
               i2_is_valid=i2_is_valid)
    
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
  
  ks_col <- suppressWarnings(
    stats::ks.test(res_matrix$pit_col,"punif",0,1)
  )
  ks_col
  D_col <- ks_col$statistic 
  D_col # want to minimize this for given N
  
  list(
    res = res_matrix,
    D_row = as.numeric(D_row),
    D_col = as.numeric(D_col)
  )
  
}

pit_plots <- function(pit_calibration_obj, params, modelname){
  res <- pit_calibration_obj$res
  
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
