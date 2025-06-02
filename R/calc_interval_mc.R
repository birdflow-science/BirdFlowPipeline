#' @export
calc_interval_mc <- function(interval, bf) {

  # Initialize origin and target distributions
  origin_abun <- target_abun <- rep(0, n_active(bf))
  # Tally the origin and target cells
  target_table <- table(interval$i2)
  origin_table <- table(interval$i1)
  # Populate origin and target distributions
  target_abun[as.numeric(names(target_table))]=target_table/sum(target_table)
  origin_abun[as.numeric(names(origin_table))]=origin_table/sum(origin_table)

  # Calculate distance matrices
  dist <- great_circle_distances(bf)  # all active cells

  # matrix product to have all combinations for two samples for origin_abun
  # note that this outer product remains normalized, because origin_abun is normalized
  origin_abun_prod <- origin_abun %*% t(origin_abun)
  # mu_D effectively equals weighted.mean(origin_dist, origin_abun_prod)
  mu_D <- sum(dist * origin_abun_prod)
  # SD in origin distance between any two given pixels
  sd_D <- sqrt(sum((dist - mu_D)^2 * origin_abun_prod))

  # same calculation, now for target_abun
  target_abun_prod <- target_abun %*% t(target_abun)
  mu_V <- sum(dist * target_abun_prod)
  sd_V <- sqrt(sum((dist - mu_V)^2 * target_abun_prod))

  # construct transition matrix
  psi <- as.matrix(table(interval$i1,interval$i2))
  # normalize the matrix
  psi <- psi/rowSums(psi)

  origin_abun <- origin_abun[as.numeric(rownames(psi))]
  target_abun <- target_abun[as.numeric(colnames(psi))]

  # multiply transition matrix and relative abundance
  psi_abun <- psi * origin_abun

  # standardizing origin distance matrix
  origin_std <- (dist[as.numeric(rownames(psi)), as.numeric(rownames(psi))] - mu_D) / sd_D
  target_std <- (dist[as.numeric(colnames(psi)), as.numeric(colnames(psi))] - mu_V) / sd_V

  # reduce the distance matrix and abundance vectors
  dist <- dist[as.numeric(colnames(psi)), as.numeric(rownames(psi))]

  dim(psi_abun)
  dim(origin_std)
  dim(target_std)

  # calculate MC
  #MC=sum(t(psi_abun) %*% origin_std %*% psi_abun * target_std)
  MC=sum(t(psi_abun) %*% origin_std %*% psi_abun * target_std)

  return(MC)
}
