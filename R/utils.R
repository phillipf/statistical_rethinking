calc_posterior <- function(x, size, prior = rep(1, 100), grid.size = grid_size) {
  
  # define grid
  p_grid <<- seq(from = 0, to = 1, length.out = grid.size)
  # define prior
  # prior <<- prior
  # compute likelihood at each value in grid
  likelihood <<- dbinom(x, size = size, prob = p_grid)
  # compute product of likelihood and prior
  unstd.posterior <<- likelihood * prior
  # standardize the posterior, so it sums to 1
  posterior <<- unstd.posterior / sum(unstd.posterior)
  
  plot(p_grid, posterior,
       type = "b",
       xlab = "probability of water", ylab = "posterior probability"
  )
  mtext("100 points")
  return(posterior)
}