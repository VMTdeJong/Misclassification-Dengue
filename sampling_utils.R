
inv_logit <- function(x) 
  1 / (1 + exp(-x))

logit <- function(x)
  log(x/(1-x))

sample_x_z_cont_to_bin <- function(n, gamma.0, gamma.z, z_mean, cov, j = NA, outcome = NA) {
  z <- sample_z_cont(n = n, z_n = length(gamma.z), z_mean = z_mean, cov = cov)
  x_p <- inv_logit(gamma.0 + z %*% gamma.z)
  x <- rbinom(n, 1, x_p)  
  cbind(x = x, z = z, j = rep(j, n), outcome = rep(outcome, n))
}

sample_z_cont <- function(n, z_n, z_mean, cov) {
  sigma <- matrix(cov, ncol = z_n, nrow = z_n)
  diag(sigma) <- 1
  out <- mvtnorm::rmvnorm(n = n, mean = rep(z_mean, z_n), sigma = sigma)
  colnames(out) <- paste("z", seq(from = 1, to = z_n), sep = "")
  out
}

v_cor <- function(d)
  cor(d$x, d$z)

e_cor <- function(d, co_target)
  sqrt((co_target - v_cor(d))^2)

e_prev <- function(d, prev_target)
  sqrt((prev_target - mean(d$x))^2)

f_prev <- function(gamma.0, gamma.z, n = 1e6, x_prev, z_prev) {
  d <- sample_z(gamma.0 = gamma.0, gamma.z = gamma.z, n = n, z_prev = z_prev)
  e_prev(d, x_prev)
}

f_cor <- function(gamma.z, gamma.0, n = 1e6, z_prev) {
  d <- sample_z(gamma.0 = gamma.0, gamma.z = gamma.z, n = n, z_prev = z_prev)
  e_cor(d, .7616) # value given in excel file.
}


# This function finds the right gamma.0 and gamma.z, given a desired x_prev and a fixed z_prev.
# Methods other than Brent would have been more obvious, but failed to find a right value in this case.
# So this solution iteratively applies Brent to the two statistics, and ultimately finds the right
# values for both of them.
optim_covs <- function(gamma.0.start, gamma.z.start, x_prev, z_prev) {
  o.gamma.0 <- optim(par = gamma.0.start, 
                     fn = f_prev, 
                     gamma.z = gamma.z.start, 
                     x_prev = x_prev,
                     z_prev = z_prev, 
                     method = "Brent", 
                     lower = -20, 
                     upper = 20)
  
  o.gamma.z  <- optim(par = gamma.z.start, 
                      fn = f_cor, 
                      gamma.0 = o.gamma.0$par, 
                      z_prev = z_prev, 
                      method = "Brent", 
                      lower = -20, 
                      upper = 20)
  
  for (i in seq_len(10)) {
    o.gamma.0 <- optim(par = o.gamma.0$par,
                       fn = f_prev,
                       gamma.z = o.gamma.z$par,
                       x_prev = x_prev,
                       z_prev = z_prev,
                       method = "Brent",
                       lower = -20,
                       upper = 20)
    
    o.gamma.z  <- optim(par = o.gamma.z$par,
                        fn = f_cor,
                        gamma.0 = o.gamma.0$par,
                        z_prev = z_prev,
                        method = "Brent",
                        lower = -20,
                        upper = 20)
  }
  
  list(gamma.0 = o.gamma.0$par, gamma.z = o.gamma.z$par)
}






