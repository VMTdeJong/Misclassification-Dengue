get_summary_rjags <- function(object) {
  s <- summary(object)
  data.frame(mean   = s$statistics["beta_x_0", "Mean"],
             se     = s$statistics["beta_x_0", "SD"],
             ci.lb  = s$quantiles[ "beta_x_0", "2.5%"],
             median = s$quantiles[ "beta_x_0", "50%"],
             ci.ub  = s$quantiles[ "beta_x_0", "97.5%"])
}

get_summary_runjags <- function(object) {
  s <- object$summary
  ss <- as.data.frame(s$statistics)
  sq <- as.data.frame(s$quantiles)
  data.frame(mean   = ss["beta_x_0", "Mean"],
             se     = ss["beta_x_0", "SD"],
             ci.lb  = sq[ "beta_x_0", "2.5%"],
             median = sq[ "beta_x_0", "50%"],
             ci.ub  = sq[ "beta_x_0", "97.5%"],
             tau.mean   = ss["tau_beta_x_j", "Mean"],
             tau.se     = ss["tau_beta_x_j", "SD"],
             tau.ci.lb  = sq[ "tau_beta_x_j", "2.5%"],
             tau.median = sq[ "tau_beta_x_j", "50%"],
             tau.ci.ub  = sq[ "tau_beta_x_j", "97.5%"])
} 
