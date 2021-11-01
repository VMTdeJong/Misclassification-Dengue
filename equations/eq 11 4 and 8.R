eq1148 <- function(data) {
  dat <- list(N = nrow(data),
              s = data$s,
              x = data$x,
              y = data$y,
              z = as.matrix(data[, grep("z", colnames(data)), drop = FALSE]),
              V = length(grep("z", colnames(data))),
              j = data$j,
              J = length(unique(data$j)))
  
  params <- Hmisc::Cs(eta_0_0, 
                      eta_z_0,
                      
                      theta_0_0,
                      theta_z_0,
                      
                      psi_0_0, 
                      psi_z_0,
                      
                      omega_0_0,
                      omega_z_0,
                      
                      gamma_0_0,
                      gamma_z_0,
                      
                      beta_0_0,
                      beta_x_0,
                      beta_z_0,
                      
                      sens_x_s,
                      spec_x_s,
                      sens_y_x,
                      spec_y_x,
                      
                      tau_eta_0_j,
                      tau_theta_0_j,
                      tau_psi_0_j,
                      tau_omega_0_j,
                      
                      tau_gamma_0_j,
                      tau_beta_0_j,
                      tau_beta_x_j
  )
  
  load.module("glm", quiet = FALSE)
  run.jags(model = "equations/JAGS eq 11 4 and 8.R",
           data = dat,
           n.chains = n_chains, 
           burnin = n_warmup,
           sample = n_iter,
           adapt = n_adapt,
           monitor = params,
           thin = n_thin,
           method = if (n_chains >= 4) "parallel" else "rjags"
  )
}
