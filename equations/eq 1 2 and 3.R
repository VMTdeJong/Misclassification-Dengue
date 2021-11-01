eq123 <- function(data) {
  dat <- list(N = nrow(data),
              s = data$s,
              x = data$x,
              y = data$y,
              z = as.matrix(data[, grep("z", colnames(data)), drop = FALSE]),
              V = length(grep("z", colnames(data))))
  
  params <- Hmisc::Cs(lambda_0_0,
                      phi_0_0,
                      
                      gamma_0_0,
                      gamma_z_0,
                      
                      beta_0_0,
                      beta_x_0,
                      beta_z_0,
                      
                      sens_x_s,
                      spec_x_s,
                      sens_y_x,
                      spec_y_x
  )
  
  load.module("glm", quiet = FALSE)
  run.jags(model = "equations/JAGS eq 1 2 and 3.R",
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



