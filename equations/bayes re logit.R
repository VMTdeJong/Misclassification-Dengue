bayes_re_logit <- function(data, cca = FALSE) {
  
  
  if (cca) {
    data <- data[!apply(data, 1, anyNA), ]
    data$j <- as.numeric(as.factor(data$j))
  }

  
  dat <- list(N = nrow(data),
              x = data$x,
              y = data$y,
              z = as.matrix(data[, grep("z", colnames(data)), drop = FALSE]),
              V = length(grep("z", colnames(data))),
              j = data$j,
              J = length(unique(data$j)))
  
  params <- Hmisc::Cs(beta_0_0,
                      beta_x_0,
                      beta_z_0,
                      
                      tau_beta_0_j,
                      tau_beta_x_j
  )
  
  load.module("glm", quiet = FALSE)
  run.jags(model = "equations/JAGS bayes re logit.R",
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
