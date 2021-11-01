
# The inits are specified as functions, so that the chains can receive different values,
# and we can easily specify random values for each chain, in each different simulation
# Note that the first chain gets the mean of the distribution and the other chains a 
# random draw from the distribution. The exception is the gamma distribution: i'm not 
# yet sure what the parametrization in jags is. So i chose some stuff from a simulation.

init_sd <- 2 # 95% of log-odds ratios will range from -4 to 4.

### Coefficients
## Scalar
eta_0_0 <- 
  theta_0_0 <- 
  psi_0_0  <- 
  omega_0_0 <- 
  lambda_0_0 <- 
  lambda_y_0 <- 
  phi_0_0 <- 
  phi_y_0 <- 
  gamma_0_0 <- 
  beta_0_0  <- 
  beta_x_0 <- 
  gamma_s_0 <- function(chain) if (chain == "1") 0 else rnorm(1, sd = init_sd)
   
## Vectors
eta_z_0 <- 
  theta_z_0 <- 
  psi_z_0 <- 
  omega_z_0 <- 
  lambda_z_0 <- 
  phi_z_0 <- 
  gamma_z_0 <- 
  beta_z_0 <- function(chain) if (chain == "1") rep(0, BZ) else rnorm(BZ, sd = init_sd)

eta_0_j <- 
  theta_0_j <- 
  psi_0_j <- 
  omega_0_j <- 
  lambda_0_j <- 
  phi_0_j <- 
  gamma_0_j <- 
  beta_0_j <- 
  beta_x_j <- function(chain) if (chain == "1") rep(0, J) else rnorm(J, sd = init_sd)
  
### Variances
## Scalar
tau_gamma_0_j <- 
  tau_beta_0_j <- 
  tau_beta_x_j <- 
  
  tau_eta_0_j <- 
  tau_theta_0_j <- 
  tau_psi_0_j <- 
  tau_omega_0_j <- 
  
  tau_lambda_0_j <- 
  tau_phi_0_j <- function(chain) if (chain == "1") 1 else rgamma(1, 20, 1/10)


