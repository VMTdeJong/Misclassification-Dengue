# Note: this is a BUGS / JAGS file. It's only saved as .R to get fancy colours in Rstudio.

### Data
## Description
# v = covariate, v = 1,...,V, where 
# V = is number of covariates z.
# z = covariate, not missing, no error assumed.
# i = individual, i = 1,.., n
# n = sample size
# y = outcome, binary, no error assumed. May be missing in some i.
# x = exposure of interest, possibly missing in some i.
# s = surrogate observation of x, i.e. with error. May be missing in some i.
# j = study/cluster, j = 1,...,J
# J = number of studies/clusters.
# Note that values of x, y, or z may be missing and are automatically imputed.

## Expected data format:
# var N, J, V, x[N], y[N], s[N], j[N], z[N, V];

### The model
## Coefficients
# beta   = coefficient in outcome model
# gamma  = coefficient in exposure model
# lambda = coefficient in measurement model, if x_ij = 1
# phi    = coefficient in measurement model, if x_ij = 0
# tau    = precision of random effect

## First subscript
# _0 = intercept
# _x = coefficient for x
# _z = coefficient for z

## Second subscript
# _0 = summary effect / fixed effect
# _j = random effect

## Tau's third subscript:
# tau has one additional subscript (the first) to indicate the coefficient it 
# is the variance of.


model{
  ### The model
  for (i in 1:N) {
    y[i] ~ dbern(y_p[i])
    logit(y_p[i]) <-  beta_0_0 + beta_0_j[j[i]] + beta_x_0 * x[i] + beta_x_j[j[i]] * x[i] + inprod(beta_z_0, z[i, ])
  }
  
  ### Hyper priors
  precision <- .1
  shape     <- .001
  rate      <- .001
  
  ###  Priors
  beta_0_0  ~ dnorm(0.0, precision)
  beta_x_0  ~ dnorm(0.0, precision)
  
  for (v in 1:V) {
    beta_z_0[v] ~ dnorm(0.0, precision)
  }
  # Random intercepts
  for (jj in 1:J) {
    beta_0_j[jj] ~ dnorm(0.0, tau_beta_0_j)
  }
  tau_beta_0_j ~ dgamma(shape, rate)
  # Random effects
  for (jj in 1:J) {
    beta_x_j[jj] ~ dnorm(0.0, tau_beta_x_j)
  }
  tau_beta_x_j ~ dgamma(shape, rate)
  
  # Initial values are taken automatically from R, as follows:
  #inits# beta_0_0, beta_x_0, beta_z_0, beta_0_j, tau_beta_0_j, beta_x_j, tau_beta_x_j
}


