# Paths

# Scenario 1
dat_path <- "data/sce_1_data_20191109.RData"

# Scenario 2
# dat_path <- "data/sce_2_data_20191109.RData"
  
# Scenario 3
# dat_path <- "data/sce_3_data_20191108.RData"
  
# all scenarios
est_path <- "estimates.RData"
fit_path <- "fit.RData"
  
# Functions
library(lme4)
source("utils.R")
source("sampling_misclass_re.R")

# Naive bayesian random effects logit models
source("equations/bayes re logit.R")
source("equations/bayes re logit misclass.R")
source("equations/bayes re logit xors.R")

# Bayesian (random effects) logit models that account for misclassification
source("equations/eq 1 2 and 3.R")
source("equations/eq 1 4 and 3.R")
source("equations/eq 5 4 and 3.R")
source("equations/eq 6 4 and 3.R")
source("equations/eq 6 4 and 7.R")
source("equations/eq 6 4 and 8.R")
source("equations/eq 10 4 and 8.R")
source("equations/eq 11 4 and 8.R")


load(dat_path)

# Data parameters
n  <- unique(table(d$j))    # Sample size per cluster (=centers, studies)
J  <- length(table(d$j))    # Number of clusters
BZ <- 1      # Number of covariates measured without error

# JAGS stuff
library(rjags)
library(runjags)
n_chains <- 2    # needs >= 2
n_iter <- 5e3    # needs >= 1e3.
n_adapt <- 1000  # default
n_warmup <- 1000 
n_thin <- 5      

# Initial values
# file requires data parameters listed above.
source("inits.R")

# Sim parameters and initialization
library(abind)
I <- i <- 1

comparison_methods <- c("full", "gold", "goldormisclass", "misclass",
                        "bayes_full", "bayes_gold", "bayes_xors", "bayes_misc")
me_methods <- c("eq123", "eq143", "eq543", "eq643", "eq647", "eq648", "eq1048", "eq1148")
statistics <- c("mean", "se", "ci.lb", "median", "ci.ub", 
                "tau.mean", "tau.se", "tau.ci.lb", "tau.median", "tau.ci.ub")

estimates <- array(dim = c(length(statistics), length(comparison_methods) + length(me_methods), I),
                   dimnames = list(statistic = statistics,
                                   method = c(comparison_methods, me_methods),
                                   I = seq_len(I)))

me_fit_list <- list()

for (i in seq_len(I)) {
  data_full <- data_miss <- d
  data_miss$x <- data_full$x_miss
  data_miss$x_miss <- NULL
  data_full$x_miss <- NULL
  
  ### Frequentist models to check validity
  # If full data were available
  full_fit <- glmer(y ~ x + z + (1 + x | j), family = binomial, data = data_full)
  isSingular(full_fit)
  estimates[c("mean", "median"), "full", i] <- fixef(full_fit)["x"]
  estimates[c("ci.lb", "ci.ub"), "full", i] <- 
    fixef(full_fit)["x"] + qnorm(c(.025, .975)) *  sqrt(vcov(full_fit)["x", "x"])
  estimates[c("se"), "full", i] <- sqrt(vcov(full_fit)["x", "x"])
  
  # Naive method, 
  # use s
  # (if full y is available)
  misclass_fit <- glmer(y ~ s + z + (1 + s | j), family = binomial, data = data_miss)
  isSingular(misclass_fit)
  estimates[c("mean", "median"), "misclass", i] <- fixef(misclass_fit)["s"]
  estimates[c("ci.lb", "ci.ub"), "misclass", i] <- 
    fixef(misclass_fit)["s"] + qnorm(c(.025, .975)) *  sqrt(vcov(misclass_fit)["s", "s"])
  estimates[c("se"), "misclass", i] <- sqrt(vcov(misclass_fit)["s", "s"])
  
  # Naive method,
  # Use true exposure (gold) where possible, error version otherwise 
  # (Only run for when y is fully available. In other cases it is equal to missclassmissy.)
  data_xors <- xors(data_miss, x = "x", s = "s")
  goldormisclass_fit <- glmer(y ~ xors + z + (1 + xors | j), family = binomial, data = data_xors)
  isSingular(goldormisclass_fit)
  estimates[c("mean", "median"), "goldormisclass", i] <- fixef(goldormisclass_fit)["xors"]
  estimates[c("ci.lb", "ci.ub"), "goldormisclass", i] <- 
    fixef(goldormisclass_fit)["xors"] + qnorm(c(.025, .975)) *  sqrt(vcov(goldormisclass_fit)["xors", "xors"])
  estimates[c("se"), "goldormisclass", i] <- sqrt(vcov(goldormisclass_fit)["xors", "xors"])
  
  # Inefficient method: only use data measured without error
  # (only estimable when y is available for obs where x is also available)
  gold_fit <- glmer(y ~ x + z + (1 + x | j), family = binomial, data = data_miss)
  isSingular(gold_fit)
  estimates[c("mean", "median"), "gold", i] <- fixef(gold_fit)["x"]
  estimates[c("ci.lb", "ci.ub"), "gold", i] <- 
    fixef(gold_fit)["x"] + qnorm(c(.025, .975)) *  sqrt(vcov(gold_fit)["x", "x"])
  estimates[c("se"), "gold", i] <- sqrt(vcov(gold_fit)["x", "x"])
  
  ### Bayesian error correction models
  for (me in me_methods) {
    set.seed(20191107)
    begin_me <- proc.time()
    print(paste("Starting method:", me))
    me_fit <- match.fun(me)(data_miss)
    estimates[ , me, i] <- unlist(get_summary_runjags(me_fit))
    if (i == 1)
      me_fit_list[[me]] <- me_fit
    remove(me_fit)
    print(proc.time() - begin_me)
  }
  
  ### For comparison: Methods that cheat or are naive
  begin_comp <- proc.time()
  # xors
  set.seed(20191107)
  print(paste("Starting method: bayes_xors"))
  me_xors_fit <- bayes_re_logit_xors(data_miss)
  estimates[ , "bayes_xors", i] <- unlist(get_summary_runjags(me_xors_fit))
  if (i == 1)
    me_fit_list[["bayes_xors"]] <- me_xors_fit
  
  # Gold only
  set.seed(20191107)
  data_gold_bayes <- data_miss[!is.na(data_miss$x), ]
  J_temp <- J
  J <- length(unique(data_gold_bayes$j)) # Because it uses J from the global env for the inits!
  print(paste("Starting method: bayes_gold"))
  me_gold_fit <- bayes_re_logit(data_gold_bayes, cca = TRUE)
  estimates[ , "bayes_gold", i] <- unlist(get_summary_runjags(me_gold_fit))
  if (i == 1)
    me_fit_list[["bayes_gold"]] <- me_gold_fit
  J <- J_temp # Set back to J for full data, for other methods.
  
  # Full data
  set.seed(20191107)
  print(paste("Starting method: bayes_full"))
  me_full_fit <- bayes_re_logit(data_full)
  estimates[ , "bayes_full", i] <- unlist(get_summary_runjags(me_full_fit))
  if (i == 1)
    me_fit_list[["bayes_full"]] <- me_full_fit
  
  # Misclassified x (automatically takes s)
  set.seed(20191107)
  print(paste("Starting method: bayes_misc"))
  me_misc_fit <- bayes_re_logit_misclass(data_full) 
  estimates[ , "bayes_misc", i] <- unlist(get_summary_runjags(me_misc_fit))
  if (i == 1)
    me_fit_list[["bayes_misc"]] <- me_misc_fit
  
  print(proc.time() - begin_comp)
  
  save(estimates, file = est_path)
}

save(me_fit_list, file = fit_path)

tab <- with(data_full, table(x, s))
tab[2, 2]/ sum(tab[2, ])
tab[1, 1]/ sum(tab[1, ])
