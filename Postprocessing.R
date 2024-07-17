#install.packages("RNetCDF")
library(RNetCDF)
library(tidyverse)

nc <- open.nc("/Users/irisyang/Downloads/copy_model_age_time_interaction_samples.nc")
print(nc)
vars <- c("slope", "age_drift", "age_time_drift")

N_t <- 31
time <- 1990:2020
N_age <- 18
ages <- seq(0, 85, 5)
N_draws <- 500
N_chains <- 2

posterior <- list()
for (var in vars) {
  #dim(var.get.nc(nc, var)) = (..., draws, chains)
  posterior[[var]] <- var.get.nc(nc, var)
  print(dim(var.get.nc(nc, var)))
}
posterior
# relevant line from the file
# latent_rate = slope_cum + age_effect + age_time_effect
# posterior$age_effect <- apply(X = posterior$age_drift, MARGIN = c(2, 3), FUN = cumsum)
# posterior$age_time_effect <- apply(X = posterior$age_time_drift, MARGIN = c(2, 3, 4), FUN = cumsum)
posterior$age_effect <- apply(X = posterior$age_drift, MARGIN = c(2, 3), FUN = cumsum)
posterior$age_time_effect <- apply(X = posterior$age_time_drift, MARGIN = c(2, 3, 4), FUN = cumsum)

dim(posterior$age_drift)

latent_rate <- array(
  data = NA,
  dim = c(N_age, N_t, N_draws, N_chains),
  dimnames = list(
    ages,
    time,
    1:N_draws,
    1:N_chains
  )
)

for (a in 1:N_age) {
  # first year
  latent_rate[a, 1, , ] <- posterior$age_effect[a, , ]
  # all other years
  for (t in 2:N_t) {
    latent_rate[a, t, , ] <- posterior$slope * (t - 1) + posterior$age_time_effect[t-1, a, , ]
  }
}

#latent_rate
write_rds(
  # optionally transform back to outcome space
  plogis(latent_rate),
  "model_age_time_interaction_rate.rds"
)
