source("src/setup.R")

# 2021-02-24_regression ---------------------------------------------------------

.n <- 10 ^ 3
.fun <- function(x) 2 * x - 1
dat_sample <- tibble(x = rnorm(.n),
                     y = .fun(x) + rnorm(.n))
dat_sample %>% 
  ggplot(aes(x, y)) +
  geom_point() +
  
  # true
  geom_smooth(method = "lm",
              formula = y ~ x) +
  
  # censored
  geom_smooth(data = dat_sample %>% 
                filter(y > 0),
              method = "lm",
              formula = y ~ x) +
  
  # truncated
  geom_smooth(data = dat_sample %>% 
                mutate(y = pmax(0, y)),
              method = "lm",
              formula = y ~ x)

.dat <- list(N = nrow(dat_sample),
             x = dat_sample %>% 
               pull(x),
             y = dat_sample %>% 
               pull(y))

.stan <- rstan::stan("stan/regression.stan", 
                     data = .dat,
                     seed = 1234)

dat_sample_cens <- dat_sample %>% 
  mutate(cens = y <= 0)
.dat_cens <- list(N_obs = dat_sample_cens %>% 
                    filter(!cens) %>% 
                    nrow(),
                  N_cens = dat_sample_cens %>% 
                    filter(cens) %>% 
                    nrow(),
                  x_obs = dat_sample_cens %>% 
                    filter(!cens) %>% 
                    pull(x),
                  y_obs = dat_sample_cens %>% 
                    filter(!cens) %>% 
                    pull(y),
                  x_cens = dat_sample_cens %>% 
                    filter(cens) %>% 
                    pull(x),
                  y_lower = 0)

.stan_cens <- rstan::stan("stan/regression_cens.stan", 
                          data = .dat_cens,
                          seed = 1234)

dat_sample_trunc <- dat_sample %>% 
  filter(y > 0)
.dat_trunc <- list(N_obs = dat_sample_trunc %>% 
                    nrow(),
                   L = 0,
                  x_obs = dat_sample_trunc %>% 
                    pull(x),
                  y_obs = dat_sample_trunc %>% 
                    pull(y))

.stan_trunc <- rstan::stan("stan/regression_trunc.stan", 
                           data = .dat_trunc,
                           seed = 1234)
.stan_trunc
