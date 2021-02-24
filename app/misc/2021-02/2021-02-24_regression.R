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
