library(pacman)
p_load(stringr, rethinking, brms, dplyr, ggplot2, gridExtra, mvtnorm, rethinking, metafor, readxl, lme4, data.table, tidyverse, lubridate, groupdata2)


setwd("C:/Users/Bruger/Desktop/Bachelor/bachelor-analysis")



# ---------------------------- MAKING MODELS


#Model with Search term only
M1 <- map2stan(
  alist(
    sentiment ~ dnorm(mu, sigma),
    mu <- a + duration_in_sleep*sleep_score,
    a ~ dnorm(0,1),
    duration_in_sleep ~ dnorm(0,1),
    sigma ~ dnorm(0,1)
  ), data = newdf, chains = 2, cores = 2, iter = 1000, warmup = 500
)
precis(M1)


m0 <- brms::brm(duration_in_bed ~ 1 + duration_in_sleep + (1|sleep_score), data = fulldf, family = gaussian, chains = 2, cores = 2)
summary(m0)
pp_check(m0)
plot(marginal_effects(m0), points = T)

m1 <- brms::brm(duration_in_bed ~ 1 + duration_in_sleep + (1|sleep_score), data = fulldf2, family = gaussian, chains = 2, cores = 2)
summary(m1)
pp_check(m1)
plot(marginal_effects(m1), points = T)

m0<-add_ic(m0,ic="waic")
m1 <- add_ic(m1, ic="waic")

compare_ic(m0, m1, ic = "waic")

model_weights(m0, m1, ic = "waic")