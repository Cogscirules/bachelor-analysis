library(pacman)
p_load(stringr, rethinking, brms, dplyr, ggplot2, gridExtra, mvtnorm, rethinking, metafor, readxl, lme4, data.table, tidyverse, lubridate, groupdata2)


setwd("C:/Users/Bruger/Desktop/Bachelor/bachelor-analysis")

finalfull = read_csv("final_full_df.csv",  col_names = TRUE)
finalfull = finalfull[-c(1)]

finalavg = read_csv("full_avg_med_df.csv",  col_names = TRUE)
finalavg = finalavg[-c(1)]

finalavg_inbeddur = read_csv("finalavg_inbeddur.csv", col_names = TRUE)
finalavg_inbeddur = finalavg_inbeddur[-c(1)]

# ---------------------------- MAKING MODELS

#Intercept model --> sleep score as predicted by how many days they are in hospital
# mu = alpha(sleepscore) + beta(days)
#This can also be duration in sleep, deep, light and rem

#------- BASIC MODELS

m0 <- brms::brm(sleep_score ~ 1 + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(m0)
pp_check(m0)
plot(marginal_effects(m0), points = T)


# duration in bed is predicted by amount of days they are there (because no rows w/ sleepscore include duration in bed)

m1 <- brms::brm(duration_in_bed ~ 1 + (1|day), data = finalavg_inbeddur, family = gaussian, chains = 2, cores = 2)
summary(m1)
pp_check(m1)
plot(marginal_effects(m1), points = T)




# sleepscore as impacted by medication sleep and antidepr

m2 <- brms::brm(sleep_score ~ 1 + (1|sleepmed), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m2)
pp_check(m2)
plot(marginal_effects(m2), points = T)



#the model below doesn't seem to check out
m3 <- brms::brm(sleep_score ~ 1 + (1|antidepressant), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m3)
pp_check(m3)
plot(marginal_effects(m3), points = T)


# amount of time out of bed improves sleep
m4 <- brms::brm(sleep_score ~ 1 + (1|bedexit_duration), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(m4)
pp_check(m4)
plot(marginal_effects(m4), points = T)


# Patients believe they sleep worse than they do
m5 <- brms::brm(sleep_score ~ 1 + (1|csd), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m5)
pp_check(m5)
plot(marginal_effects(m5), points = T)


# ------ MULTIVARIATE MODELS







# fit model
m0.1 <- map2stan(
  alist(
    sleep_score ~ dnorm( mu , sigma ) ,
    mu <- a + bD * day ,
    a ~ dnorm( 0 , 1 ) ,
    bD ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 1 )
  ) , data = finalavg, chains = 2, cores = 2, iter = 1000, warmup = 500)
precis(m0.1)



finalavg = as.data.frame(finalavg)


# ------------- INTERACTION MODELS





#Assessing model quality

m0<-add_ic(m0,ic="waic")
m1 <- add_ic(m1, ic="waic")

compare_ic(m0, m1, ic = "waic")

model_weights(m0, m1, ic = "waic")




# ---------- Trying from the bottom

library(rethinking)


?brms

