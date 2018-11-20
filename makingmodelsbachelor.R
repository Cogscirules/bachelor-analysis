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




# sleepscore as impacted by medication sleep and antidepr pr day

m2 <- brms::brm(sleep_score ~ 1 + sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m2)
pp_check(m2)
plot(marginal_effects(m2), points = T)



#the model below doesn't seem to check out
m3 <- brms::brm(sleep_score ~ 1 + antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m3)
pp_check(m3)
plot(marginal_effects(m3), points = T)


# amount of time out of bed improves sleep
m4 <- brms::brm(sleep_score ~ 1 + bedexit_duration + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(m4)
pp_check(m4)
plot(marginal_effects(m4), points = T)


# Patients believe they sleep worse than they do
m5 <- brms::brm(sleep_score ~ 1 + csd + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m5)
pp_check(m5)
plot(marginal_effects(m5), points = T)

# sleepscore and hr and rr
m5.1 <- brms::brm(sleep_score ~ 1 + avg_hr + avg_rr + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m5.1)
pp_check(m5.1)
plot(marginal_effects(m5.1), points = T)


# ------ INTERACTION MODELS

#interaction between duration in sleep and sleepmed
m6 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m6)
pp_check(m6)
plot(marginal_effects(m6), points = T)

#interaction between duration in sleep and antidepressant
m7 <- brms::brm(bf(sleep_score ~ 1 + duration_in_sleep * antidepressant + (1|day)), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m7)
pp_check(m7)
plot(marginal_effects(m7), points = T)



#interaction between sleepscore and RR
m8 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * avg_hr + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m8)
pp_check(m8)
plot(marginal_effects(m8), points = T)

m9 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * avg_rr + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m9)
pp_check(m9)
plot(marginal_effects(m9), points = T)

m10 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * avg_rr * avg_hr + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m10)
pp_check(m10)
plot(marginal_effects(m10), points = T)


# Interaction with both medications
m11<- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m11)
pp_check(m11)
plot(marginal_effects(m11), points = T)



# Make above models without duration in sleep

m12 <- brms::brm(sleep_score ~ 1 + avg_rr * avg_hr + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m12)
pp_check(m12)
plot(marginal_effects(m12), points = T)

m13<- brms::brm(sleep_score ~ 1 + sleepmed * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m13)
pp_check(m13)
plot(marginal_effects(m13), points = T)




#Make a model about respiration rate and sleep in REM, Light and Deep sleep




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
m1<- add_ic(m1, ic="waic")
m2<- add_ic(m2, ic="waic")
m3<- add_ic(m3, ic="waic")
m4<- add_ic(m4, ic="waic")
m5<- add_ic(m5, ic="waic")
m5.1<- add_ic(m5.1, ic="waic")
m6<- add_ic(m6, ic="waic")
m7<- add_ic(m7, ic="waic")
m8<- add_ic(m8, ic="waic")
m9<- add_ic(m9, ic="waic")
m10<- add_ic(m10, ic="waic")
m11<- add_ic(m11, ic="waic")
m12<- add_ic(m12, ic="waic")
m13<- add_ic(m13, ic="waic")

round(model_weights(m0, m1, m2, m3, m5, m5.1, m6, m7, m8, m9, m10, m11, m12, m13, ic = "waic"),3)

compare_ic(m0, m1, m2, m3, m5, m5.1, m6, m7, m8, m9, m10, m11, m12, m13, ic = "waic")

model_weights(m0, m1, m2, ic = "waic")


WAIC(m0,m1,m2,m3,m4,m5, se = FALSE)



# ---------- Trying from the bottom

library(rethinking)


?brms

