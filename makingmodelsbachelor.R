library(pacman)
p_load(stringr, rethinking, brms, dplyr, ggplot2, gridExtra, mvtnorm, rethinking, metafor, readxl, lme4, data.table, tidyverse, lubridate, groupdata2)


setwd("C:/Users/Bruger/Desktop/Bachelor/bachelor-analysis")

finalfull = read_csv("final_full_df.csv",  col_names = TRUE)
finalfull = finalfull[-c(1)]

finalavg = read_csv("finalavg.csv",  col_names = TRUE)
finalavg = finalavg[-c(1)]

#finalavg = read_csv("final_avg_no_na.csv",  col_names = TRUE)
#finalavg = finalavg[-c(1)]

finalavg_inbeddur = read_csv("finalavg_inbeddur.csv", col_names = TRUE)
finalavg_inbeddur = finalavg_inbeddur[-c(1)]

# ---------------------------- EXTRACTING INFORMATION

#age 
mean
min
max(finalavg$age)

#5 women
#3 male






# ---------------------------- MAKING MODELS

#Intercept model --> sleep score as predicted by how many days they are in hospital
# mu = alpha(sleepscore) + beta(days)
#This can also be duration in sleep, deep, light and rem

#------- BASIC MODELS

#sleep score as predicted by days
m0 <- brms::brm(sleep_score ~ 1 + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(m0)
pp_check(m0)
plot(marginal_effects(m0), points = T)



# sleep score as predicted by consensus sleep diary
m1 <- brms::brm(sleep_score ~ 1 + csd + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m1)
pp_check(m1)
plot(marginal_effects(m1), points = T)



# ------ INTERACTION MODELS

# sleep score as predicted by interaction between duration in sleep and sleepmed
m2 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m2)
pp_check(m2)
plot(marginal_effects(m2), points = T)



# sleep score as predicted by interaction between duration in sleep and antidepressant
m3 <- brms::brm(bf(sleep_score ~ 1 + duration_in_sleep * antidepressant + (1|day)), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m3)
pp_check(m3)
plot(marginal_effects(m3), points = T)


# sleep score as predicted by Interaction with both medications
m4<- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m4)
pp_check(m4)
plot(marginal_effects(m4), points = T)



# Sleep score as predicted by interaction between both medications
m5<- brms::brm(sleep_score ~ 1 + sleepmed * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m5)
pp_check(m5)
plot(marginal_effects(m5), points = T)



#Sleep score as predicted by interaction between duration in sleep and rem
m6<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m6)
pp_check(m6)
plot(marginal_effects(m6), points = T)

#Sleep score as predicted by duration in sleep and duration in deep
m7<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m7)
pp_check(m7)
plot(marginal_effects(m7), points = T)


#Sleep score as predicted by interaction between sleep med and duration in deep
m8<- brms::brm(sleep_score ~ 1 + sleepmed * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m8)
pp_check(m8)
plot(marginal_effects(m8), points = T)

#sleep score as predicted by interaction between sleep med and rem
m9<- brms::brm(sleep_score ~ 1 + sleepmed * duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m9)
pp_check(m9)
plot(marginal_effects(m9), points = T)


#sleep score as predicted by interaction between antidepressant and deep
m10<- brms::brm(sleep_score ~ 1 + antidepressant * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m10)
pp_check(m10)
plot(marginal_effects(m10), points = T)

#sleep score as predicted by interaction between antipressant and rem
m11<- brms::brm(sleep_score ~ 1 + antidepressant * duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m11)
pp_check(m11)
plot(marginal_effects(m11), points = T)


# ------------- USING NO NA DF FOR ASSESSING WEIGHT



#Intercept model --> sleep score as predicted by how many days they are in hospital
# mu = alpha(sleepscore) + beta(days)
#This can also be duration in sleep, deep, light and rem

#------- BASIC MODELS

#sleep score as predicted by days
mm0 <- brms::brm(sleep_score ~ 1 + (1|day), data = DF, family = gaussian, chains = 2, cores = 2)

# sleep score as predicted by consensus sleep diary
mm1 <- brms::brm(sleep_score ~ 1 + csd + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

# ------ INTERACTION MODELS

# sleep score as predicted by interaction between duration in sleep and sleepmed
mm2 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

# sleep score as predicted by interaction between duration in sleep and antidepressant
mm3 <- brms::brm(bf(sleep_score ~ 1 + duration_in_sleep * antidepressant + (1|day)), data = DF, family = gaussian, chains = 2, cores = 2) 


# sleep score as predicted by Interaction with both medications
mm4<- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed * antidepressant + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


# Sleep score as predicted by interaction between both medications
mm5<- brms::brm(sleep_score ~ 1 + sleepmed * antidepressant + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#Sleep score as predicted by interaction between duration in sleep and rem
mm6<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_rem + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#Sleep score as predicted by duration in sleep and duration in deep
mm7<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#Sleep score as predicted by interaction between sleep med and duration in deep
mm8<- brms::brm(sleep_score ~ 1 + sleepmed * duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#sleep score as predicted by interaction between sleep med and rem
mm9<- brms::brm(sleep_score ~ 1 + sleepmed * duration_in_rem + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#sleep score as predicted by interaction between antidepressant and deep
mm10<- brms::brm(sleep_score ~ 1 + antidepressant * duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#sleep score as predicted by interaction between antipressant and rem
mm11<- brms::brm(sleep_score ~ 1 + antidepressant * duration_in_rem + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#finalavg = as.data.frame(finalavg)


# ------------- INTERACTION MODELS




#Assessing model quality

mm0<-add_ic(mm0,ic="waic")
mm1<- add_ic(mm1, ic="waic")
mm2<- add_ic(mm2, ic="waic")
mm3<- add_ic(mm3, ic="waic")
mm4<- add_ic(mm4, ic="waic")
mm5<- add_ic(mm5, ic="waic")
mm6<- add_ic(mm6, ic="waic")
mm7<- add_ic(mm7, ic="waic")
mm8<- add_ic(mm8, ic="waic")
mm9<- add_ic(mm9, ic="waic")
mm10<- add_ic(mm10, ic="waic")
mm11<- add_ic(mm11, ic="waic")







#Try this with the original df
round(model_weights(m0, m1, m2, m3, m5, m6, m7, m8, m9, m10, m11, ic = "waic"),3)



weights = round(model_weights(m0, m1, m2, m3, m4,m5, m6, m7, m9, m10, m11, ic = "waic"),3)

#lets test model 1 and 4 too --> They still don't work
round(model_weights(m0, m1, m2, m3, m5, m5.1, m6, m7, m9, m10, m11, m12, m13, ic = "waic"),3)
round(model_weights(m0, m4, m2, m3, m5, m5.1, m6, m7, m9, m10, m11, m12, m13, ic = "waic"),3)

comparison = compare_ic(m0, m2, m3, m5, m5.1, m6, m7, m9, m10, m11, m12, m13, ic = "waic")

comparison


WAIC(m0,m2,m3,m5, se = FALSE) 
WAIC(m0, m2, m3, m5, m5.1, m6, m7, m9, m10, m11, m12, m13, se = FALSE)




