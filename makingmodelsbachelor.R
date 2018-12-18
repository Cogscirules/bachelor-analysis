library(pacman)
p_load(stringr, rethinking, brms, dplyr, ggplot2, gridExtra, mvtnorm, rethinking, metafor, readxl, lme4, data.table, tidyverse, lubridate, groupdata2)


setwd("C:/Users/Bruger/Desktop/Bachelor/bachelor-analysis")

finalfull = read_csv("final_full_df.csv",  col_names = TRUE)
finalfull = finalfull[-c(1)]

View(finalfull)

finalavg = read_csv("finalavg.csv",  col_names = TRUE)
finalavg = finalavg[-c(1)]

f = read_csv("full_avg_med_df.csv", col_names = TRUE)

View(f)

#finalavg = read_csv("final_avg_no_na.csv",  col_names = TRUE)
#finalavg = finalavg[-c(1)]

finalavg_inbeddur = read_csv("finalavg_inbeddur.csv", col_names = TRUE)
finalavg_inbeddur = finalavg_inbeddur[-c(1)]

# ---------------------------- EXTRACTING INFORMATION

#age 
mean
min
max(finalavg$age)

#6 women
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
pp2 = pp_check(m2)
pp2 + theme_classic()
marg2 = plot(marginal_effects(m2), points = T)



# sleep score as predicted by interaction between duration in sleep and antidepressant
m3 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m3)
pp_check(m3)
plot(marginal_effects(m3), points = T)



# Sleep score as predicted by interaction between both medications
m5<- brms::brm(sleep_score ~ 1 + sleepmed * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m5)
pp_check(m5)
plot(marginal_effects(m5), points = T)



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

# sleep score as predicted by consensus sleep diary, medication, duration in sleep, rem and deep
mm1 <- brms::brm(sleep_score ~ 1 + csd + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

mm2 <- brms::brm(sleep_score ~ 1 + sleepmed + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

mm3 <- brms::brm(sleep_score ~ 1 + antidepressant + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

mm4 <- brms::brm(sleep_score ~ 1 + duration_in_sleep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

mm5 <- brms::brm(sleep_score ~ 1 + duration_in_rem + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

mm6 <- brms::brm(sleep_score ~ 1 + duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

# ------ INTERACTION MODELS

# sleep score as predicted by interaction between duration in sleep and sleepmed
mm7 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

# sleep score as predicted by interaction between duration in sleep and antidepressant
mm8 <- brms::brm(bf(sleep_score ~ 1 + duration_in_sleep * antidepressant + (1|day)), data = DF, family = gaussian, chains = 2, cores = 2) 


# sleep score as predicted by Interaction with both medications
mm9<- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed * antidepressant + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


# Sleep score as predicted by interaction between both medications
mm10<- brms::brm(sleep_score ~ 1 + sleepmed * antidepressant + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#Sleep score as predicted by interaction between duration in sleep and rem
mm11<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_rem + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#Sleep score as predicted by duration in sleep and duration in deep
mm12<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#Sleep score as predicted by interaction between sleep med and duration in deep
mm13<- brms::brm(sleep_score ~ 1 + sleepmed * duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#sleep score as predicted by interaction between sleep med and rem
mm14<- brms::brm(sleep_score ~ 1 + sleepmed * duration_in_rem + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#sleep score as predicted by interaction between antidepressant and deep
mm15<- brms::brm(sleep_score ~ 1 + antidepressant * duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#sleep score as predicted by interaction between antipressant and rem
mm16<- brms::brm(sleep_score ~ 1 + antidepressant * duration_in_rem + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 




#finalavg = as.data.frame(finalavg)


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
mm12<- add_ic(mm12, ic="waic")
mm13<- add_ic(mm13, ic="waic")
mm14<- add_ic(mm14, ic="waic")
mm15<- add_ic(mm15, ic="waic")
mm16<- add_ic(mm16, ic="waic")



#Try this with the original df
round(model_weights(mm0, mm1, mm2, mm3, mm5, mm6, mm7, mm8, mm9, mm10, mm11, mm12, mm13, mm14, mm15, mm16, ic = "waic"),3)

weights = round(model_weights(mm0, mm1, mm2, mm3, mm5, mm6, mm7, mm8, mm9, mm10, mm11, mm12, mm13, mm14, mm15, mm16, ic = "waic"),3)
weights

weights2 = round(model_weights(mm5, mm6, mm7, mm9, mm11, mm12, mm13, mm15, ic = "waic"),3)
weights2

comparison = compare_ic(mm5, mm6, mm9, mm7, mm11, mm12, mm13, mm15, ic = "waic")
comparison


WAIC(mm0, mm1, mm2, mm3, mm5, mm6, mm7, mm8, mm9, mm10, mm11, se = FALSE) 


# ------------------ Models with most weight from low to high


# sleep score as predicted by Interaction with both medications
m4<- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m4)
pp4 = pp_check(m4)
pp4 + theme_classic()
marg4 = plot(marginal_effects(m4), points = T)
plot(m4)


# sleep score as predicted by interaction between duration in sleep and sleepmed
m2 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m2)
pp2 = pp_check(m2)
pp2 + theme_classic()
marg2 = plot(marginal_effects(m2), points = T)



# sleep score as predicted by interaction between duration in sleep and rem
m6<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m6)
pp6 = pp_check(m6)
pp6 + theme_classic()
marg6 = plot(marginal_effects(m6), points = T)


# sleep score as predicted by duration in sleep and duration in deep
m7<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m7)
pp7 = pp_check(m7)
pp7 + theme_classic()
marg7 = plot(marginal_effects(m7), points = T)



# ------------- INTERACTION MODELS FOR MOVEMENT MODELS

# sleep score as predicted by movement

mo1 <- brms::brm(sleep_score ~ 1 + avg_act + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo1)
pp_check(m01)
plot(marginal_effects(m01), points = T)


mo2 <- brms::brm(sleep_score ~ 1 + duration_in_bed + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(m4)
pp_check(m4)
plot(marginal_effects(m4), points = T)


mo3 <- brms::brm(sleep_score ~ 1 + bedexit_duration + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo3)
pp_check(mo3)
plot(marginal_effects(mo3), points = T)

#Interaction

mo4 <- brms::brm(sleep_score ~ 1 + bedexit_duration * duration_in_bed + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo4)
pp_check(mo4)
plot(marginal_effects(mo4), points = T)

mo5 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_bed + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo5)
pp_check(mo5)
plot(marginal_effects(mo5), points = T)


mo6 <- brms::brm(sleep_score ~ 1 + bedexit_duration * duration_in_sleep + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo6)
pp_check(mo6)
plot(marginal_effects(mo6), points = T)

mo7 <- brms::brm(sleep_score ~ 1 + tossnturn_count + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo7)
pp_check(mo7)
plot(marginal_effects(mo7), points = T)

mo8 <- brms::brm(sleep_score ~ 1 + tossnturn_count * duration_in_sleep + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo8)
pp_check(mo8)
plot(marginal_effects(mo8), points = T)


# ------------------- MODEL WEIGHT

#Assessing model quality

mo1<- add_ic(mo1, ic="waic")
mo2<- add_ic(mo2, ic="waic")
mo3<- add_ic(mo3, ic="waic")
mo4<- add_ic(mo4, ic="waic")
mo5<- add_ic(mo5, ic="waic")
mo6<- add_ic(mo6, ic="waic")
mo7<- add_ic(mo7, ic="waic")
mo8<- add_ic(mo8, ic="waic")




#Try this with the original df
round(model_weights(mo1, mo2, mo3, mo5, mo6, mo7, mo8, ic = "waic"),3)

weights3 = round(model_weights(mo1, mo2, mo3, mo5, mo6, mo7, mo8, ic = "waic"),3)
weights3

weights4 = round(model_weights(mo2, mo5, mo6, mo8, ic = "waic"),3)
weights4

comparison2 = compare_ic(mo2, mo5, mo6, mo8, ic = "waic")
comparison2

