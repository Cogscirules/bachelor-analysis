library(pacman)
p_load(DMwR, cmce, stringr, rethinking, brms, dplyr, ggplot2, gridExtra, mvtnorm, rethinking, metafor, readxl, lme4, data.table, tidyverse, lubridate, groupdata2)


setwd("C:/Users/Bruger/Desktop/Bachelor/bachelor-analysis")

finalfull = read_csv("final_full_df.csv",  col_names = TRUE)
finalfull = finalfull[-c(1)]

View(finalfull)

finalavg = read_csv("finalavg.csv",  col_names = TRUE)
finalavg = finalavg[-c(1)]

f = read_csv("full_avg_med_df.csv", col_names = TRUE)
f = f[-c(1)]
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

m1 <- brms::brm(sleep_score ~ 1 + day + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(m1)
pp_check(m1)
plot(marginal_effects(m1), points = T)



# sleep score as predicted by consensus sleep diary
m2 <- brms::brm(sleep_score ~ 1 + csd + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m2)
pp_check(m2)
plot(marginal_effects(m2), points = T)


m3 <- brms::brm(sleep_score ~ 1 + day + csd + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(m3)
pp_check(m3)
plot(marginal_effects(m3), points = T)

m4 <- brms::brm(sleep_score ~ 1 + day + sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(m4)
pp_check(m4)
plot(marginal_effects(m4), points = T)


m5 <- brms::brm(sleep_score ~ 1 + day + antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(m5)
pp_check(m5)
plot(marginal_effects(m5), points = T)


# ------ INTERACTION MODELS


# sleep score as predicted by interaction between duration in sleep and sleepmed
m6 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m6)
pp2 = pp_check(m6)
pp2 + theme_classic()
marg6 = plot(marginal_effects(m6), points = T)



# sleep score as predicted by interaction between duration in sleep and antidepressant
m7 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m7)
pp_check(m7)
plot(marginal_effects(m7), points = T)



# Sleep score as predicted by interaction between both medications
m8<- brms::brm(sleep_score ~ 1 + sleepmed * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m8)
pp_check(m8)
plot(marginal_effects(m8), points = T)



#Sleep score as predicted by interaction between sleep med and duration in deep
m9<- brms::brm(sleep_score ~ 1 + sleepmed * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m9)
pp_check(m9)
plot(marginal_effects(m9), points = T)

#sleep score as predicted by interaction between sleep med and rem
m10<- brms::brm(sleep_score ~ 1 + sleepmed * duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m10)
pp_check(m10)
plot(marginal_effects(m10), points = T)


#sleep score as predicted by interaction between antidepressant and deep
m11<- brms::brm(sleep_score ~ 1 + antidepressant * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m10)
pp_check(m10)
plot(marginal_effects(m10), points = T)

#sleep score as predicted by interaction between antipressant and rem
m12<- brms::brm(sleep_score ~ 1 + antidepressant * duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m11)
pp_check(m11)
plot(marginal_effects(m11), points = T)



# sleep score as predicted by Interaction with both medications
m13 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m12)
pp4 = pp_check(m12)
pp4 + theme_classic()
marg4 = plot(marginal_effects(m12), points = T)
plot(m12)


# sleep score as predicted by interaction between duration in sleep and sleepmed
m14 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m13)
pp2 = pp_check(m13)
pp2 + theme_classic()
marg2 = plot(marginal_effects(m13), points = T)



# sleep score as predicted by interaction between duration in sleep and rem
m15<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m14)
pp6 = pp_check(m14)
pp6 + theme_classic()
marg6 = plot(marginal_effects(m14), points = T)


# sleep score as predicted by duration in sleep and duration in deep
m16<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m15)
pp7 = pp_check(m15)
pp7 + theme_classic()
marg7 = plot(marginal_effects(m15), points = T)

#sleep scores and day 
m17<- brms::brm(sleep_score ~ 1 + day * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m16)
pp7 = pp_check(m16)
pp7 + theme_classic()
marg7 = plot(marginal_effects(m16), points = T)


m18<- brms::brm(sleep_score ~ 1 + day * duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m17)
pp7 = pp_check(m17)
pp7 + theme_classic()
marg7 = plot(marginal_effects(m15), points = T)

m19<- brms::brm(sleep_score ~ 1 + day * sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m18)
pp7 = pp_check(m18)
pp7 + theme_classic()
marg7 = plot(marginal_effects(m15), points = T)

m20<- brms::brm(sleep_score ~ 1 + day * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m19)
pp7 = pp_check(m19)
pp7 + theme_classic()
marg7 = plot(marginal_effects(m15), points = T)

m21<- brms::brm(sleep_score ~ 1 + day * antidepressant * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(m20)
pp7 = pp_check(m20)
pp7 + theme_classic()
marg7 = plot(marginal_effects(m15), points = T)



# ------------- USING NO NA DF FOR ASSESSING WEIGHT




#Intercept model --> sleep score as predicted by how many days they are in hospital
# mu = alpha(sleepscore) + beta(days)
#This can also be duration in sleep, deep, light and rem

#------- BASIC MODELS


#sleep score as predicted by days
mm0 <- brms::brm(sleep_score ~ 1 + (1|day), data = DF, family = gaussian, chains = 2, cores = 2)

mm1 <- brms::brm(sleep_score ~ 1 + day + (1|day), data = DF, family = gaussian, chains = 2, cores = 2)
ma1 <- brms::brm(sleep_score ~ 1 + day + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(ma1)
pp_ma1 = pp_check(ma1)
pp_ma1 + theme_classic()
marg_ma1 = plot(marginal_effects(ma1), points = T)

hyp1 <- brms::brm(duration_in_sleep ~ 1 + day + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(hyp1)
pphyp1 = pp_check(hyp1)
pphyp1 + theme_classic()
marghyp1 = plot(marginal_effects(hyp1), points = T)

# sleep score as predicted by consensus sleep diary, medication, duration in sleep, rem and deep
mm2 <- brms::brm(sleep_score ~ 1 + csd + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

mm3 <- brms::brm(sleep_score ~ 1 + sleepmed + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

mm4 <- brms::brm(sleep_score ~ 1 + antidepressant + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

mm5 <- brms::brm(sleep_score ~ 1 + duration_in_sleep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

mm6 <- brms::brm(sleep_score ~ 1 + duration_in_rem + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

mm7 <- brms::brm(sleep_score ~ 1 + duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


mm8 <- brms::brm(sleep_score ~ 1 + day + duration_in_sleep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2)
ma8 <- brms::brm(sleep_score ~ 1 + day + duration_in_sleep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(ma8)
pp_ma8 = pp_check(ma8)
pp_ma8 + theme_classic()
marg_ma8 = plot(marginal_effects(ma8), points = T) + theme_classic()


mm9 <- brms::brm(sleep_score ~ 1 + day + sleepmed + (1|day), data = DF, family = gaussian, chains = 2, cores = 2)
"""ma9 <- brms::brm(sleep_score ~ 1 + day + sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(ma9)
pp_ma9 = pp_check(ma9)
pp_ma9 + theme_classic()
marg_ma9 = plot(marginal_effects(ma9), points = T)"""


mm10 <- brms::brm(sleep_score ~ 1 + day + antidepressant + (1|day), data = DF, family = gaussian, chains = 2, cores = 2)
"""ma10 <- brms::brm(sleep_score ~ 1 + day + antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(ma10)
pp_ma10 = pp_check(ma10)
pp_ma10 + theme_classic()
marg_ma10 = plot(marginal_effects(ma10), points = T)"""


# ------ INTERACTION MODELS

# sleep score as predicted by interaction between duration in sleep and sleepmed
mm11 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

# sleep score as predicted by interaction between duration in sleep and antidepressant
mm12 <- brms::brm(bf(sleep_score ~ 1 + duration_in_sleep * antidepressant + (1|day)), data = DF, family = gaussian, chains = 2, cores = 2) 


# sleep score as predicted by Interaction with both medications
mm13<- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed * antidepressant + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


# Sleep score as predicted by interaction between both medications
mm14<- brms::brm(sleep_score ~ 1 + sleepmed * antidepressant + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#Sleep score as predicted by interaction between duration in sleep and rem
mm15<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_rem + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#Sleep score as predicted by duration in sleep and duration in deep
mm16<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 

ma16<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(ma16)
pp_ma16 = pp_check(ma16)
pp_ma16 + theme_classic()
marg_ma16 = plot(marginal_effects(ma16), points = T) - theme_classic()



#Sleep score as predicted by interaction between sleep med and duration in deep
mm17<- brms::brm(sleep_score ~ 1 + sleepmed * duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#sleep score as predicted by interaction between sleep med and rem
mm18<- brms::brm(sleep_score ~ 1 + sleepmed * duration_in_rem + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#sleep score as predicted by interaction between antidepressant and deep
mm19<- brms::brm(sleep_score ~ 1 + antidepressant * duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#sleep score as predicted by interaction between antipressant and rem
mm20<- brms::brm(sleep_score ~ 1 + antidepressant * duration_in_rem + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


#sleep scores and day 
mm21<- brms::brm(sleep_score ~ 1 + day * duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 
ma21<- brms::brm(sleep_score ~ 1 + day * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(ma21)
pp_ma21 = pp_check(ma21)
pp_ma21 + theme_classic()
marg_ma21 = plot(marginal_effects(ma21), points = T)+ theme_classic()

mm22<- brms::brm(sleep_score ~ 1 + day * duration_in_rem + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 


mm23<- brms::brm(sleep_score ~ 1 + day * sleepmed + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 
"""ma23<- brms::brm(sleep_score ~ 1 + day * sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(ma23)
pp_ma23 = pp_check(ma23)
pp_ma23 + theme_classic()
marg_ma23 = plot(marginal_effects(ma23), points = T)"""

mm24<- brms::brm(sleep_score ~ 1 + day * antidepressant + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 
ma24<- brms::brm(sleep_score ~ 1 + day * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(ma24)
pp_ma24 = pp_check(ma24)
pp_ma24 + theme_classic()
marg_ma24 = plot(marginal_effects(ma24), points = T)+theme_classic()


mm25<- brms::brm(sleep_score ~ 1 + day * antidepressant * duration_in_deep + (1|day), data = DF, family = gaussian, chains = 2, cores = 2) 




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
mm17<- add_ic(mm17, ic="waic")
mm18<- add_ic(mm18, ic="waic")
mm19<- add_ic(mm19, ic="waic")
mm20<- add_ic(mm20, ic="waic")
mm21<- add_ic(mm21, ic="waic")
mm22<- add_ic(mm22, ic="waic")
mm23<- add_ic(mm23, ic="waic")
mm24<- add_ic(mm24, ic="waic")
mm25<- add_ic(mm25, ic="waic")



#Try this with the original df

weights = round(model_weights(mm0, mm1, mm2, mm3, mm5, mm6, mm7, mm8, mm9, mm10, mm11, mm12, mm13, mm14, mm15, mm16, mm17, mm18, mm19, mm20, mm21, mm22, mm23, mm24, mm25, ic = "waic"),3)
weights

weights2 = round(model_weights(mm25, mm19, mm6,mm13, mm20, mm17, mm5, mm18, mm7, mm15, mm16, mm21, mm11, ic = "waic"),3)
weights2

comparison = compare_ic(mm25, mm19, mm6,mm13, mm20, mm17, mm5, mm18, mm7, mm15, mm16, mm21, mm11, ic = "waic")
comparison


WAIC(mm0, mm1, mm2, mm3, mm5, mm6, mm7, mm8, mm9, mm10, mm11, se = FALSE) 


# ------------------ Models with most weight from low to high





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


mo4 <- brms::brm(sleep_score ~ 1 + day + duration_in_bed + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo3)
pp_check(mo3)
plot(marginal_effects(mo3), points = T)


mo5 <- brms::brm(sleep_score ~ 1 + day + avg_act + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo3)
pp_check(mo3)
plot(marginal_effects(mo3), points = T)


mo6 <- brms::brm(sleep_score ~ 1 + day + bedexit_duration + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo3)
pp_check(mo3)
plot(marginal_effects(mo3), points = T)


mo7 <- brms::brm(sleep_score ~ 1 + day + bedexit_duration + duration_in_bed + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo3)
pp_check(mo3)
plot(marginal_effects(mo3), points = T)

#Interaction

mo8 <- brms::brm(sleep_score ~ 1 + bedexit_duration * duration_in_bed + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo4)
pp_check(mo4)
plot(marginal_effects(mo4), points = T)



mo9 <- brms::brm(sleep_score ~ 1 + bedexit_duration * duration_in_sleep + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo9)
pp_mo9 = pp_check(mo9)
pp_mo9 + theme_classic()
plot(marginal_effects(mo9), points = T) + theme_classic()

mo10 <- brms::brm(sleep_score ~ 1 + tossnturn_count + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo7)
pp_check(mo7)
plot(marginal_effects(mo7), points = T)

mo11 <- brms::brm(sleep_score ~ 1 + tossnturn_count * duration_in_sleep + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo8)
pp_check(mo8)
plot(marginal_effects(mo8), points = T)


mo12 <- brms::brm(sleep_score ~ 1 + day + duration_in_bed * bedexit_duration + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo12)
pp_mo12 = pp_check(mo12)
pp_mo12 + theme_classic()
marg12 = plot(marginal_effects(mo12), points = T) + theme_classic()



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
mo9<- add_ic(mo9, ic="waic")
mo10<- add_ic(mo10, ic="waic")
mo11<- add_ic(mo11, ic="waic")
mo12<- add_ic(mo12, ic="waic")



#Try this with the original df

weights3 = round(model_weights(mo1, mo2, mo3, mo5, mo6, mo7, mo8, mo9, mo10, mo11, mo12, ic = "waic"),3)
weights3

weights4 = round(model_weights(mo2, mo5, mo7, mo9, mo11, mo12, ic = "waic"),3)
weights4

comparison2 = compare_ic(mo2, mo5, mo7, mo9, mo11, mo12, ic = "waic")
comparison2



# ------------ PLOT

#general duration in sleep
ds = ggplot(finalavg, aes(day, duration_in_sleep, colour = duration_in_sleep))+
  geom_point()+
  geom_smooth(method = "lm") 

#general sleep score
ss = ggplot(finalavg, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm") 

both = grid.arrange(ds, ss)

all = grid.arrange(p1, p2, p3, p4, p5, p6, p8)
alls = grid.arrange(s1, s2, s3, s4, s5, s6, s8)

pat1<- finalavg[ which(finalavg$patient==1), ]
contr <- f[ which(f$patient=="control"), ]

comparesubject = merge(pat1, contr, all = TRUE)
View(comparesubject)

r = finalavg
unscalemat(finalavg,r)




#general duration in comparison w most similar person
p1 = ggplot(comparesubject, aes(day, duration_in_sleep, colour = duration_in_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)


s1 = ggplot(comparesubject, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)


#2
p2 = ggplot(comparesubject2, aes(day, duration_in_sleep, colour = duration_in_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)

s2 = ggplot(comparesubject2, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)

#3
p3 = ggplot(comparesubject3, aes(day, duration_in_sleep, colour = duration_in_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)

s3 = ggplot(comparesubject3, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)



#4
p4 = ggplot(comparesubject4, aes(day, duration_in_sleep, colour = duration_in_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)

s4 = ggplot(comparesubject4, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)



#5
p5 = ggplot(comparesubject5, aes(day, duration_in_sleep, colour = duration_in_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)

s5 = ggplot(comparesubject5, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)


#6
p6 = ggplot(comparesubject6, aes(day, duration_in_sleep, colour = duration_in_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)

s6 = ggplot(comparesubject6, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)


#8
p8 = ggplot(comparesubject8, aes(day, duration_in_sleep, colour = duration_in_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)

s8 = ggplot(comparesubject8, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)