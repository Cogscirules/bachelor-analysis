library(pacman)
p_load(stringr, rethinking, brms, dplyr, ggplot2, gridExtra, mvtnorm, rethinking, metafor, readxl, lme4, data.table, tidyverse, lubridate, groupdata2)


#The first part of the code is merely data cleaning, based on patient 1


# ------------------------------------ BASE CODE BASED ON 1 PATIENT AND DATASET


setwd("C:/Users/Bruger/Desktop/Bachelor/bachelor-analysis/11/06-08-2018")


df <-
  list.files(pattern = "*.csv") %>%  
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

#Extracting the timestamp
df1_1<-mutate(df,at=as.character(at))
df1_2<-mutate(df,at=sapply(strsplit(df$at, split=' ', fixed=TRUE),function(x) (x[2])))



#shows which values are not na
#which(is.na(full_01_df$bedexit_count) == FALSE)
#se række
#full_01_df[45337,]

#looping to make the data frame (exchange the namesin the loop for different dataframes)
for (i in df1_2) {
  #making the dataframe, removing a few columns not needed, and selecting the desired ones
  i = df1_2[ -c(1,2,3,4)]
  i = select(df1_2, at, hr, rr, act, tossnturn_count, sleep_score, duration_awake, 
             duration_in_sleep, duration_in_rem, duration_in_light, duration_in_deep, duration_sleep_onset, bedexit_count, 
             awakenings, bedexit_duration)
  #removing rows only containing NA's, leaving the ones which have values
  ind = apply(i, 1, function(x) all(is.na(x)))
  i = i[ !ind, ]
  #making new columns
  i$day = 22
  i$patient = 8
  i = i[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
  df13 = i
  
  
}

View(df1)

visualize_pat8 = Reduce(function(x, y) merge(x, y, all=TRUE), list(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12))

full_01_df = full_01_df[c(1,21,3,4,5,6,2,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]

#saving to csv
write.csv(full_01_df, file = "full_01_df_cleansed.csv")
#saved in bachelor/bachelor-analysis folder






# ------------------------------ DOWNSAMPLING & SCALING

#selecting the things I need, once more


columns <- sapply(fulldf, is.numeric)
columns



View(fulldf)

#making them numeric, apparently the were not
fulldf= full_01_df %>%
  mutate_at(vars(at, 
                 hr, 
                 rr, 
                 act, 
                 duration_in_bed, 
                 avg_hr, 
                 avg_rr, 
                 avg_act, 
                 tossnturn_count, 
                 sleep_score, 
                 duration_awake, 
                 duration_in_sleep, 
                 duration_in_rem, 
                 duration_in_light, 
                 duration_in_deep, 
                 duration_sleep_onset, 
                 bedexit_count, 
                 awakenings, 
                 bedexit_duration), as.numeric)



#scaling the variables which need scaling, placing them in new df

scaleddf = group_by(fulldf, patient, add = FALSE) %>%
  mutate(hr = scale(hr),
         rr=scale(rr),
         act=scale(act),
         bedexit_duration = scale(bedexit_duration),
         duration_in_bed = scale(duration_in_bed), 
         avg_hr = scale(avg_hr), 
         avg_rr = scale(avg_rr), 
         avg_act = scale(avg_act), 
         tossnturn_count = scale(tossnturn_count), 
         sleep_score = scale(sleep_score), 
         duration_awake = scale(duration_awake), 
         duration_in_sleep = scale(duration_in_sleep),
         duration_in_rem = scale(duration_in_rem),
         duration_in_light = scale(duration_in_light),
         duration_in_deep = scale(duration_in_deep),
         duration_sleep_onset = scale(duration_sleep_onset),
  )





# ---------------------------- OTHER DATASETS



setwd("C:/Users/Bruger/Desktop/Bachelor/bachelor-analysis/control/13-10-2018")


df <-
  list.files(pattern = "*.csv") %>%  
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

#Extracting the timestamp
df1_1<-mutate(df,at=as.character(at))
df1_2<-mutate(df,at=sapply(strsplit(df$at, split=' ', fixed=TRUE),function(x) (x[2])))



#shows which values are not na
#which(is.na(full_01_df$bedexit_count) == FALSE)
#se række
#full_01_df[45337,]

#looping to make the data frame (exchange the namesin the loop for different dataframes)
for (i in df1_2) {
  #making the dataframe, removing a few columns not needed, and selecting the desired ones
  i = df1_2[ -c(1,2,3,4)]
  i = select(df1_2, at, hr, rr, act, duration_in_bed, avg_hr, avg_rr, avg_act, tossnturn_count, sleep_score, duration_awake, 
             duration_in_sleep, duration_in_rem, duration_in_light, duration_in_deep, duration_sleep_onset, bedexit_count, 
             awakenings, bedexit_duration)
  #removing rows only containing NA's, leaving the ones which have values
  ind = apply(i, 1, function(x) all(is.na(x)))
  i = i[ !ind, ]
  #making new columns
  i$date = c("2018-13-10")
  i$patient = "control"
  i = i[c(20,21,19,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]
  df16 = i
  
}


View(df16)

full_09_df= Reduce(function(x, y) merge(x, y, all=TRUE), list(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16))
View(full_09_df)

full_09_df = full_09_df[c(1,2,4,5,6,7,3,8,9,10,11,12,13,14,15,16,17,18,19,20,21)]

#saving to csv
write.csv(full_09_df, file = "full_09_df_cleansed.csv")
#saved in bachelor/bachelor-analysis folder


#checking whether they are numeric
columns <- sapply(full_04_df, is.numeric)
columns



#making them numeric, apparently the were not
fulldf9= full_09_df %>%
  mutate_at(vars(hr, 
                 rr, 
                 act, 
                 duration_in_bed, 
                 avg_hr, 
                 avg_rr, 
                 avg_act, 
                 tossnturn_count, 
                 sleep_score, 
                 duration_awake, 
                 duration_in_sleep, 
                 duration_in_rem, 
                 duration_in_light, 
                 duration_in_deep, 
                 duration_sleep_onset, 
                 bedexit_count, 
                 awakenings, 
                 bedexit_duration), as.numeric)



#scaling the variables which need scaling, placing them in new df

scaleddf9 = group_by(fulldf9, patient, add = FALSE) %>%
  mutate(hr = scale(hr),
         rr=scale(rr),
         act=scale(act),
         bedexit_duration = scale(bedexit_duration),
         duration_in_bed = scale(duration_in_bed), 
         avg_hr = scale(avg_hr), 
         avg_rr = scale(avg_rr), 
         avg_act = scale(avg_act), 
         tossnturn_count = scale(tossnturn_count), 
         sleep_score = scale(sleep_score), 
         duration_awake = scale(duration_awake), 
         duration_in_sleep = scale(duration_in_sleep),
         duration_in_rem = scale(duration_in_rem),
         duration_in_light = scale(duration_in_light),
         duration_in_deep = scale(duration_in_deep),
         duration_sleep_onset = scale(duration_sleep_onset),  
  )

quodf = merge(quodf, scaleddf9, all = TRUE)




# ----------------------- MAKING THE OTHER DF


final_emfit_df = read_csv("final_emfit_df.csv", col_names = TRUE)

# Taking all avarages and other descriptive data to patients sleep and combining them in 1 dataframe

avg_duration_df = select(final_emfit_df, patient, day, avg_hr, avg_rr, avg_act, tossnturn_count, sleep_score, duration_awake, 
                         duration_in_sleep, duration_in_bed, duration_in_rem, duration_in_light, duration_in_deep, duration_sleep_onset, bedexit_count, 
                         awakenings, bedexit_duration)

View(avg_duration_df)

#delete rows with NA's, leaving only avarages and other descriptive data on their sleep
avg_dur_df<-subset(avg_duration_df,!(is.na(avg_duration_df["avg_hr"]) | is.na(avg_duration_df["avg_rr"]) | is.na(avg_duration_df["avg_act"]) | is.na(avg_duration_df["tossnturn_count"]) | is.na(avg_duration_df["sleep_score"]) | is.na(avg_duration_df["duration_awake"]) | is.na(avg_duration_df["duration_in_sleep"]) | is.na(avg_duration_df["duration_in_rem"]) | is.na(avg_duration_df["duration_in_bed"]) | is.na(avg_duration_df["duration_in_light"]) | is.na(avg_duration_df["duration_in_deep"]) | is.na(avg_duration_df["duration_sleep_onset"]) | is.na(avg_duration_df["bedexit_count"]) | is.na(avg_duration_df["awakenings"]) | is.na(avg_duration_df["bedexit_duration"])))

View(avg_dur_df)


write.csv(avg_dur_df, file = "avg_and_dur_data.csv")



# Taking AVG_HR, AVG_RR, sleep class and CSD into a dataframe

#selecting what I need
sleepscore_df = select(avg_dur_df, patient, date, avg_hr, avg_rr, sleep_score, duration_in_sleep, duration_in_rem, duration_in_light, duration_in_deep)
View(sleepscore_df)


#subsetting the patient i want first
sleepscore1 = subset(sleepscore_df, patient == "1")
sleepscorec = subset(sleepscore_df, patient == "control")

#selecting what I need from csd and chaning the column name so it makes sense in the big df
csd_1 = select(csd_scaled, pat1, day)
csd_c = select(csd_scaled, control, day)
colnames(csd_c)[1]="csd"
View(csd_7)

#merging into a small df per patient
sleepscore_dfc = merge(sleepscorec, csd_c, by = "day")
View(sleepscore_dfc)


#merging into a big df with all avg's and patients
score = merge(score, sleepscore_dfc, all = TRUE)
View(score)

#writing
write.csv(score, file = "avg_and_sleep_score.csv")


# --------------Realized I need to anonymize even more, making dates into days


fquo = read.csv("statusquodf.csv")
fquo=fquo[-c(1)]



fquo = quodf

fquo = fquo %>%
  mutate_at(vars(date), as.character)


#1
yquo = subset(tquo, patient == 1)
yquo$date[yquo$date == "2018-02-14"] = "25"

#2
uquo = subset(tquo,patient ==2)
uquo$date[uquo$date == "2018-01-26"] = "3"

#3
iquo = subset(tquo,patient==3)
iquo$date[iquo$date == "2018-05-03"] = "29"

#4
oquo = subset(tquo,patient==4)
oquo$date[oquo$date == "2018-05-31"] = "8"

#5
pquo = subset(tquo,patient==5)
pquo$date[pquo$date == "2018-06-19"] = "9"

#6
aquo = subset(tquo,patient==6)
aquo$date[aquo$date == "2018-07-24"] = "35"

#7
squo = subset(tquo,patient==7)
squo$date[squo$date == "2018-06-25"] = "5"

#8
dquo = subset(tquo,patient==8)
dquo$date[dquo$date == "2018-08-06"] = "22"

#9
fquo = subset(fquo,patient== "control")
fquo$date[fquo$date == "2018-13-10"] = "16"   


View(fquo)

final_emfit_df = merge(final_emfit_df, fquo, all = TRUE)



#renaming date to day
colnames(final_emfit_df)[2] = "day" 


#make day numeric

final_emfit_df = final_emfit_df %>%
  mutate_at(vars(day), as.numeric)


#scale day

final_emfit_df = group_by(final_emfit_df, patient, add = FALSE) %>%
  mutate(day = scale(day))


#Write file
write.csv(final_emfit_df, file = "final_emfit_df.csv")



# ----------------- MEDICATION DATA

# Now we want the medication data too

#medpatx = medication data per patient x

medpat1 = read_csv("medpart1.csv",  col_names = TRUE)

View(medpat1)

#X1 = days, but is not right since the dates are jumping, need to be fixed
medpat1[40, 1] = NA
colnames(medpat1)[1]="day"

medpat1[is.na(medpat1)] <- 0


#make a new column, collecting all the antidepressive mg's 
medpat1$antidepressant <- (medpat1$Clomipramin+medpat1$Mirtazapin+medpat1$Citalopram)

medpat1 = select(medpat1, day, Clomipramin, Mirtazapin, Citalopram, antidepressant, Zolpidem)

medpat1$sleepmed = medpat1$Zolpidem


#merging into a small df per patient
medscore_1 = merge(sleepscore1, medpat1, by = "day")
View(medscore_1)


# Now we want the other medication data too

#merge all meddata

medscore = full_join(medscore, medscore_8)
View(score)


write.csv(medscore, file = "all_medscore.csv")


#merging into a big df with all data

full_avg_df = merge(final_emfit_df, medscore, all = TRUE)

#making NA in medication into 0
full_avg_df[is.na(full_avg_df)] <- 0


# -------------------- ADDING THE BASIC VARIABLES, I forgot


patient = 9
age = 36
gender = 1

patient = data.frame(c(1,2,3,4,5,6,7,8,9))
colnames(patient)[1] <- "patient"

age = data.frame(c(24,53,58,46,19,36,67,35,36))
colnames(age)[1] <- "age"

gender = data.frame(c(1,2,2,2,1,1,2,1,1))
colnames(gender)[1] <- "gender"

f = c(patient, age, gender)

#Merge it all
finalavg = merge(finalavg_inbeddur, f, by = "patient")
finalavg = finalavg[c(1,14,15,2:13)]


# ------------- LAST PART

#I realized I had to scale + make more variables numeric before merging it all into one big dataframe
#I had to make two dataframes because of computational limitations for models

#Scaling what I haven't scaled yet
#First numeric

finalavg_inbeddur= finalavg_inbeddur %>%
  mutate_at(vars(gender,
                 age), as.numeric)



#making them numeric, apparently the were not
full_avg_df= full_avg_df %>%
  mutate_at(vars(day,
                 sleepmed,
                 antidepressant), as.numeric)




full_avg_df = group_by(full_avg_df, patient, add = FALSE) %>%
  mutate(day = scale(day),
         sleepmed = scale(sleepmed),
         antidepressant = scale(antidepressant),   
  )


#Write new dataframe with movement/in bed data

write.csv(full_avg_df, file = "full_avg_med_df.csv")

inbeddur = select(avg_dur_df, day, patient, duration_in_bed)
View(inbeddur)


inbeddur = merge(finalavg, inbeddur, all = TRUE)
View(finalavg_inbeddur)


inbeddur<-subset(inbeddur,!(is.na(inbeddur["avg_hr"]) | is.na(inbeddur["avg_rr"]) | is.na(inbeddur["sleep_score"]) | is.na(inbeddur["duration_in_sleep"]) | is.na(inbeddur["duration_in_rem"]) | is.na(inbeddur["duration_in_bed"]) | is.na(inbeddur["duration_in_light"]) | is.na(inbeddur["duration_in_deep"])))


write.csv(finalavg, file = "finalavg.csv")


#NOW, make the big data set.....
View(full_avg_df)
View(final_emfit_df)


final_full_df = merge(full_avg_df, final_emfit_df, all = TRUE)


write.csv(final_full_df, file = "final_avg.csv")










# ------------------- A BIT OF CODE FOR VISUALIZATION



#DURATION IN SLEEP IN HOURS


for (i in visualize_pat8) {
  #making the dataframe, removing a few columns not needed, and selecting the desired ones
  i = select(visualize_pat8, patient, duration_in_sleep, day)
  #removing rows only containing NA's, leaving the ones which have values
  i = subset(i,!(is.na(i["duration_in_sleep"]) | is.na(i["day"])))
  #making it numeric
  i = as.data.frame(sapply(i, as.numeric))
  #create another column with duration in hours
  i = transform(i, hours_of_sleep = duration_in_sleep / 3600)
  vis_pat8 = i
}

View(vis_pat3)


vis_pat = merge(vis_pat, vis_pat8, all = TRUE)



write.csv(vis_pat8, file = "vis_pat8.csv")


# ------------ PLOT


#general duration in sleep
ds = ggplot(finalavg, aes(day, duration_in_sleep, colour = duration_in_sleep))+
  geom_point()+
  geom_smooth(method = "lm") 


ghs = ggplot(vis_pat, aes(day, hours_of_sleep, colour = hours_of_sleep))+
  geom_point()+
  geom_smooth(method = "lm")

ghs


#general sleep score
ss = ggplot(finalavg, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm") 

both = grid.arrange(all, alls)
both

all = grid.arrange(p1, p2, p3, p4, p5, p6, p8)
alls = grid.arrange(s1, s2, s3, s4, s5, s6, s8)



pat1<- finalavg[ which(finalavg$patient==1), ]
contr <- f[ which(f$patient=="control"), ]

comparesubject = merge(pat1, contr, all = TRUE)
View(comparesubject)


finalavg = as.data.frame(sapply(finalavg, as.numeric))



#general duration in comparison w most similar person
p1 = ggplot(vis_pat1, aes(day, hours_of_sleep, colour = hours_of_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)
p1

s1 = ggplot(comparesubject, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)


#2
p2 = ggplot(vis_pat2, aes(day, hours_of_sleep, colour = hours_of_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)
p2

View(vis_pat2)

s2 = ggplot(comparesubject2, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)

#3
p3 = ggplot(vis_pat3, aes(day, hours_of_sleep, colour = hours_of_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)
p3


s3 = ggplot(comparesubject3, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)



#4
p4 = ggplot(vis_pat4, aes(day, hours_of_sleep, colour = hours_of_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)
p4

View(vis_pat4)

s4 = ggplot(comparesubject4, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)



#5
p5 = ggplot(vis_pat5, aes(day, hours_of_sleep, colour = hours_of_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)
p5


s5 = ggplot(comparesubject5, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)


#6
p6 = ggplot(vis_pat6, aes(day, hours_of_sleep, colour = hours_of_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)
p6

s6 = ggplot(comparesubject6, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)


#8
p8 = ggplot(vis_pat8, aes(day, hours_of_sleep, colour = hours_of_sleep))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)
p8


View(vis_pat1)

s8 = ggplot(comparesubject8, aes(day, sleep_score, colour = sleep_score))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~ patient)


# --------------------- MAKING MODELS

#med data
finalavg = read_csv("finalavg.csv",  col_names = TRUE)
finalavg = finalavg[-c(1)]

#mov data
finalfull = read_csv("final_full_df.csv",  col_names = TRUE)
finalfull = finalfull[-c(1)]


#Intercept model --> sleep score as predicted by how many days they are in hospital
# mu = alpha(sleepscore) + beta(days)
#This can also be duration in sleep, deep, light and rem

#------- BASIC MODELS


#sleep score as predicted by days
mm0 <- brms::brm(sleep_score ~ 1 + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)


mm1 <- brms::brm(sleep_score ~ 1 + day + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(mm1)
pp_mm1 = pp_check(mm1)
pp_mm1 + theme_classic()
marg_mm1 = plot(marginal_effects(mm1), points = T)

hyp1 <- brms::brm(duration_in_sleep ~ 1 + day + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(hyp1)
pphyp1 = pp_check(hyp1)
pphyp1 + theme_classic()
marghyp1 = plot(marginal_effects(hyp1), points = T)

both2 = grid.arrange(marghyp1, marg_ma1)

# sleep score as predicted by consensus sleep diary, medication, duration in sleep, rem and deep
mm2 <- brms::brm(sleep_score ~ 1 + csd + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 

mm3 <- brms::brm(sleep_score ~ 1 + sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 

mm4 <- brms::brm(sleep_score ~ 1 + antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 

mm5 <- brms::brm(sleep_score ~ 1 + duration_in_sleep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 

mm6 <- brms::brm(sleep_score ~ 1 + duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 

mm7 <- brms::brm(sleep_score ~ 1 + duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 



mm8 <- brms::brm(sleep_score ~ 1 + day + duration_in_sleep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(mm8)
pp_mm8 = pp_check(mm8)
pp_mm8 + theme_classic()
marg_mm8 = plot(marginal_effects(mm8), points = T) + theme_classic()


mm9 <- brms::brm(sleep_score ~ 1 + day + sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(mm9)
pp_mm9 = pp_check(mm9)
pp_mm9 + theme_classic()
marg_mm9 = plot(marginal_effects(mm9), points = T)



mm10 <- brms::brm(sleep_score ~ 1 + day + antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(mm10)
pp_mm10 = pp_check(mm10)
pp_mm10 + theme_classic()
marg_mm10 = plot(marginal_effects(mm10), points = T)


# ------ INTERACTION MODELS

# sleep score as predicted by interaction between duration in sleep and sleepmed
mm11 <- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 

# sleep score as predicted by interaction between duration in sleep and antidepressant
mm12 <- brms::brm(bf(sleep_score ~ 1 + duration_in_sleep * antidepressant + (1|day)), data = finalavg, family = gaussian, chains = 2, cores = 2) 


# sleep score as predicted by Interaction with both medications
mm13<- brms::brm(sleep_score ~ 1 + duration_in_sleep * sleepmed * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 


# Sleep score as predicted by interaction between both medications
mm14<- brms::brm(sleep_score ~ 1 + sleepmed * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 


#Sleep score as predicted by interaction between duration in sleep and rem
mm15<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 


#Sleep score as predicted by duration in sleep and duration in deep
mm16<- brms::brm(sleep_score ~ 1 + duration_in_sleep * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(mm16)
pp_mm16 = pp_check(mm16)
pp_mm16 + theme_classic()
marg_mm16 = plot(marginal_effects(mm16), points = T) - theme_classic()



#Sleep score as predicted by interaction between sleep med and duration in deep
mm17<- brms::brm(sleep_score ~ 1 + sleepmed * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 


#sleep score as predicted by interaction between sleep med and rem
mm18<- brms::brm(sleep_score ~ 1 + sleepmed * duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 


#sleep score as predicted by interaction between antidepressant and deep
mm19<- brms::brm(sleep_score ~ 1 + antidepressant * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 


#sleep score as predicted by interaction between antipressant and rem
mm20<- brms::brm(sleep_score ~ 1 + antidepressant * duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 


#sleep scores and day 

mm21<- brms::brm(sleep_score ~ 1 + day * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(mm21)
pp_mm21 = pp_check(mm21)
pp_mm21 + theme_classic()
marg_mm21 = plot(marginal_effects(mm21), points = T)+ theme_classic()

mm22<- brms::brm(sleep_score ~ 1 + day * duration_in_rem + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 


mm23<- brms::brm(sleep_score ~ 1 + day * sleepmed + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 
summary(mm23)
pp_mm23 = pp_check(mm23)
pp_mm23 + theme_classic()
marg_mm23 = plot(marginal_effects(mm23), points = T)


mm24<- brms::brm(sleep_score ~ 1 + day * antidepressant + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2)
summary(mm24)
pp_mm24 = pp_check(mm24)
pp_mm24 + theme_classic()
marg_mm24 = plot(marginal_effects(mm24), points = T)+theme_classic()


mm25<- brms::brm(sleep_score ~ 1 + day * antidepressant * duration_in_deep + (1|day), data = finalavg, family = gaussian, chains = 2, cores = 2) 




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

weights2 = round(model_weights(mm25, mm19, mm6, mm13, mm20, mm17, mm5, mm18, mm7, mm15, mm16, mm21, mm11, ic = "waic"),3)
weights2

weights3 = round(model_weights(mm6, mm13, mm15, mm16, mm11, ic = "waic"),3)
weights3 

comparison = compare_ic(mm25, mm19, mm6,mm13, mm20, mm17, mm5, mm18, mm7, mm15, mm16, mm21, mm11, ic = "waic")
comparison


WAIC(mm0, mm1, mm2, mm3, mm5, mm6, mm7, mm8, mm9, mm10, mm11, se = FALSE) 


# ------------------ Models with most weight from low to high





# ------------- INTERACTION MODELS FOR MOVEMENT MODELS

# sleep score as predicted by movement

mo1 <- brms::brm(sleep_score ~ 1 + avg_act + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo1)
pp_check(mo1)
plot(marginal_effects(mo1), points = T)


mo2 <- brms::brm(sleep_score ~ 1 + duration_in_bed + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo2)
pp_check(mo2)
plot(marginal_effects(mo2), points = T)


mo3 <- brms::brm(sleep_score ~ 1 + bedexit_duration + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo3)
pp_check(mo3)
plot(marginal_effects(mo3), points = T)


mo4 <- brms::brm(sleep_score ~ 1 + day + duration_in_bed + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo4)
pp_check(mo4)
plot(marginal_effects(mo4), points = T)


mo5 <- brms::brm(sleep_score ~ 1 + day + avg_act + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo5)
pp_check(mo5)
plot(marginal_effects(mo5), points = T)


mo6 <- brms::brm(sleep_score ~ 1 + day + bedexit_duration + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo6)
pp_check(mo6)
plot(marginal_effects(mo6), points = T)


mo7 <- brms::brm(sleep_score ~ 1 + day + bedexit_duration + duration_in_bed + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo7)
pp_check(mo7)
plot(marginal_effects(mo7), points = T)

#Interaction

mo8 <- brms::brm(sleep_score ~ 1 + bedexit_duration * duration_in_bed + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo8)
pp_check(mo8)
plot(marginal_effects(mo8), points = T)



mo9 <- brms::brm(sleep_score ~ 1 + bedexit_duration * duration_in_sleep + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo9)
pp_mo9 = pp_check(mo9)
pp_mo9 + theme_classic()
plot(marginal_effects(mo9), points = T) + theme_classic()

mo10 <- brms::brm(sleep_score ~ 1 + tossnturn_count + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo10)
pp_check(mo10)
plot(marginal_effects(mo10), points = T)

mo11 <- brms::brm(sleep_score ~ 1 + tossnturn_count * duration_in_sleep + (1|day), data = finalfull, family = gaussian, chains = 2, cores = 2) 
summary(mo11)
pp_check(mo11)
plot(marginal_effects(mo11), points = T)


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



