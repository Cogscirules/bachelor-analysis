library(pacman)
p_load(stringr, rethinking, brms, dplyr, ggplot2, gridExtra, mvtnorm, rethinking, metafor, readxl, lme4, data.table, tidyverse, lubridate, groupdata2)



#This part of the code is merely cleaning and mergin intro the resulting final DF


# ------------------------------------ BASE CODE BASED ON 1 PATIENT AND DATASET


setwd("C:/Users/Bruger/Desktop/Bachelor/bachelor-analysis/01/14-02-2018")


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
  i$day = 26
  i$patient = 01
  i = i[c(20,21,19,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]
  df8 = i
  
  
}

View(df1)

visualize_pat1 = Reduce(function(x, y) merge(x, y, all=TRUE), list(df1,df2,df3,df4,df5,df6,df7,df8))
View(visualize_pat1)

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


View(newdf)

plot(newdf$duration_in_sleep, newdf$duration_in_rem)

?unique



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

quodf = read.csv("statusquodf.csv")
quodf=quodf[-c(1)]

#Realized I need to anonymize even more, making dates into days

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

final_emfit_dft = merge(final_emfit_dft, fquo, all = TRUE)

final_emfit_df = final_emfit_dft

patient7 = final_emfit_df$patient == "7"
View(patient7)
View(final_emfit_df)

#renaming date to day
colnames(final_emfit_df)[2] = "day" 


#make stuff numeric

final_emfit_df = final_emfit_df %>%
  mutate_at(vars(day), as.numeric)


#scale stuff

final_emfit_df = group_by(final_emfit_df, patient, add = FALSE) %>%
  mutate(day = scale(day))






write.csv(final_emfit_df, file = "final_emfit_df.csv")

# ----------------------- MAKING OTHER DFs


final_emfit_df = read_csv("final_emfit_df.csv", col_names = TRUE)

# Taking all avarages and other descriptive data to patients sleep and combiningthem in 1 dataframe

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




# Now we want the medication data too

medpat9 = read_csv("medpart9.csv",  col_names = TRUE)

View(medpat9)

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

View(medpat5)

#X1 = days, but is not right since the dates are jumping, need to be fixed
medpat9[44, 1] = NA
colnames(medpat9)[1]="day"

medpat9[is.na(medpat9)] <- 0

#make a new column, collecting all the antidepressive mg's 
medpat9$antidepressant <- (medpat9$Escitalopram)

medpat9$sleepmed = medpat9$Zolpidem+medpat9$Zopiclon


medpat9 = select(medpat9, day, sleepmed, antidepressant)


#merging into a small df per patient
medscore_8 = merge(sleepscore8, medpat9, by = "day")
View(medscore_8)

#No avg data for nr 7
#No medicin data for nr. 4 + 7 during avg dates

#merge all meddata

medscore = full_join(medscore, medscore_8)
View(score)


write.csv(medscore, file = "all_medscore.csv")


#merging into a big df with all data

full_avg_df = merge(score, medscore, all = TRUE)

#making NA in medication intro 0
full_avg_df[is.na(full_avg_df)] <- 0


#add gender and age

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

finalavg_inbeddur = merge(finalavg_inbeddur, f, by = "patient")
finalavg <- finalavg[c(1,13,14,2:12)]

finalavg_inbeddur = finalavg_inbeddur[c(1,14,15,2:13)]

#Scaling what I haven't scaled yet
#First numeric

finalavg_inbeddur= finalavg_inbeddur %>%
  mutate_at(vars(gender,
                 age), as.numeric)



#makes it nan?
finalavg = group_by(finalavg, patient, add = FALSE) %>%
  mutate(age = scale(age),
         gender = scale(gender))




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


#Write dataframe

write.csv(full_avg_df, file = "full_avg_med_df.csv")

inbeddur = select(avg_dur_df, day, patient, duration_in_bed)
View(inbeddur)


inbeddur = merge(finalavg, inbeddur, all = TRUE)
View(finalavg_inbeddur)


inbeddur<-subset(inbeddur,!(is.na(inbeddur["avg_hr"]) | is.na(inbeddur["avg_rr"]) | is.na(inbeddur["sleep_score"]) | is.na(inbeddur["duration_in_sleep"]) | is.na(inbeddur["duration_in_rem"]) | is.na(inbeddur["duration_in_bed"]) | is.na(inbeddur["duration_in_light"]) | is.na(inbeddur["duration_in_deep"])))


write.csv(finalavg, file = "finalavg.csv")


#NOW, the big data set.....
View(full_avg_df)
View(final_emfit_df)


final_full_df = merge(full_avg_df, final_emfit_df, all = TRUE)


write.csv(finalavg, file = "final_avg_no_na.csv")

View(final_full_df)


#see which are not Na or NaN
which(is.na(final_full_df$sleepmed) == FALSE)
#se række
final_full_df[4909,]




#dataframe for male
malefullbed = finalavg_inbeddur[which(finalavg_inbeddur$gender == 2),]
View(malefull)

#df for female
femfullbed = finalavg_inbeddur[which(finalavg_inbeddur$gender == 1),]
View(femfull)


write.csv(malefull, file = "malefull.csv")
write.csv(malefullbed, file = "malefullbed.csv")
write.csv(femfull, file = "femfull.csv")
write.csv(femfullbed, file = "femfullbed.csv")
write.csv(DF, file = "DF.csv")









#need to eliminate control and NA

finalfull= finalfull %>%
  mutate_at(vars(day,
                 patient,
                 avg_hr,
                 avg_rr,
                 duration_in_rem,
                 duration_in_light,
                 duration_in_deep,
                 csd,
                 sleepmed,
                 antidepressant,
                 at,
                 hr,
                 rr,
                 act,
                 duration_awake,
                 duration_sleep_onset,
                 awakenings,
                 sleep_score,
                 tossnturn_count,
                 bedexit_count,
                 avg_act,
                 duration_in_sleep,
                 duration_in_bed,
                 bedexit_duration), as.numeric)


DF2 = finalfull

finalavg[ ! ( ( finalavg$patient == "control" ) ) ]

cond3 <- finalavg$patient == "sleepmed"

c12 <- finalavg[!cond3,]

merge(c1, c12, all = TRUE)

View(c12)

View(DF2)



#DURATION IN SLEEP IN HOURS


for (i in visualize_pat1) {
  #making the dataframe, removing a few columns not needed, and selecting the desired ones
  i = select(visualize_pat1, patient, duration_in_sleep, day)
  #removing rows only containing NA's, leaving the ones which have values
  i = subset(i,!(is.na(i["duration_in_sleep"]) | is.na(i["day"])))
  #making it numeric
  i = as.data.frame(sapply(i, as.numeric))
  #create another column with duration in hours
  i = transform(i, hours_of_sleep = duration_in_sleep / 3600)
  vis_pat1 = i
}

View(vis_pat1)
