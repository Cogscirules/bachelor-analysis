library(pacman)
p_load(stringr, rethinking, brms, dplyr, ggplot2, gridExtra, mvtnorm, rethinking, metafor, readxl, lme4, tidyverse, lubridate, groupdata2)



# ------------------------------------ BASE CODE BASED ON 1 PATIENT AND DATASET


setwd("C:/Users/Bruger/Desktop/Bachelor/bachelor-analysis/01")


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
  i$date = c(as.Date("2018-02-14"))
  i$patient = 01
  i = i[c(20,21,19,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]
  df9 = i
  
  
}



full_01_df= Reduce(function(x, y) merge(x, y, all=TRUE), list(df1,df2,df3,df4,df5,df6,df7,df8,df9))
View(full_01_df)

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



setwd("C:/Users/Bruger/Desktop/Bachelor/bachelor-analysis/03/30-04-2018")


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
  i$date = c(as.Date("2018-04-30"))
  i$patient = 03
  i = i[c(20,21,19,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]
  df24 = i
  
}


View(df1)

full_03_df= Reduce(function(x, y) merge(x, y, all=TRUE), list(df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,df13,df14,df15,df16,df17,df18,df19,df20,df21,df22,df23,df24,df1,df2))
View(full_03_df)

full_03_df = full_03_df[c(1,2,4,5,6,7,3,8,9,10,11,12,13,14,15,16,17,18,19,20,21)]

#saving to csv
write.csv(full_03_df, file = "full_03_df_cleansed.csv")
#saved in bachelor/bachelor-analysis folder


#checking wether they are numeric
columns <- sapply(full_03_df, is.numeric)
columns



#making them numeric, apparently the were not
fulldf3= full_03_df %>%
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

scaleddf3 = group_by(fulldf3, patient, add = FALSE) %>%
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




quodf = merge(quodf, scaleddf3, all = TRUE)
View(quodf)

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
