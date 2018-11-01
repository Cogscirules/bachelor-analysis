library(pacman)
p_load(stringr, rethinking, brms, dplyr, ggplot2, gridExtra, mvtnorm, rethinking, metafor, readxl, lme4, tidyverse, lubridate, groupdata2)

setwd("C:/Users/Bruger/Desktop/Bachelor/bachelor-analysis/01/14-02-2018")

df <-
  list.files(pattern = "*.csv") %>% 
  map_df(~read_csv(., col_types = cols(.default = "c")))



#looping to make the data frame (exchange the namesin the loop for different dataframes)
for (i in df) {
  #making the dataframe, removing a few columns not needed, and selecting the desired ones
  i = df[ -c(1,2,3,4)]
  i = select(df, hr, rr, act, duration_in_bed, avg_hr, avg_rr, avg_act, tossnturn_count, sleep_score, duration_awake, 
             duration_in_sleep, duration_in_rem, duration_in_light, duration_in_deep, duration_sleep_onset, bedexit_count, 
             awakenings, bedexit_duration)
  #removing rows only containing NA's, leaving the ones which have values
  ind = apply(i, 1, function(x) all(is.na(x))) #not sure it works?
  i = i[ !ind, ]
  #making new columns
  i$date = c(as.Date("2018-02-14"))
  i$participant = 09
  i = i[c(20,19,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]
  df9 = i
  
  
}



full_01_df= Reduce(function(x, y) merge(x, y, all=TRUE), list(df1,df2,df3,df4,df5,df6,df7,df8,df9))
View(full_01_df)

#saving to csv
write.csv(full_01_df, file = "full_01_df_uncleansed.csv")
#saved in bachelor/bachelor-analysis folder


full_02_df = Reduce(function(x, y) merge(x, y, all=TRUE), list(df2,df3,df4,df5,df6,df7,df8,df9,df1))
View(full_02_df)

#qualitative data, just messing around

full_qd_df= Reduce(function(x, y) merge(x, y, all=TRUE), list(sleep_table1, sleep_table2, sleep_table3, sleep_table4, sleep_table5, sleep_table8))
View(full_qd_df)

#deleting rows I don't need
full_qd_df = full_qd_df[ -c(12:187),]

#full_01qd_df = merge(full_qd_df, full_01_df)
View(full_01qd_df)


# ------------------------------ Playing

importEmfitJSON <- function(filename, timezone) {
  dataAll <- df(filename)
  
  calc_data <- as.data.frame(dataAll[["calc_data"]])
  colnames(calc_data) <- c("timestamp","heart_rate","heart_rate_quality","breathing_rate", "breathing_rate_quality","activity")
  
  calc_data[,"timestamp"] <- as.POSIXct(origin="1970-01-01", tz="GMT", x=calc_data[,"timestamp"])
  head(calc_data)
  # transform times to local format (Ljubljana)
  calc_data[,"timestamp"] <- as.POSIXct(format(calc_data[,"timestamp"], tz=timezone))
  
  return(calc_data)
  
}


# ------------------------------ DOWNSAMPLING & SCALING

#selecting the things I need, once more
fulldf = select(full_01_df, participant, date, duration_in_bed, avg_hr, avg_rr, avg_act, tossnturn_count, sleep_score, duration_awake, 
                          duration_in_sleep, duration_in_rem, duration_in_light, duration_in_deep, duration_sleep_onset, bedexit_count, 
                          awakenings, bedexit_duration)

columns <- sapply(fulldf, is.numeric)
columns



View(fulldf)

#making them numeric, apparently the were not
fulldf= fulldf %>%
  mutate_at(vars(duration_in_bed, 
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

#downsampling, not sure I need to

"fulldf1 = group_by(fulldf, participant, date, add = FALSE) %>%
  mutate(duration_in_bed = mean(duration_in_bed, na.rm = T),
  avg_hr = mean(avg_hr, na.rm = T), 
  avg_rr = mean(avg_rr, na.rm = T), 
  avg_act = mean(avg_act, na.rm = T), 
  tossnturn_count = mean(tossnturn_count, na.rm = T), 
  sleep_score = mean(sleep_score, na.rm = T), 
  duration_awake = mean(duration_awake, na.rm = T), 
  duration_in_sleep = mean(duration_in_sleep, na.rm = T), 
  duration_in_rem = mean(duration_in_rem, na.rm = T), 
  duration_in_light = mean(duration_in_light, na.rm = T), 
  duration_in_deep = mean(duration_in_deep, na.rm = T), 
  duration_sleep_onset = mean(duration_sleep_onset, na.rm = T), 
  bedexit_count = mean(bedexit_count, na.rm = T), 
  awakenings = mean(awakenings, na.rm = T), 
  bedexit_duration = mean(bedexit_duration, na.rm = T)
)"

#scaling the variables which need scaling, placing them in new df

newdf = group_by(fulldf, participant, date, add = FALSE) %>%
  mutate(duration_in_bed = scale(duration_in_bed), 
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
         bedexit_duration = scale(bedexit_duration)
          )


View(newdf)

plot(newdf$duration_in_sleep, newdf$duration_in_rem)

?unique



# ---------------------------- OTHER DATASET

#selecting the things I need, once more
fulldf2 = select(full_02_df, participant, date, hr, rr, act, duration_in_bed, avg_hr, avg_rr, avg_act, tossnturn_count, sleep_score, duration_awake, 
                duration_in_sleep, duration_in_rem, duration_in_light, duration_in_deep, duration_sleep_onset, bedexit_count, 
                awakenings, bedexit_duration)

columns <- sapply(fulldf2, is.numeric)
columns



View(fulldf)

#making them numeric, apparently the were not
fulldf2= fulldf2 %>%
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

#downsampling, not sure I need to

"fulldf1 = group_by(fulldf, participant, date, add = FALSE) %>%
mutate(duration_in_bed = mean(duration_in_bed, na.rm = T),
avg_hr = mean(avg_hr, na.rm = T), 
avg_rr = mean(avg_rr, na.rm = T), 
avg_act = mean(avg_act, na.rm = T), 
tossnturn_count = mean(tossnturn_count, na.rm = T), 
sleep_score = mean(sleep_score, na.rm = T), 
duration_awake = mean(duration_awake, na.rm = T), 
duration_in_sleep = mean(duration_in_sleep, na.rm = T), 
duration_in_rem = mean(duration_in_rem, na.rm = T), 
duration_in_light = mean(duration_in_light, na.rm = T), 
duration_in_deep = mean(duration_in_deep, na.rm = T), 
duration_sleep_onset = mean(duration_sleep_onset, na.rm = T), 
bedexit_count = mean(bedexit_count, na.rm = T), 
awakenings = mean(awakenings, na.rm = T), 
bedexit_duration = mean(bedexit_duration, na.rm = T)
)"

#scaling the variables which need scaling, placing them in new df

newdf2 = group_by(fulldf2, participant, date, hr, rr, add = FALSE) %>%
  mutate(act, duration_in_bed = scale(duration_in_bed), 
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
         bedexit_duration = scale(bedexit_duration)
          )

View(newdf2)

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
