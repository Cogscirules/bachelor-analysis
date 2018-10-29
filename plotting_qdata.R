library(pacman)
p_load(stringr, rethinking, brms, dplyr, ggplot2, gridExtra, mvtnorm, rethinking, metafor, readxl, lme4, tidyverse, lubridate, groupdata2)

setwd("C:/Users/Bruger/Desktop/Bachelor/Data")


# --------- Importing + Plotting CSD


csd = read_excel("csd.xlsx", sheet = 1, range = NULL, col_names = TRUE,
                          col_types = NULL, na = "")

#making a new column w/ days
csd$day = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)

#rearranging
csd = csd[c(10,1,2,3,4,5,6,7,8,9)]

#adding the control
csd$control = c(3,3,5,2,4,3,2,5,2,2,4,4,3,3,5,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)


View(csd)

#saving to csv
write.csv(csd, file = "csd.csv")

#plotting as a line 
dev.off()
ggplot(csd, aes(day,control)) +
  geom_line() +
  geom_smooth(method ="lm") +
  coord_cartesian() +
  scale_color_gradient() +
  theme_bw()


# -------------- Plotting hads
#hospital depresion anxiety scale


hads = read_excel("datasleep.xlsx", sheet = 5, range = NULL, col_names = TRUE,
                 col_types = NULL, na = "")
#deleting rows I don't need
hads = hads[-c(6,7,12:51),]

View(hads)

#saving to csv
write.csv(hads, file = "hads.csv")


#deleting columns for another df

hadssum = hads[ -c(3:16,20:33,37:50,54:67,71:84)]
View(hadssum)


#saving to csv
write.csv(hadssum, file = "hadssum.csv")


# -------------- Plotting Hamilton-D

hamd = read_excel("datasleep.xlsx", sheet = 3, range = NULL, col_names = TRUE,
                  col_types = NULL, na = "")
#deleting rows I don't need
hamd = hamd[-c(6,7,12:51),]

View(hamd)


#saving to csv
write.csv(hamd, file = "ham-d.csv")


#deleting columns for another df

hamdsum = hamd[ c(1,2,20:21, 42:46, 64:68, 86:89)]
View(hamdsum)


#saving to csv
write.csv(hamdsum, file = "ham-dsum.csv")

# ------------ Plotting ISI

isi = read_excel("datasleep.xlsx", sheet = 11, range = NULL, col_names = TRUE,
                  col_types = NULL, na = "")
#deleting rows I don't need
isi = isi[-c(6,7,12:51),]

View(isi)

#saving to csv
write.csv(isi, file = "isi.csv")


#deleting columns for another df

isisum = isi[ -c(3:9, 12:18, 21:27, 30:36, 39:45)]
View(isisum)


#saving to csv
write.csv(isisum, file = "isisum.csv")


