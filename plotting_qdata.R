library(pacman)
p_load(stringr, rethinking, brms, dplyr, ggplot2, gridExtra, mvtnorm, rethinking, metafor, readxl, lme4, tidyverse, lubridate, groupdata2)


setwd("C:/Users/Bruger/Desktop/Bachelor/bachelor-analysis")

# --------- Importing + Plotting CSD


csd = read_csv("csd.csv",  col_names = TRUE)

#making a new column w/ days
csd$day = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)

#rearranging
csd = csd[c(10,1,2,3,4,5,6,7,8,9)]

#adding the control
csd$control = c(3,3,5,2,4,3,2,5,2,2,4,4,3,3,5,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)


View(csd)

#deleting column
csd = csd[-c(1)]


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



# ------------------ Medicin data


part5 = read_excel("nicolai_output.xlsx", sheet = 5, range = NULL, col_names = TRUE,
                  col_types = NULL, na = "")


View(part1)
#renaming rows
colnames(part1)[1] = "Dato"
colnames(part1)[2] = "Citalopram"
colnames(part1)[3] = "Atomoxetin"
colnames(part1)[4] = "Clomipramin"
colnames(part1)[5] = "Duloxetin"
colnames(part1)[5] = "Mirtazapin"
colnames(part1)[6] = "Oxazepam"
colnames(part1)[7] = "Quetiapin"
colnames(part1)[8] = "Zolpidem"

#deleting rows I don't need
part1 = part1[-c(1),]

write.csv(part1, file = "medpart1.csv")


View(part2)
#renaming rows
colnames(part2)[1] = "Dato"

write.csv(part2, file = "medpart2.csv")


View(part3)
#renaming rows
colnames(part3)[1] = "Dato"
colnames(part3)[2] = "Oxazepam"
colnames(part3)[3] = "Sertralin"
colnames(part3)[4] = "Zolpidem"
colnames(part3)[5] = "Hovedtotal"


#deleting rows I don't need
part3 = part3[-c(1),]

write.csv(part3, file = "medpart3.csv")


View(part4)
#renaming rows
colnames(part4)[1] = "Dato"

write.csv(part4, file = "medpart4.csv")


View(part5)
#renaming rows
colnames(part5)[1] = "Dato"
colnames(part5)[2] = "Acamprosat"
colnames(part5)[3] = "Lamotrigin"
colnames(part5)[4] = "Quetiapin"
colnames(part5)[5] = "Venlafaxin"
colnames(part5)[6] = "Zopiclon"
colnames(part5)[7] = "Hovedtotal"

#deleting rows I don't need
part5 = part5[-c(1),]

write.csv(part5, file = "medpart5.csv")



View(part6)
#renaming rows
colnames(part6)[1] = "Dato"
colnames(part6)[2] = "Duloxetin"
colnames(part6)[3] = "Olanzapin"
colnames(part6)[4] = "Oxazepam"
colnames(part6)[5] = "Hovedtotal"


#deleting rows I don't need
part6 = part6[-c(1),]

write.csv(part6, file = "medpart6.csv")


View(part7)
#renaming rows
colnames(part7)[1] = "Dato"
colnames(part7)[2] = "Citalopram"
colnames(part7)[3] = "Mirtazapin"
colnames(part7)[4] = "Olanzapin"
colnames(part7)[5] = "Oxazepam"
colnames(part7)[6] = "Quetiapin"
colnames(part7)[7] = "Risperidon"
colnames(part7)[8] = "Sertralin"
colnames(part7)[9] = "Zolpidem"
colnames(part7)[10] = "Zopiclon"

#deleting rows I don't need
part7 = part7[-c(1),]

write.csv(part7, file = "medpart7.csv")


View(part8)
#renaming rows
colnames(part8)[1] = "Dato"
colnames(part8)[2] = "Nortriptylin"
colnames(part8)[3] = "Venlafaxin"
colnames(part8)[4] = "Zolpidem"


#deleting rows I don't need
part8 = part8[-c(1),]

write.csv(part8, file = "medpart8.csv")



View(part9)
#renaming rows
colnames(part9)[1] = "Dato"
colnames(part9)[2] = "Escitalopram"
colnames(part9)[3] = "Lamotrigin"
colnames(part9)[4] = "Lithium"
colnames(part9)[5] = "Oxazepam"
colnames(part9)[6] = "Quetiapin"
colnames(part9)[7] = "Zolpidem"
colnames(part9)[8] = "Zopiclon" 

#deleting rows I don't need
part9 = part9[-c(1),]

write.csv(part9, file = "medpart9.csv")



# -------------- Plotting


