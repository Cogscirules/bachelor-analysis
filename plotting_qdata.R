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

?mutate



csd_scaled = group_by(csd, add = FALSE) %>%
  mutate(pat1 = scale(pat1),
         pat2 = scale(pat2),
         pat3 = scale(pat3),
         pat5 = scale(pat5),
         pat8 = scale(pat8),
         pat9 = scale(pat9),
         pat10 = scale(pat10),
         pat11 = scale(pat11),
         control = scale(control), 
         ) 
?group_by

View(csd_scaled)

csd_scaled = csd_scaled[-c(5)]

colnames(csd_scaled)[5] <- "pat4"
colnames(csd_scaled)[6] <- "pat5"
colnames(csd_scaled)[7] <- "pat6"
colnames(csd_scaled)[8] <- "pat7"
colnames(csd_scaled)[9] <- "pat8"

#saving to csv
write.csv(csd, file = "csd.csv")
write.csv(csd_scaled, file = "csd_scaled.csv")



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

?gsub

hads = read_csv("hads.csv",  col_names = TRUE)

#deleting rows I don't need
hads = hads[-c(6,7,12:51),]

#changing values/dates
hads[1,2] = "18-01-2018"


#changing date values to understandable dates
hads[hads==51618] = "2018-05-16"


View(hads)

hads=hads[-c(2,3)]

colnames(hads)[1] <- "patient"
hads$lengthofstay = c(33,9,23,35,9,8,5,21,16)
hads=hads[c(87,1:86)]

#saving to csv
write.csv(hads, file = "hads.csv")


#deleting columns for another df

hadssum = hads[ -c(3:16,20:33,37:50,54:67,71:84)]
hadssum$lengthofstay = c(33,9,23,35,9,8,5,21,16)
hadssum=hadssum[c(17,1:16)]
colnames(hadssum)[2] ="patient"


View(hadssum)

#changing date values
hadssum[hadssum==11818] = "2018-01-18"
hadssum[hadssum==12418] = "2018-01-24"
hadssum[hadssum==41118] = "2018-04-11"
hadssum[hadssum==41918] = "2018-04-19"
hadssum[hadssum==52318] = "2018-05-23"
hadssum[hadssum==61218] = "2018-06-12"
hadssum[hadssum==62018] = "2018-06-20"
hadssum[hadssum==62118] = "2018-06-21"
hadssum[hadssum==71618] = "2018-07-16"
hadssum[hadssum==12518] = "2018-01-25"
hadssum[hadssum==13118] = "2018-01-31"
hadssum[hadssum==42518] = "2018-04-25"
hadssum[hadssum==53018] = "2018-05-30"
hadssum[hadssum==62818] = "2018-06-28"
hadssum[hadssum==62518] = "2018-06-25"
hadssum[hadssum==73118] = "2018-07-31"
hadssum[hadssum==50318] = "2018-05-03"
hadssum[hadssum==21318] = "2018-02-13"
hadssum[hadssum==51618] = "2018-05-16"


#saving to csv
write.csv(hadssum, file = "hadssum.csv")


# -------------- Plotting Hamilton-D

hamd = read_excel("datasleep.xlsx", sheet = 3, range = NULL, col_names = TRUE,
                  col_types = NULL, na = "")
#deleting rows I don't need
hamd = hamd[-c(6,7,12:51),]


hamd$lengthofstay = c(33,9,23,35,9,8,5,21,16)
hamd=hamd[c(100,1:99)]
colnames(hamd)[2] ="patient"



View(hamd)


#changing date values
hamd[hamd==11818] = "2018-01-18"
hamd[hamd==12418] = "2018-01-24"
hamd[hamd==41118] = "2018-04-11"
hamd[hamd==41918] = "2018-04-19"
hamd[hamd==52318] = "2018-05-23"
hamd[hamd==61218] = "2018-06-12"
hamd[hamd==62018] = "2018-06-20"
hamd[hamd==62118] = "2018-06-21"
hamd[hamd==71618] = "2018-07-16"
hamd[hamd==12518] = "2018-01-25"
hamd[hamd==13118] = "2018-01-31"
hamd[hamd==42518] = "2018-04-25"
hamd[hamd==53018] = "2018-05-30"
hamd[hamd==62818] = "2018-06-28"
hamd[hamd==62518] = "2018-06-25"
hamd[hamd==73118] = "2018-07-31"
hamd[hamd==50318] = "2018-05-03"
hamd[hamd==21318] = "2018-02-13"
hamd[hamd==51618] = "2018-05-16"
hamd[hamd==41018] = "2018-04-10"


#saving to csv
write.csv(hamd, file = "ham-d.csv")


#deleting columns for another df

hamdsum = read_csv("ham-dsum.csv",  col_names = TRUE)

hamdsum = hamd[ c(1,2,20:21, 42:46, 64:68, 86:89)]
hamdsum=hamdsum[-c(1)]

hamdsum$lengthofstay = c(33,9,23,35,9,8,5,21,16)
hamdsum=hamdsum[c(19,1:18)]
colnames(hamdsum)[2] ="patient"


View(hamdsum)

#changing date values
hamdsum[hamdsum==11818] = "2018-01-18"
hamdsum[hamdsum==12418] = "2018-01-24"
hamdsum[hamdsum==41118] = "2018-04-11"
hamdsum[hamdsum==41918] = "2018-04-19"
hamdsum[hamdsum==52318] = "2018-05-23"
hamdsum[hamdsum==61218] = "2018-06-12"
hamdsum[hamdsum==62018] = "2018-06-20"
hamdsum[hamdsum==62118] = "2018-06-21"
hamdsum[hamdsum==71618] = "2018-07-16"
hamdsum[hamdsum==12518] = "2018-01-25"
hamdsum[hamdsum==13118] = "2018-01-31"
hamdsum[hamdsum==42518] = "2018-04-25"
hamdsum[hamdsum==53018] = "2018-05-30"
hamdsum[hamdsum==62818] = "2018-06-28"
hamdsum[hamdsum==62518] = "2018-06-25"
hamdsum[hamdsum==73118] = "2018-07-31"
hamdsum[hamdsum==50318] = "2018-05-03"
hamdsum[hamdsum==21318] = "2018-02-13"
hamdsum[hamdsum==51618] = "2018-05-16"
hamdsum[hamdsum==41018] = "2018-04-10"



#saving to csv
write.csv(hamdsum, file = "ham-dsum.csv")

# ------------ Plotting ISI

isi = read.csv("isi.csv")



#deleting rows I don't need
isi = isi[-c(6,7,12:5),]

isi=isi[-c(2)]

isi$lengthofstay = c(33,9,23,35,9,8,5,21,16)
isi=isi[c(48, 1:47)]
colnames(isi)[2] ="patient"


View(isi)


#changing date values
isi[isi==11818] = "2018-01-18"
isi[isi==12418] = "2018-01-24"
isi[isi==41118] = "2018-04-11"
isi[isi==41918] = "2018-04-19"
isi[isi==52318] = "2018-05-23"
isi[isi==61218] = "2018-06-12"
isi[isi==62018] = "2018-06-20"
isi[isi==62118] = "2018-06-21"
isi[isi==71618] = "2018-07-16"
isi[isi==12518] = "2018-01-25"
isi[isi==13118] = "2018-01-31"
isi[isi==42518] = "2018-04-25"
isi[isi==53018] = "2018-05-30"
isi[isi==62818] = "2018-06-28"
isi[isi==62518] = "2018-06-25"
isi[isi==73118] = "2018-07-31"
isi[isi==50318] = "2018-05-03"
isi[isi==21318] = "2018-02-13"
isi[isi==51618] = "2018-05-16"
isi[isi==41018] = "2018-04-10"




#saving to csv
write.csv(isi, file = "isi.csv")


#deleting columns for another df

isisum = isi[ -c(3:9, 12:18, 21:27, 30:36, 39:45)]

isisum$lengthofstay = c(33,9,23,35,9,8,5,21,16)
isisum=isisum[c(12, 1:11)]
colnames(isisum)[2] ="patient"

View(isisum)

isisum[isisum==11818] = "2018-01-18"
isisum[isisum==12418] = "2018-01-24"
isisum[isisum==41118] = "2018-04-11"
isisum[isisum==41918] = "2018-04-19"
isisum[isisum==52318] = "2018-05-23"
isisum[isisum==61218] = "2018-06-12"
isisum[isisum==62018] = "2018-06-20"
isisum[isisum==62118] = "2018-06-21"
isisum[isisum==71618] = "2018-07-16"
isisum[isisum==12518] = "2018-01-25"
isisum[isisum==13118] = "2018-01-31"
isisum[isisum==42518] = "2018-04-25"
isisum[isisum==53018] = "2018-05-30"
isisum[isisum==62818] = "2018-06-28"
isisum[isisum==62518] = "2018-06-25"
isisum[isisum==73118] = "2018-07-31"
isisum[isisum==50318] = "2018-05-03"
isisum[isisum==21318] = "2018-02-13"
isisum[isisum==51618] = "2018-05-16"
isisum[isisum==41018] = "2018-04-10"




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


part1[part1 == 43115] = "2018-01-15"
part1[part1 == 43116] = "2018-01-16"
part1[part1 == 43117] = "2018-01-17"
part1[part1 == 43118] = "2018-01-18"
part1[part1 == 43119] = "2018-01-19"
part1[part1 == 43120] = "2018-01-20"
part1[part1 == 43121] = "2018-01-21"
part1[part1 == 43122] = "2018-01-22"
part1[part1 == 43123] = "2018-01-23"
part1[part1 == 43124] = "2018-01-24"
part1[part1 == 43125] = "2018-01-25"
part1[part1 == 43126] = "2018-01-26"
part1[part1 == 43127] = "2018-01-27"
part1[part1 == 43128] = "2018-01-28"
part1[part1 == 43129] = "2018-01-29"
part1[part1 == 43130] = "2018-01-30"
part1[part1 == 43131] = "2018-01-31"
part1[part1 == 43132] = "2018-02-01"
part1[part1 == 43133] = "2018-02-02"
part1[part1 == 43134] = "2018-02-03"
part1[part1 == 43135] = "2018-02-04"
part1[part1 == 43136] = "2018-02-05"
part1[part1 == 43137] = "2018-02-06"
part1[part1 == 43138] = "2018-02-07"
part1[part1 == 43139] = "2018-02-08"
part1[part1 == 43140] = "2018-02-09"
part1[part1 == 43141] = "2018-02-10"
part1[part1 == 43142] = "2018-02-11"
part1[part1 == 43143] = "2018-02-12"
part1[part1 == 43144] = "2018-02-13"
part1[part1 == 43145] = "2018-02-14"
part1[part1 == 43146] = "2018-02-15"
part1[part1 == 43147] = "2018-02-16"
part1[part1 == 43148] = "2018-02-17"
part1[part1 == 43149] = "2018-02-18"
part1[part1 == 43150] = "2018-02-19"
part1[part1 == 43151] = "2018-02-20"
part1[part1 == 43165] = "2018-03-06"
part1[part1 == 43166] = "2018-03-07"




write.csv(part1, file = "medpart1.csv")


View(part2)
#renaming rows
colnames(part2)[1] = "Dato"


part2[part2 == 43122] = "2018-01-22"
part2[part2 == 43123] = "2018-01-23"
part2[part2 == 43124] = "2018-01-24"
part2[part2 == 43125] = "2018-01-25"
part2[part2 == 43126] = "2018-01-26"
part2[part2 == 43127] = "2018-01-27"
part2[part2 == 43128] = "2018-01-28"
part2[part2 == 43129] = "2018-01-29"
part2[part2 == 43130] = "2018-01-30"
part2[part2 == 43131] = "2018-01-31"
part2[part2 == 43132] = "2018-02-01"
part2[part2 == 43133] = "2018-02-02"
part2[part2 == 43134] = "2018-02-03"


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
#deliting columns
part3 = part3[ -c(1)]
part3 = read_csv("medpart3.csv", col_names = TRUE)

View(part3)

part3[part3 == 43199] = "2018-04-09"
part3[part3 == 43200] = "2018-04-10"
part3[part3 == 43201] = "2018-04-11"
part3[part3 == 43202] = "2018-04-12"
part3[part3 == 43203] = "2018-04-13"
part3[part3 == 43204] = "2018-04-14"
part3[part3 == 43205] = "2018-04-15"
part3[part3 == 43206] = "2018-04-16"
part3[part3 == 43207] = "2018-04-17"
part3[part3 == 43209] = "2018-04-19"
part3[part3 == 43210] = "2018-04-20"
part3[part3 == 43211] = "2018-04-21"
part3[part3 == 43212] = "2018-04-22"



write.csv(part3, file = "medpart3.csv")


#Not entirely sure I need participant 4??
"
View(part4)
#renaming rows
colnames(part4)[1] = "Dato"


part3[part3 == 43206] = "2018-04-16"
part3[part3 == 43207] = "2018-04-17"
part3[part3 == 43209] = "2018-04-19"
part3[part3 == 43210] = "2018-04-20"
part3[part3 == 43211] = "2018-04-21"
part3[part3 == 43212] = "2018-04-22"
part3[part3 == 43213] = "2018-04-16"
part3[part3 == 43214] = "2018-04-17"
part3[part3 == 43215] = "2018-04-19"
part3[part3 == 43216] = "2018-04-20"
part3[part3 == 43217] = "2018-04-21"
part3[part3 == 43218] = "2018-04-22"
part3[part3 == 43219] = "2018-04-16"
part3[part3 == 43220] = "2018-04-17"
part3[part3 == 43221] = "2018-04-19"
part3[part3 == 43222] = "2018-04-20"
part3[part3 == 43223] = "2018-04-21"
part3[part3 == 43224] = "2018-04-22"
part3[part3 == 43225] = "2018-04-16"
part3[part3 == 43226] = "2018-04-17"
part3[part3 == 43227] = "2018-04-19"
part3[part3 == 43228] = "2018-04-20"
part3[part3 == 43229] = "2018-04-21"
part3[part3 == 43230] = "2018-04-22"
part3[part3 == 43231] = "2018-04-16"
part3[part3 == 43232] = "2018-04-17"
part3[part3 == 43234] = "2018-04-19"
part3[part3 == 43235] = "2018-04-20"
part3[part3 == 43236] = "2018-04-21"
part3[part3 == 43237] = "2018-04-22"
part3[part3 == 43238] = "2018-04-16"
part3[part3 == 43239] = "2018-04-17"
part3[part3 == 43240] = "2018-04-19"
part3[part3 == 43241] = "2018-04-20"
part3[part3 == 43242] = "2018-04-21"
part3[part3 == 43243] = "2018-04-22"
part3[part3 == 43244] = "2018-04-16"



write.csv(part4, file = "medpart4.csv") 
"

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

part5[part5 == 43242] = "2018-05-22"
part5[part5 == 43243] = "2018-05-23"
part5[part5 == 43244] = "2018-05-24"


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


part6[part6 == 43259] = "2018-06-08"
part6[part6 == 43260] = "2018-06-09"
part6[part6 == 43261] = "2018-06-10"
part6[part6 == 43262] = "2018-06-11"
part6[part6 == 43263] = "2018-06-12"
part6[part6 == 43264] = "2018-06-13"


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


part7[part7 == 43265] = "2018-06-14"
part7[part7 == 43266] = "2018-06-15"
part7[part7 == 43267] = "2018-06-16"
part7[part7 == 43268] = "2018-06-17"
part7[part7 == 43269] = "2018-06-18"
part7[part7 == 43270] = "2018-06-19"
part7[part7 == 43271] = "2018-06-20"
part7[part7 == 43272] = "2018-06-21"
part7[part7 == 43273] = "2018-06-22"
part7[part7 == 43274] = "2018-06-23"
part7[part7 == 43275] = "2018-06-24"
part7[part7 == 43276] = "2018-06-25"
part7[part7 == 43277] = "2018-06-26"
part7[part7 == 43278] = "2018-06-27"
part7[part7 == 43279] = "2018-06-28"
part7[part7 == 43280] = "2018-06-29"
part7[part7 == 43281] = "2018-06-30"
part7[part7 == 43282] = "2018-07-01"
part7[part7 == 43283] = "2018-07-02"
part7[part7 == 43284] = "2018-07-03"
part7[part7 == 43285] = "2018-07-04"
part7[part7 == 43286] = "2018-07-05"
part7[part7 == 43287] = "2018-07-06"
part7[part7 == 43288] = "2018-07-07"
part7[part7 == 43289] = "2018-07-08"
part7[part7 == 43290] = "2018-07-09"
part7[part7 == 43291] = "2018-07-10"
part7[part7 == 43292] = "2018-07-11"
part7[part7 == 43293] = "2018-07-12"
part7[part7 == 43294] = "2018-07-13"
part7[part7 == 43295] = "2018-07-14"
part7[part7 == 43296] = "2018-07-15"
part7[part7 == 43297] = "2018-07-16"
part7[part7 == 43298] = "2018-07-17"
part7[part7 == 43299] = "2018-07-18"
part7[part7 == 43300] = "2018-07-19"
part7[part7 == 43301] = "2018-07-20"
part7[part7 == 43302] = "2018-07-21"
part7[part7 == 43303] = "2018-07-22"
part7[part7 == 43304] = "2018-07-23"
part7[part7 == 43305] = "2018-07-24"
part7[part7 == 43306] = "2018-07-25"
part7[part7 == 43307] = "2018-07-26"


write.csv(part7, file = "medpart7.csv")


View(part8)
#renaming rows
colnames(part8)[1] = "Dato"
colnames(part8)[2] = "Nortriptylin"
colnames(part8)[3] = "Venlafaxin"
colnames(part8)[4] = "Zolpidem"


#deleting rows I don't need
part8 = part8[-c(1),]

part8[part8 == 43271] = "2018-06-20"
part8[part8 == 43272] = "2018-06-21"
part8[part8 == 43273] = "2018-06-22"
part8[part8 == 43274] = "2018-06-23"
part8[part8 == 43275] = "2018-06-24"
part8[part8 == 43276] = "2018-06-25"
part8[part8 == 43277] = "2018-06-26"


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


part9[part9 == 43294] = "2018-07-13"
part9[part9 == 43295] = "2018-07-14"
part9[part9 == 43296] = "2018-07-15"
part9[part9 == 43297] = "2018-07-16"
part9[part9 == 43298] = "2018-07-17"
part9[part9 == 43299] = "2018-07-18"
part9[part9 == 43300] = "2018-07-19"
part9[part9 == 43301] = "2018-07-20"
part9[part9 == 43302] = "2018-07-21"
part9[part9 == 43303] = "2018-07-22"
part9[part9 == 43304] = "2018-07-23"
part9[part9 == 43305] = "2018-07-24"
part9[part9 == 43306] = "2018-07-25"
part9[part9 == 43307] = "2018-07-26"
part9[part9 == 43308] = "2018-07-27"
part9[part9 == 43309] = "2018-07-28"
part9[part9 == 43310] = "2018-07-29"
part9[part9 == 43311] = "2018-07-30"
part9[part9 == 43312] = "2018-07-31"
part9[part9 == 43313] = "2018-08-01"
part9[part9 == 43314] = "2018-08-02"
part9[part9 == 43315] = "2018-08-03"
part9[part9 == 43316] = "2018-08-04"
part9[part9 == 43317] = "2018-08-05"
part9[part9 == 43318] = "2018-08-06"





write.csv(part9, file = "medpart9.csv")



# lets try and combinet them!
part1$Dato = as.Date()
part2$patient = 2
part3$patient = 3
part5$patient = 4
part6$patient = 5
part7$patient = 6
part8$patient = 7
part9$patient = 8




meddata = merge(part1, part2, all = TRUE)

View(part1)

# -------------- Plotting




