#Power calculations

rm(list = ls())
setwd("/Users/anissa/Desktop/BRFSS")

library(tidyverse)
library(haven)
library(rigr)

######----------loading in BRFSS 2017----------######

#brfss17xpt <- read_xpt("LLCP2017.XPT ")

#write.csv(brfss17xpt, file = "LLCP2017.csv", row.names = F)
brfss17 <- read.csv("LLCP2017.csv")

#####----------loading in BRFSS 2019----------#####

#brfss19xpt <- read_xpt("LLCP2019.XPT ")

#write.csv(brfss19xpt, file = "LLCP2019.csv", row.names = F)
brfss19 <- read.csv("LLCP2019.csv")



#####----------contraceptive non-use x employment status in 2017----------#####
View(brfss17)

summarize(brfss17)
descrip(brfss17)

#####make a new employment variable with our 4 categories of employment status
#employed for wages / self-employed = 1
brfss17$employCat[brfss17$EMPLOY1==1] <- 1
brfss17$employCat[brfss17$EMPLOY1==2] <- 1

#not employed = 2
brfss17$employCat[brfss17$EMPLOY1==3] <- 2
brfss17$employCat[brfss17$EMPLOY1==4] <- 2
brfss17$employCat[brfss17$EMPLOY1==8] <- 2

#homemaker = 3
brfss17$employCat[brfss17$EMPLOY1==5] <- 3

#student = 4
brfss17$employCat[brfss17$EMPLOY1==6] <- 4

table(brfss17$employCat, useNA = "always")

#labeling employCat
brfss17$employCat <- factor(brfss17$employCat, 
                            levels = 1:4, 
                            labels = c("Employed","Not employed", "Homemaker","Student"))


#####new contraception variable
brfss17$cont[brfss17$PFPPRVN2==1] <- 1
brfss17$cont[brfss17$PFPPRVN2==2] <- 2

table(brfss17$cont, useNA = "always")

#labeling contraception variable
brfss17$cont <- factor(brfss17$cont, levels = 1:2, labels = c("Contraception Yes", "Contraception No"))


table(brfss17$cont, brfss17$employCat, useNA = "always")



#####----------contraceptive non-use x employment status in 2019----------#####
View(brfss19)

summarize(brfss19)
descrip(brfss19)


#####make a new employment variable with our 4 categories of employment status
#employed for wages / self-employed = 1
brfss19$employCat[brfss19$EMPLOY1==1] <- 1
brfss19$employCat[brfss19$EMPLOY1==2] <- 1

#not employed = 2
brfss19$employCat[brfss19$EMPLOY1==3] <- 2
brfss19$employCat[brfss19$EMPLOY1==4] <- 2
brfss19$employCat[brfss19$EMPLOY1==8] <- 2

#homemaker = 3
brfss19$employCat[brfss19$EMPLOY1==5] <- 3

#student = 4
brfss19$employCat[brfss19$EMPLOY1==6] <- 4

table(brfss19$employCat, useNA = "always")

#labeling employCat
brfss19$employCat <- factor(brfss19$employCat, 
                            levels = 1:4, 
                            labels = c("Employed","Not employed", "Homemaker","Student"))


#####new contraception variable
brfss19$cont[brfss19$PFPPRVN3==1] <- 1
brfss19$cont[brfss19$PFPPRVN3==2] <- 2

table(brfss19$cont, useNA = "always")

#labeling contraception variable
brfss19$cont <- factor(brfss19$cont, levels = 1:2, labels = c("Contraception Yes", "Contraception No"))


table(brfss19$cont, brfss19$employCat, useNA = "always")


table(brfss19$employCat)



#####-----CALCULATING POWER-----#####
library(epiR)

#actual study numbers
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.2, 
  n = 70837, 
  power = 0.8, 
  r = 21128/49709)

#10% unexposed, n=5000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.1, 
  n = 70000, 
  power = 0.8, 
  r = 5000/65000)

#10% unexposed, n=10000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.1, 
  n = 70000, 
  power = 0.8, 
  r = 10000/60000)

#10% unexposed, n=15000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.1, 
  n = 70000, 
  power = 0.8, 
  r = 15000/55000)

#10% unexposed, n=20000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.1, 
  n = 70000, 
  power = 0.8, 
  r = 20000/50000)

#10% unexposed, n=30000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.1, 
  n = 70000, 
  power = 0.8, 
  r = 30000/40000)

#20% unexposed, n=5000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.2, 
  n = 70000, 
  power = 0.8, 
  r = 5000/65000)

#20% unexposed, n=10000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.2, 
  n = 70000, 
  power = 0.8, 
  r = 10000/60000)

#20% unexposed, n=15000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.2, 
  n = 70000, 
  power = 0.8, 
  r = 15000/55000)

#20% unexposed, n=20000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.2, 
  n = 70000, 
  power = 0.8, 
  r = 20000/50000)

#20% unexposed, n=30000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.2, 
  n = 70000, 
  power = 0.8, 
  r = 30000/40000)


#30% unexposed, n=5000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.3, 
  n = 70000, 
  power = 0.8, 
  r = 5000/65000)

#30% unexposed, n=10000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.3, 
  n = 70000, 
  power = 0.8, 
  r = 10000/60000)

#30% unexposed, n=15000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.3, 
  n = 70000, 
  power = 0.8, 
  r = 15000/55000)

#30% unexposed, n=20000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.3, 
  n = 70000, 
  power = 0.8, 
  r = 20000/50000)

#30% unexposed, n=30000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.3, 
  n = 70000, 
  power = 0.8, 
  r = 30000/40000)


#40% unexposed, n=5000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.4, 
  n = 70000, 
  power = 0.8, 
  r = 5000/65000)

#40% unexposed, n=10000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.4, 
  n = 70000, 
  power = 0.8, 
  r = 10000/60000)

#40% unexposed, n=15000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.4, 
  n = 70000, 
  power = 0.8, 
  r = 15000/55000)

#40% unexposed, n=20000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.4, 
  n = 70000, 
  power = 0.8, 
  r = 20000/50000)

#40% unexposed, n=30000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.4, 
  n = 70000, 
  power = 0.8, 
  r = 30000/40000)


#50% unexposed, n=5000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.5, 
  n = 70000, 
  power = 0.8, 
  r = 5000/65000)

#50% unexposed, n=10000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.5, 
  n = 70000, 
  power = 0.8, 
  r = 10000/60000)

#50% unexposed, n=15000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.5, 
  n = 70000, 
  power = 0.8, 
  r = 15000/55000)

#50% unexposed, n=20000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.5, 
  n = 70000, 
  power = 0.8, 
  r = 20000/50000)

#50% unexposed, n=30000 exposed
epi.ssxsectn(
  pdexp1 = NA, 
  pdexp0 = 0.5, 
  n = 70000, 
  power = 0.8, 
  r = 30000/4000)
