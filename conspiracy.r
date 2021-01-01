library(car)
library(foreign)
library(plyr)
install.packages("ggplot2")
library(ggplot2)
anes19 <- read.csv("Desktop/Rstudio/anes2019.csv")
options(knitr.duplicate.label = "allow")

#clean conspiracy variables
table(anes19$conspire1)
anes19$conspire1 <- recode(anes19$conspire1, "-7=NA")

table(anes19$conspire2)

table(anes19$gender)
anes19$gender2 <- recode(anes19$gender, "1=0; 2=1")
table(anes19$gender2)

table(anes19$conspire3)
anes19$conspire3 <- recode(anes19$conspire3, "-7=NA")

table(anes19$conspire3)

table(anes19$autism1)
anes19$autism1 <- recode(anes19$autism1, "-7=NA")
table(anes19$autism1)

table(anes19$autism2)
anes19$autism2 <- recode(anes19$autism2, "-7=NA")
table(anes19$autism2)

#recode DV's after identifying them
#RACE, recoded as white/nonwhite

table(anes19$race)
anes19$race <- recode(anes19$race, "2:8=2")
anes19$race2 <- recode(anes19$race, "1=0; 2:8=1")
table(anes19$race2)

white <- subset(anes19, race =- 1)
nonwhite <- subset(anes19, race == 2)
#GENDER (1=m,2=f)

#ideology
table(anes19$ideo5)
anes19$ideo5 <- recode(anes19$ideo5, "-7=NA; 6=NA")
table(anes19$ideo5)

#dem/rep leaning 
table(anes19$pid7)
anes19$pid7 <- recode(anes19$pid7, "8=NA")
table(anes19$pid7)

#dem/rep 2 party
table(anes19$pid1d)
anes19$pid1d <- recode(anes19$pid1d, "-7=NA; -1=NA; 3=NA; 4=NA")
table(anes19$pid1d)

#trust in experts
table(anes19$experts)
anes19$experts <- recode(anes19$experts, "-7=NA;")
table(anes19$experts)

#religion
table(anes19$pew_religimp)

#political interest
table(anes19$newsint)
anes19$newsint <- recode(anes19$newsint, "-7=NA")
table(anes19$newsint)

#reddit use
table(anes19$reddit2)
anes19$reddit2 <- recode(anes19$reddit2, "-7= NA; -1=NA")
table(anes19$reddit2)

#facebook use
table(anes19$facebook1)
anes19$facebook1 <- recode(anes19$facebook1, "-1=NA")
table(anes19$facebook1)

#facebook use + prevalency of seeing ads
table(anes19$facebook2)
anes19$facebook2 <- recode(anes19$facebook2, "-1=NA")
table(anes19$facebook2)

#facebook use + posting 
anes19$facebook3 <- recode(anes19$facebook3, "-7=NA; -1=NA")
table(anes19$facebook3)

summary(anes19$conspire1)
##T-TESTING WITH SOME OF THE VARIABLES
##GENDER
t.test(anes19$conspire1~anes19$gender)
#no high score disparity between men and women, but there is a difference.
t.test(anes19$conspire2~anes19$gender)
#not statistically significant that there is a difference between gender.
t.test(anes19$conspire3~anes19$gender)
#no high score disparity between men and women, but there is a difference.

##RACE
t.test(anes19$conspire1~anes19$race)
#no high score disparity between men and women, but there is a difference.
t.test(anes19$conspire2~anes19$race)
#no difference, p0value too high to determine
t.test(anes19$conspire3~anes19$race)
#no difference,but p-value is too high to determine

##PARTY
t.test(anes19$conspire1~anes19$pid1d)
#big difference between dems and reps, 1=dem, 2=rep
t.test(anes19$conspire2~anes19$pid1d)
#no difference, but p-value DNA
t.test(anes19$conspire3~anes19$pid1d)
#reps score higher, high statistical significance.

#TIME FOR SOME MEANS AND SUBSETS
male <- subset(anes19, gender==1)
female <- subset(anes19, gender ==2)

mean(male$conspire1, na.rm=TRUE)
#3.1
mean(female$conspire1, na.rm=TRUE)
#3.24

mean(male$conspire2, na.rm=TRUE)
#2.95
mean(female$conspire2, na.rm=TRUE)
#2.91

mean(male$conspire3, na.rm=TRUE)
#3.07
mean(female$conspire3, na.rm=TRUE)
#2.95

#education mean
mean(anes19$conspire1[anes19$educ==1], na.rm=TRUE)
#3.05
mean(anes19$conspire1[anes19$educ==2], na.rm=TRUE)
#3.29
mean(anes19$conspire1[anes19$educ==3], na.rm=TRUE)
#3.208
mean(anes19$conspire1[anes19$educ==4], na.rm=TRUE)
#3.226
mean(anes19$conspire1[anes19$educ==5], na.rm=TRUE)
#3.09
mean(anes19$conspire1[anes19$educ==6], na.rm=TRUE)
#2.97

mean(anes19$conspire2[anes19$educ==1], na.rm=TRUE)
#2.86
mean(anes19$conspire2[anes19$educ==2], na.rm=TRUE)
#3.02
mean(anes19$conspire2[anes19$educ==3], na.rm=TRUE)
#2.86
mean(anes19$conspire2[anes19$educ==4], na.rm=TRUE)
#2.88
mean(anes19$conspire2[anes19$educ==5], na.rm=TRUE)
#2.87
mean(anes19$conspire2[anes19$educ==6], na.rm=TRUE)
#2.98

mean(anes19$conspire3[anes19$educ==1], na.rm=TRUE)
#2.99
mean(anes19$conspire3[anes19$educ==2], na.rm=TRUE)
#3.17
mean(anes19$conspire3[anes19$educ==3], na.rm=TRUE)
#3.04
mean(anes19$conspire3[anes19$educ==4], na.rm=TRUE)
#3.136
mean(anes19$conspire3[anes19$educ==5], na.rm=TRUE)
#2.8
mean(anes19$conspire3[anes19$educ==6], na.rm=TRUE)
#2.76



#religious importance
mean(anes19$conspire1[anes19$pew_religimp==1], na.rm=TRUE)
#3.2
mean(anes19$conspire1[anes19$pew_religimp==2], na.rm=TRUE)
#3.137
mean(anes19$conspire1[anes19$pew_religimp==3], na.rm=TRUE)
#3.104
mean(anes19$conspire1[anes19$pew_religimp==4], na.rm=TRUE)
#3.22

mean(anes19$conspire2[anes19$pew_religimp==1], na.rm=TRUE)
#2.99
mean(anes19$conspire2[anes19$pew_religimp==2], na.rm=TRUE)
#2.9
mean(anes19$conspire2[anes19$pew_religimp==3], na.rm=TRUE)
#2.93
mean(anes19$conspire2[anes19$pew_religimp==4], na.rm=TRUE)
#2.857

mean(anes19$conspire3[anes19$pew_religimp==1], na.rm=TRUE)
#3.308
mean(anes19$conspire3[anes19$pew_religimp==2], na.rm=TRUE)
#3.02
mean(anes19$conspire3[anes19$pew_religimp==3], na.rm=TRUE)
#2.885
mean(anes19$conspire3[anes19$pew_religimp==4], na.rm=TRUE)
#2.613

#PARTY
mean(anes19$conspire1[anes19$pid1d==1], na.rm=TRUE)
#3.27
mean(anes19$conspire1[anes19$pid1d==2], na.rm=TRUE)
#3.05

mean(anes19$conspire2[anes19$pid1d==1], na.rm=TRUE)
#2.9
mean(anes19$conspire2[anes19$pid1d==2], na.rm=TRUE)
#3.04

mean(anes19$conspire3[anes19$pid1d==1], na.rm=TRUE)
#2.46
mean(anes19$conspire3[anes19$pid1d==2], na.rm=TRUE)
#3.645

#RELIGIOUS IMPORTANCE

pew_relig1 <- subset(anes19,pew_religimp==1)
pew_relig2 <- subset(anes19,pew_religimp==2)
pew_relig3 <- subset(anes19,pew_religimp==3)
pew_relig4 <- subset(anes19,pew_religimp==4)

t.test(pew_relig1$conspire1)
t.test(pew_relig2$conspire1)
t.test(pew_relig3$conspire1)
t.test(pew_relig4$conspire1)

t.test(pew_relig1$conspire2)
t.test(pew_relig2$conspire2)
t.test(pew_relig3$conspire2)
t.test(pew_relig4$cosnspire2)

t.test(pew_relig1$conspire3)
t.test(pew_relig2$conspire3)
t.test(pew_relig3$conspire3)
t.test(pew_relig4$conspire3)

##TRUST
mean(anes19$experts, na.rm=TRUE)
#3.012

#HISTOGRAM
barplot(table(anes19$conspire1),
        main = "Distribution of Conspiracy Question #1 Results",
        ylab = "Count",
        ylim = c(0,1500),
        names.arg=c(" Not at all", "Not very well", "Somewhat well", "Very well", "Extremely well"),
        cex.names = .6)

barplot(table(dems$conspire1),
        main = "Distribution of Conspiracy Question #2 Results",
        ylab = "Count",
        ylim = c(0,400),
        names.arg=c(" Not at all", "Not very well", "Somewhat well", "Very well", "Extremely well"),
        cex.names = .6)

barplot(table(anes19$conspire3),
        main = "Distribution of Conspiracy Question #3 Results",
        ylab = "Count",
        ylim = c(0,1500),
        names.arg=c(" Not at all", "Not very well", "Somewhat well", "Very well", "Extremely well"),
        cex.names = .6)

barplot(table(female$conspire3),
        main = "Distribution of Conspiracy Question #3 Among Women",
        ylab = "Count",
        ylim = c(0,700),
        names.arg=c(" Not at all", "Not very well", "Somewhat well", "Very well", "Extremely well"),
        cex.names = .6)
        
barplot(table(male$conspire2),
        main = "Distribution of Conspiracy Question #1 Among Men",
        ylab = "Count",
        ylim = c(0,700),
        names.arg=c(" Not at all", "Not very well", "Somewhat well", "Very well", "Extremely well"),
        cex.names = .6)

barplot(table(female$conspire1),
        main = "Distribution of Conspiracy Question #1 Among Women",
        ylab = "Count",
        ylim = c(0,700))


barplot(table(white$conspire1),
        main = "Distribution of Conspiracy Questions #1 Among Whites",
        ylab = "Count",
        ylim = c(0,1500),
        names.arg=c(" Not at all", "Not very well", "Somewhat well", "Very well", "Extremely well"),
        cex.names = .7)

barplot(table(nonwhite$conspire1),
        main = "Distribution of Conspiracy Questions #1 Among Nonwhites",
        ylab = "Count",
        ylim = c(0,400),
        names.arg=c(" Not at all", "Not very well", "Somewhat well", "Very well", "Extremely well"),
        cex.names = .7)


#CORRELATION WITH VARIABLES 
#gender
summary(lm(anes19$conspire1~ anes19$gender, data=anes19))
#.135
chisq.test(anes19$conspire1, anes19$gender)
#x^2= 25.893, p= 3.24e-5

summary(lm(anes19$conspire2~ anes19$gender, data=anes19))
#-0.04
chisq.test(anes19$conspire2, anes19$gender)
#x^2= 7.02, p= .13

summary(lm(anes19$conspire3~ anes19$gender, data=anes19))
#-.12
chisq.test(anes19$conspire3, anes19$gender)
#x^2= 40.178, p = 3.976e-8

#RACE 
summary(lm(anes19$conspire1~ anes19$race, data=anes19))
#.107
chisq.test(anes19$conspire1, anes19$race)
#x^2= 8.4135, p=.07

summary(lm(anes19$conspire2~ anes19$race, data=anes19))
#-0.05
chisq.test(anes19$conspire2, anes19$race)
#x^2= 9.6, p=.0475

summary(lm(anes19$conspire3~ anes19$race, data=anes19))
#.01
chisq.test(anes19$conspire3, anes19$race)
#x^2= 16.926

#education
summary(lm(anes19$conspire1~ anes19$educ, data=anes19))
#-0.05
chisq.test(anes19$conspire1, anes19$educ)
#x^2= 53.81, p=6.171e-5

summary(lm(anes19$conspire2~ anes19$educ, data=anes19))
#-0.01
chisq.test(anes19$conspire2, anes19$educ)
#x^2= 51.924, p=.0001

summary(lm(anes19$conspire3~ anes19$educ, data=anes19))
#-.09
chisq.test(anes19$conspire3, anes19$educ)
#x^2= 84.362, p=7.05e-10

#religious importance
summary(lm(anes19$conspire1~ anes19$pew_religimp, data=anes19))
#-0.0008
chisq.test(anes19$conspire1, anes19$pew_religimp)
#x^2= 32, p= .001

summary(lm(anes19$conspire2~ anes19$pew_religimp, data=anes19))
#-.04
chisq.test(anes19$conspire2, anes19$pew_religimp)
#x^2 = 35.14, p = .0004

summary(lm(anes19$conspire3~ anes19$pew_religimp, data=anes19))
#-0.22
chisq.test(anes19$conspire3, anes19$pew_religimp)
#179.45, p= 2.2e-16

#trust
summary(lm(anes19$conspire1~ anes19$experts, data=anes19))
#-0.14
chisq.test(anes19$conspire1, anes19$experts)
#x^2= 170.7, p=2.2e-16

summary(lm(anes19$conspire2~ anes19$experts, data=anes19))
#-.0079
chisq.test(anes19$conspire2, anes19$experts)
#x^2= 98.719, p=6.018e-14

summary(lm(anes19$conspire3~ anes19$experts, data=anes19))
#-.0459
chisq.test(anes19$conspire3, anes19$experts)
#x^2= 738.89, p = 2.2e-16


#LAST ONE OF TRUST IS SIGNIFICANT

#party
summary(lm(anes19$conspire1~ anes19$pid1d, data=anes19))
#-0.2178
chisq.test(anes19$conspire1, anes19$pid1d)
#x^2= 13.931, p=.007

summary(lm(anes19$conspire2~ anes19$pid1d, data=anes19))
#0.113
chisq.test(anes19$conspire2, anes19$pid1d)
#x^2= 3.464, p=.48

summary(lm(anes19$conspire3~ anes19$pid1d, data=anes19))
#1.1 !!!!!!
chisq.test(anes19$conspire3, anes19$pid1d)
#x^2= 202.41, p =2.2e-16

#news interest
summary(lm(anes19$conspire1~ anes19$newsint, data=anes19))
#0.0159
chisq.test(anes19$conspire1, anes19$newsint)
#x^2= 95.805, p = 2.105e-13

summary(lm(anes19$conspire2~ anes19$newsint, data=anes19))
#-0.03626
chisq.test(anes19$conspire2, anes19$newsint)
#x^2= 52.094, p=1.058e-5

summary(lm(anes19$conspire3~ anes19$newsint, data=anes19))
#-0.0077
chisq.test(anes19$conspire3, anes19$newsint)
#x^2= 216.29, p=2.2e-16


#FACEBOOK USE
chisq.test(anes19$conspire1, anes19$facebook1)
#x^2= 27.35, p =.288
chisq.test(anes19$conspire2, anes19$facebook1)
#x^2= 35.715, p= .058

chisq.test(anes19$conspire3, anes19$facebook1)
#x&2= 42.419, p =.011

#PARTY IDENTIFICATION 7 POINTS
chisq.test(anes19$conspire1, anes19$pid7)
#x=42, p=.0129
chisq.test(anes19$conspire2, anes19$pid7)
#x= 42.192, p=.01228
chisq.test(anes19$conspire3, anes19$pid7)
#x= 739.45,p=2.2e-16
#CONTROLLED COMPARISONS
dems<- subset(anes19, pid1d==1)
reps <- subset(anes19,pid1d==2)

cor(anes19$conspire, anes19$gender)

#trust
mean(dems$conspire1[dems$experts], na.rm=TRUE)
#2.80
mean(dems$conspire2[dems$experts], na.rm=TRUE)
#2.42
mean(dems$conspire3[dems$experts], na.rm=TRUE)
#3.718

mean(reps$conspire1[dems$experts], na.rm=TRUE)
#2.94
mean(reps$conspire2[dems$experts], na.rm=TRUE)
#2.45
mean(reps$conspire3[dems$experts], na.rm=TRUE)
#3.916

#religion
mean(dems$conspire1[dems$pew_religimp], na.rm=TRUE)
#2.97
mean(dems$conspire2[dems$pew_religimp], na.rm=TRUE)
#2.757
mean(dems$conspire3[dems$pew_religimp], na.rm=TRUE)
#2.34

mean(reps$conspire1[dems$pew_religimp], na.rm=TRUE)
#3.09
mean(reps$conspire2[dems$pew_religimp], na.rm=TRUE)
#3.408
mean(reps$conspire3[dems$pew_religimp], na.rm=TRUE)
#3.712

#political interest
mean(dems$conspire1[dems$newsint], na.rm=TRUE)
#3.99
mean(dems$conspire2[dems$newsint], na.rm=TRUE)
#2.81
mean(dems$conspire3[dems$newsint], na.rm=TRUE)
#1.73

mean(reps$conspire1[dems$newsint], na.rm=TRUE)
#2.63
mean(reps$conspire2[dems$newsint], na.rm=TRUE)
#3.99
mean(reps$conspire3[dems$newsint], na.rm=TRUE)
#3.44


mean(dems$conspire1[dems$gender==1], na.rm=TRUE)
mean(reps$conspire1[reps$gender==1], na.rm=TRUE)

mean(dems$conspire1[dems$gender==2], na.rm=TRUE)
mean(reps$conspire1[reps$gender==2], na.rm=TRUE)

mean(dems$conspire2[dems$gender==1], na.rm=TRUE)
mean(reps$conspire2[reps$gender==1], na.rm=TRUE)

mean(dems$conspire2[dems$gender==2], na.rm=TRUE)
mean(reps$conspire2[reps$gender==2], na.rm=TRUE)

mean(dems$conspire3[dems$gender==1], na.rm=TRUE)
mean(reps$conspire3[reps$gender==1], na.rm=TRUE)

mean(dems$conspire3[dems$gender==2], na.rm=TRUE)
mean(reps$conspire3[reps$gender==2], na.rm=TRUE)

mean(dems$conspire1[dems$race==1], na.rm=TRUE)
mean(reps$conspire1[reps$race==1], na.rm=TRUE)

mean(dems$conspire1[dems$race==2], na.rm=TRUE)
mean(reps$conspire1[reps$race==2], na.rm=TRUE)

mean(dems$conspire2[dems$race==1], na.rm=TRUE)
mean(reps$conspire2[reps$race==1], na.rm=TRUE)

mean(dems$conspire2[dems$race==2], na.rm=TRUE)
mean(reps$conspire2[reps$race==2], na.rm=TRUE)

mean(dems$conspire3[dems$race==1], na.rm=TRUE)
mean(reps$conspire3[reps$race==1], na.rm=TRUE)

mean(dems$conspire3[dems$race==2], na.rm=TRUE)
mean(reps$conspire3[reps$race==2], na.rm=TRUE)

mean(dems$conspire1[dems$facebook1], na.rm=TRUE)
mean(reps$conspire1[reps$facebook1], na.rm=TRUE)

mean(dems$conspire2[dems$facebook1], na.rm=TRUE)
mean(reps$conspire2[reps$facebook1], na.rm=TRUE)

mean(dems$conspire3[dems$facebook1], na.rm=TRUE)
mean(reps$conspire3[reps$facebook1], na.rm=TRUE)

mean(dems$conspire3[dems$pid7], na.rm=TRUE)
mean(reps$conspire3[reps$pid7], na.rm=TRUE)

t.test(anes19$conspire1~anes19$pid1d)

mean(reps$conspire1[reps$race==1], na.rm=TRUE)

mean(dems$conspire3[dems$race==2], na.rm=TRUE)
mean(reps$conspire3[reps$race==2], na.rm=TRUE)


mean(reps$conspire3[reps$educ], na.rm=TRUE)
#3.99
mean(dems$conspire2[dems$newsint], na.rm=TRUE)
#2.81
mean(dems$conspire3[dems$newsint], na.rm=TRUE)
#1.73
#time for the big cahones
#COMPLETE MODELS
summary(lm(anes19$conspire1~ anes19$gender2+anes19$race2+anes19$educ+anes19$pew_religimp+anes19$experts+anes19$pid7+anes19$facebook1+anes19$newsint, data=anes19))
summary(lm(anes19$conspire2~ anes19$gender2+anes19$race2+anes19$educ+anes19$pew_religimp+anes19$experts+anes19$pid7+anes19$facebook1+anes19$newsint, data=anes19))         
summary(lm(anes19$conspire3~ anes19$gender2+anes19$race2+anes19$educ+anes19$pew_religimp+anes19$experts+anes19$pid7+anes19$facebook1+anes19$newsint, data=anes19))  

#MODELS BASED ON CHI SQUARED ASSOCIATION
summary(lm(anes19$conspire1~anes19$gender+anes19$educ+anes19$pew_religimp+anes19$experts+anes19$newsint+anes19$facebook1+anes19$pid7, data=anes19))
summary(lm(anes19$conspire2~anes19$educ+anes19$pew_religimp+anes19$experts+anes19$newsint+anes19$facebook1+anes19$pid7, data=anes19))
summary(lm(anes19$conspire3~anes19$gender+anes19$educ+anes19$pew_religimp+anes19$experts+anes19$pid1d+anes19$newsint+anes19$facebook1+anes19$pid7, data=anes19))
