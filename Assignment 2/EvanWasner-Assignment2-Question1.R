## Evan Wasner
## Econ 753
## Assignment 2, Question 1

## Set working directory
setwd("I:/Evan/Documents/Umass/Econ 753/EvanWasner_Econ753_ProblemSets/Assignment 2")

## Libraries
library(foreign)
library(dplyr)

## Clear workplace
rm(list = ls())
dev.off()


################
## Question 3 ##
##   PART a   ##
################

## Import data
chow <- read.dta("http://courses.umass.edu/econ753/berndt/stata/chow.dta")

## Add columns
chow <- mutate(chow, lnrent=log(rent), lnmult=log(mult), lnaccess=log(access), 
               lnadd=log(add), mem=words*binary*digits, lnmem=log(words*binary*digits),
               d61=ifelse(year==61,1,0), d62=ifelse(year==62,1,0), d63=ifelse(year==63,1,0),
               d64=ifelse(year==64,1,0), d65=ifelse(year==65,1,0))

## Construct correlation matrices and save data
cor59 <- cor(select(filter(chow, year>=54 & year<=59), lnrent, lnmult, lnaccess, lnadd, lnmem))
cor65 <- cor(select(filter(chow, year>=60 & year<=65), lnrent, lnmult, lnaccess, lnadd, lnmem))
save(cor59,file="cor59.Rdata")
save(cor65,file="cor65.Rdata")

################
## Question 3 ##
##   PART b   ##
################

chow.lm65 <- lm(lnrent ~ d61 + d62 + d63 + d64 + d65 + lnmult + lnmem + lnaccess, 
               data=filter(chow, year>=60 & year<=65))

save(chow.lm65,file="chow.lm65.Rdata")

## This is a test