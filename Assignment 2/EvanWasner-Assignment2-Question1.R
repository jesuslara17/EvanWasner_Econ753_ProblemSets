## Evan Wasner
## Econ 753
## Assignment 2, Question 1

## Set working directory
setwd("I:/Evan/Documents/Umass/Econ 753/EvanWasner_Econ753_ProblemSets/Assignment 2")

## Libraries
library(foreign)
library(tidyverse)
library(nlme)

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

## Filter data into different years
chow59 <- filter(chow, year>=54 & year<=59)
chow65 <- filter(chow, year>=60 & year<=65)

## Construct correlation matrices and save data
cor59 <- cor(select(chow59, lnrent, lnmult, lnaccess, lnadd, lnmem))
cor65 <- cor(select(chow65, lnrent, lnmult, lnaccess, lnadd, lnmem))
save(cor59,file="cor59.Rdata")
save(cor65,file="cor65.Rdata")

################
## Question 3 ##
##   PART b   ##
################

## Run regression of lnrent and save data
chow.lm65 <- lm(lnrent ~ d61 + d62 + d63 + d64 + d65 + lnmult + lnmem + lnaccess, 
               data=chow65)
save(chow.lm65,file="chow.lm65.Rdata")

## Create table for price indices
priceIndexTable <- data.frame(Year=c("1960", "1961", "1962", "1963", "1964", "1965"),
                              Estimated_Coefficient=chow.lm65$coefficients[1:6])
priceIndexTable <- mutate(priceIndexTable, Price_Index=ifelse(Year==1960,1,exp(Estimated_Coefficient)))
save(priceIndexTable,file="priceIndexTable.Rdata")

################
## Question 3 ##
##   PART e   ##
################

chow65 <- mutate(chow65, generalizedvolume=sqrt(volume),
                 generalizedlnrent=lnrent*sqrt(volume),
               generalizedlnmult=lnmult*sqrt(volume),
               generalizedlnaccess=lnaccess*sqrt(volume),
               generalizedlnadd=lnadd*sqrt(volume),
               generalizedlnmem=lnmem*sqrt(volume),
               generalizedd61=d61*sqrt(volume),
               generalizedd62=d62*sqrt(volume),
               generalizedd63=d63*sqrt(volume),
               generalizedd64=d64*sqrt(volume),
               generalizedd65=d65*sqrt(volume))

chow.generalized.lm65 <- lm(generalizedlnrent ~ d61 + d62 + d63 + d64 + d65 + 
                              generalizedlnmult + generalizedlnmem + generalizedlnaccess,
                            data=chow65)

chow.gls65 <- gls(lnrent ~ d61 + d62 + d63 + d64 + d65 + lnmult + lnmem + lnaccess, 
                data=chow65, weights=~sqrt(volume))


## This is just personal stuff testing out how to do regressions with matrices, ignore...
xMatrix <- cbind(numeric(82)+1, 
                 as.matrix(select(chow65, lnmult, lnaccess, lnmem,
                            d61, d62, d63, d64, d65)))
yMatrix <- as.matrix(chow65$lnrent)

betaMatrix <- solve(t(xMatrix) %*% xMatrix) %*% t(xMatrix) %*% yMatrix
summary(chow.lm65)

uhat <- yMatrix - xMatrix %*% betaMatrix
varCovarMatrix <- t(uhat) %*% uhat
sigma <- t(uhat) %*% uhat / (82 - 9)
diag(sigma[1] * solve(t(xMatrix) %*% xMatrix))

## GLS with Matrix Algebra
xMatrixgls <- as.matrix(select(chow65, generalizedvolume, generalizedlnmult, generalizedlnaccess, generalizedlnmem,
                            generalizedd61, generalizedd62, generalizedd63, generalizedd64, generalizedd65))

yMatrixgls <- as.matrix(chow65$generalizedlnrent)

betaMatrixgls <- solve(t(xMatrixgls) %*% xMatrixgls) %*% t(xMatrixgls) %*% yMatrixgls
betaMatrixgls
summary(chow.gls65)