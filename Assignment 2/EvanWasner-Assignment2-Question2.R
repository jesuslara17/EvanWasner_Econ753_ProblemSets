## Evan Wasner
## Econ 753
## Assignment 2, Question 1

## Set working directory
setwd("I:/Evan/Documents/Umass/Econ 753/EvanWasner_Econ753_ProblemSets/Assignment 2")

## Libraries
library(tidyverse)
library(XLConnect)

## Clear workplace
rm(list = ls())
dev.off()

## Read CPI-U data from spreadsheet
cpiU <- readWorksheetFromFile("CPI_U.xls",
                              sheet="Monthly",
                              startRow=1,
                              startCol=1)
cpiU <- cbind(cpiU, "CPI-U")
colnames(cpiU) <- c("date", "cpi", "type")
CcpiU <- readWorksheetFromFile("C_CPI_U.xls",
                               sheet="FRED Graph",
                               startRow=11,
                               startCol=1)
CcpiU <- cbind(CcpiU, "C-CPI-U")
colnames(CcpiU) <- c("date", "cpi", "type")

## Separate year and month
cpiU <- mutate(cpiU, year=str_split_fixed(date, "-", 3)[,1], month=str_split_fixed(date, "-", 3)[,2])
CcpiU <- mutate(CcpiU, year=str_split_fixed(date, "-", 3)[,1], 
                month=str_split_fixed(date, "-", 3)[,2])

## Filter for annual data
cpiUAnnual <- filter(cpiU, month=="01")
CcpiUAnnual <- filter(CcpiU, month=="01")


## Calculate inflation rate
cpiUAnnual <- mutate(cpiUAnnual, previous=lag(cpi), inflation=100*(cpi-previous)/previous)
CcpiUAnnual <- mutate(CcpiUAnnual, previous=lag(cpi), inflation=100*(cpi-previous)/previous)



ggplot(filter(bind_rows(cpiUAnnual,CcpiUAnnual), year >= 2000), aes(x=year)) + geom_point(aes(y=inflation, color=type)) +
  scale_x_discrete(breaks=c(1970,1980,1990,2000,2010,2020))
