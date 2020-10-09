## Evan Wasner
## Econ 753
## Assignment 1, Question 1

## Set working directory
setwd("I:/Evan/Documents/Umass/Econ 753/git-econ753/lab-03-io-tables")

## Libraries
library(XLConnect)
library(gridExtra)

## Clear workplace
rm(list = ls())
dev.off()


############
## PART A ##
############


##Import IO data, Employment Figures, and weights
rawIO <- readWorksheetFromFile("IND_NIOT_row_09132019.xlsx",
                              sheet="2009",
                              startRow=4,
                              startCol=4,
                              colTypes="numeric")
rawEmployment <- readWorksheetFromFile("India-Input-Output Analysis--Employment Estimates--09132019.xlsx",
                                    sheet="EO Matrix",
                                    startCol=8,
                                    colTypes="numeric")
weights <- readWorksheetFromFile("India-Input-Output Analysis--Employment Estimates--09132019.xlsx",
                                 sheet="Green Energy Program",
                                 region = "D11:AL20",
                                 header=FALSE,
                                 forceConversion=TRUE,
                                 colTypes="numeric")
weights[is.na(weights)] <- 0
weights <- cbind(weights,0,0,0)
colnames(weights) <- colnames(rawIO)[2:36]
rownames(weights) <- t(readWorksheetFromFile("India-Input-Output Analysis--Employment Estimates--09132019.xlsx",
                                           sheet="Green Energy Program",
                                           region = "A11:A20",
                                           header=FALSE))

## Create domestic, import, output, and totalEmployment matrices from raw data
domestic <- data.matrix(rawIO[c(3:37),c(2:36)])
import <- data.matrix(rawIO[c(38:72),c(2:36)])
output <- data.matrix(rawIO[c(80),c(2:36)])
totalEmployment <- data.matrix(rawEmployment[c(2:36),3])

## Combine domestic and import matrices
domesticPlusImport <- domestic + import

## Setup A-Matrix
aMatrix <- domesticPlusImport

## Convert A-Matrix to ratios
i <- 1
for(val in output)
{
  aMatrix[1:35,i] <- aMatrix[1:35,i] / val
  i <- i + 1
}

## Identity Matrix
I <- diag(numeric(35)+1)

## I-A Matrix
IminusA <- I - aMatrix

## I-A Inverse Matrix
IminusAinverse <- solve(IminusA)

## EO Vector
EO <- matrix(nrow=35, ncol=1)
i <- 1
for(val in totalEmployment)
{
  EO[i] <- val / output[i]
  i <- i + 1
}

## Employment Matrix
employmentMatrix <- matrix(nrow=35, ncol=35)
i <- 1
for(val in EO)
{
  employmentMatrix[i,1:35] <- IminusAinverse[i,1:35] * val
  i <- i + 1
}

## Total employment numbers
totalEmployment <- matrix(nrow=3, ncol=35)
colnames(totalEmployment) <- colnames(rawIO)[2:36]
rownames(totalEmployment) <- c("Total Employment", "Direct Employment", "Indirect Employment")
totalEmployment[1,1:35] <- colSums(employmentMatrix)
totalEmployment[2,1:35] <- t(diag(employmentMatrix))
totalEmployment[3,1:35] <- totalEmployment[1,1:35]-totalEmployment[2,1:35]

## Results: employment by sector
employmentBySector <- totalEmployment %*% t(weights)

## Create matrix for category weights
categoryWeights <- matrix(nrow=3, ncol=10)
categoryWeights[1,1:5] <- c(0.2,0.2,0.2,0.2,0.2)
categoryWeights[2,6:8] <- c(0.5,0.25,0.25)
categoryWeights[3,9:10] <- c(0.5,0.5)
categoryWeights[is.na(categoryWeights)] <- 0
rownames(categoryWeights) <- c("Renewable Energy", "Energy Efficiency", "Fossil Fuels")
colnames(categoryWeights) <- colnames(employmentBySector)

## Results: weighted averages 
weightedAverages <- employmentBySector %*% t(categoryWeights)

## Replicate Table 10
resultsA <- matrix(nrow=18, ncol=3)
rownames(resultsA) <- c("Renewables", colnames(employmentBySector)[1:5], "Weighted Average", "___",
                        "Energy Efficiency", colnames(employmentBySector)[6:8], "Weighted Average", "___",
                        "Fossil Fuels", colnames(employmentBySector)[9:10], "Weighted Average")
colnames(resultsA) <- c("Direct Employment", "Indirect Employment", "Total Employment")
resultsA[2:6,1] <- t(employmentBySector[2,1:5])
resultsA[2:6,2] <- t(employmentBySector[3,1:5])
resultsA[2:6,3] <- t(employmentBySector[1,1:5])
resultsA[7,1] <- weightedAverages[3,1]
resultsA[7,2] <- weightedAverages[2,1]
resultsA[7,3] <- weightedAverages[1,1]
resultsA[10:12,1] <- t(employmentBySector[2,6:8])
resultsA[10:12,2] <- t(employmentBySector[3,6:8])
resultsA[10:12,3] <- t(employmentBySector[1,6:8])
resultsA[13,1] <- weightedAverages[3,2]
resultsA[13,2] <- weightedAverages[2,2]
resultsA[13,3] <- weightedAverages[1,2]
resultsA[16:17,1] <- t(employmentBySector[2,9:10])
resultsA[16:17,2] <- t(employmentBySector[3,9:10])
resultsA[16:17,3] <- t(employmentBySector[1,9:10])
resultsA[18,1] <- weightedAverages[3,3]
resultsA[18,2] <- weightedAverages[2,3]
resultsA[18,3] <- weightedAverages[1,3]

resultsA10 <- round(resultsA, 1)
resultsA10[is.na(resultsA10)] <- " "

## Print table 10
grid.table(resultsA10)

## Replicate table 11
resultsA11 <- matrix(nrow=5, ncol=1)
rownames(resultsA11) <- c("Renewable Energy", "Energy Efficiency", "Clean Energy Total",
                          "Fossil Fuels", "Clean Energy Relative to FFF in excess %")
colnames(resultsA11) <- "Jobs per $ million USD"

resultsA11[1] <- resultsA[7,3]
resultsA11[2] <- resultsA[13,3]
resultsA11[3] <- 0.67 * resultsA11[1] + 0.33 * resultsA11[2]
resultsA11[4] <- resultsA[18,3]
resultsA11[5] <- 100 * resultsA11[3] / resultsA11[4] - 100
resultsA11 <- round(resultsA11, 1)

## Print table 11
grid.table(resultsA11)


############
## PART B ##
############

## Import new weights matrix
weights[1:10,1:35] <- 0
weights[1:10,1:24] <- readWorksheetFromFile("Alternative_Weights.xlsx",
                                 sheet="Sheet1",
                                 region = "D9:AL18",
                                 header=FALSE,
                                 colTypes="numeric")
weights[is.na(weights)] <- 0
categoryWeights <- readWorksheetFromFile("Alternative_Weights.xlsx",
                                                   sheet="Sheet1",
                                                   region = "D23:M25",
                                                   header=FALSE,
                                                   colTypes="numeric")
categoryWeights[is.na(categoryWeights)] <- 0
rownames(categoryWeights) <- c("Renewable Energy", "Energy Efficiency", "Fossil Fuels")
colnames(categoryWeights) <- colnames(employmentBySector)

## Results: employment by sector
employmentBySector <- totalEmployment %*% t(weights)

## Results: weighted averages 
weightedAverages <- employmentBySector %*% t(categoryWeights)

## Replicate Table 10
resultsB <- matrix(nrow=18, ncol=3)
rownames(resultsB) <- c("Renewables", colnames(employmentBySector)[1:5], "Weighted Average", "___",
                        "Energy Efficiency", colnames(employmentBySector)[6:8], "Weighted Average", "___",
                        "Fossil Fuels", colnames(employmentBySector)[9:10], "Weighted Average")
colnames(resultsB) <- colnames(resultsA) <- c("Direct Employment", "Indirect Employment", "Total Employment")
resultsB[2:6,1] <- t(employmentBySector[2,1:5])
resultsB[2:6,2] <- t(employmentBySector[3,1:5])
resultsB[2:6,3] <- t(employmentBySector[1,1:5])
resultsB[7,1] <- weightedAverages[3,1]
resultsB[7,2] <- weightedAverages[2,1]
resultsB[7,3] <- weightedAverages[1,1]
resultsB[10:12,1] <- t(employmentBySector[2,6:8])
resultsB[10:12,2] <- t(employmentBySector[3,6:8])
resultsB[10:12,3] <- t(employmentBySector[1,6:8])
resultsB[13,1] <- weightedAverages[3,2]
resultsB[13,2] <- weightedAverages[2,2]
resultsB[13,3] <- weightedAverages[1,2]
resultsB[16:17,1] <- t(employmentBySector[2,9:10])
resultsB[16:17,2] <- t(employmentBySector[3,9:10])
resultsB[16:17,3] <- t(employmentBySector[1,9:10])
resultsB[18,1] <- weightedAverages[3,3]
resultsB[18,2] <- weightedAverages[2,3]
resultsB[18,3] <- weightedAverages[1,3]

resultsB10 <- round(resultsB, 1)
resultsB10[is.na(resultsB10)] <- " "

## Print table 10
grid.table(resultsB10)

## Replicate table 11
resultsB11 <- matrix(nrow=5, ncol=1)
rownames(resultsB11) <- c("Renewable Energy", "Energy Efficiency", "Clean Energy Total",
                          "Fossil Fuels", "Clean Energy Relative to FFF in excess %")
colnames(resultsB11) <- "Jobs per $ million USD"

resultsB11[1] <- resultsB[7,3]
resultsB11[2] <- resultsB[13,3]
resultsB11[3] <- 0.67 * resultsB11[1] + 0.33 * resultsB11[2]
resultsB11[4] <- resultsB[18,3]
resultsB11[5] <- 100 * resultsB11[3] / resultsB11[4] - 100
resultsB11 <- round(resultsB11, 1)

## Print table 11
grid.table(resultsB11)
