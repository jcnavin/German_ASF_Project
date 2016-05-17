
###############################################################################
# Model for ASF in German wild boar
#
# Formatting notes:
#   - use camel case, no periods or underscores when naming objects
#   - all objects begin with lower case letter, except use uppercase for first
#     letter of function names
#   - use descriptive names for objects
#     e.g. 'mortalityProb' rather than 'mp'
#   - indent should be two spaces
#   - use spaces around mathematical operators, except when specifying function
#     arguments
#   - reference columns of any matrix by name rather than index number
#   - wrap lines of code longer than 79 characters (width of the # bars)
###############################################################################



rm(list=ls())
set.seed(1)



###############################################################################
# Input Section

initialAbundance <- 100



# First line of inputs here

###############################################################################

# Triangle Package

install.packages("triangle")
library("triangle")

# Helper Functions--Jordan

traitList <- c( "id", "sounderId", "location", "age", "female", "mortProb")

initialAdultFemales <- round(.25 * initialAbundance)
initialJuvFemales <- round(.27 * initialAbundance)
initialAdultMales <- round(.21 * initialAbundance)
initialJuvMales <- round(.27 * initialAbundance)

# Checking Total Abundance Consistency 
initialAdultFemales + initialJuvFemales + initialAdultMales + initialJuvMales



###############################################################################
# Functions

# Creating Initital popMatrix

popMatrix <- matrix(0, nrow=initialAbundance, ncol=length(traitList))
colnames(popMatrix) <- traitList

################################################################################
# Filling In Respective popMatrix Columns

# Individual ID
popMatrix[, "id"] <- seq(1:nrow(popMatrix))

# Female
popMatrix[1:initialAdultFemales, "female"] <- 1
popMatrix[initialAdultFemales+1:initialJuvFemales, "female"] <- 1

# Sounder ID

# Location

# Age  (Assume 30 Days in a Month)

## Adult Females
# Sampling from Triangular Distribution
popMatrix[1:initialAdultFemales, "age"] <- 
  round(rtriangle(initialAdultFemales, a=(19*30), b=(96*30), c=(19*30)))

## Juvenile Females
innerFemVecLength <- initialJuvFemales %/% 3
remainder <- initialJuvFemales %% 3
femAgeSeq <- c(rep(sample((30*7):(30*10), innerFemVecLength, replace=TRUE), 3),
            rep(sample((30*7):(30*10), 1), remainder))
length(femAgeSeq)
popMatrix[initialAdultFemales+1:initialJuvFemales, "age"] <- femAgeSeq

## Adult Males

# Creating Male Indicies
startAdultMale <- initialAdultFemales+initialJuvFemales+1
endAdultMale <- startAdultMale+initialAdultMales-1

# Sampling from Triangluar Distribution
popMatrix[startAdultMale:endAdultMale, "age"] <- 
  round(rtriangle(initialAdultMales, a=(19*30), b=(72*30), c=(19*30)))
startJuvMale <- endAdultMale+1 
endJuvMale <- initialAbundance

## Juvenile Males

innerMaleVecLength <- initialJuvMales %/% 3
remainder <- initialJuvFemales %% 3
maleAgeSeq <- c(rep(sample((30*7):(30*10), innerFemVecLength, replace=TRUE), 3),
               rep(sample((30*7):(30*10), 1), remainder))
length(maleAgeSeq)
popMatrix[startJuvMale:endJuvMale, "age"]<- maleAgeSeq



## Starting Group Structure 

## Creating Unique Solo-Male Sounders (Age > 18 Months)

soloMales <- sum(popMatrix[ , "female"]==0 & popMatrix[ , "age"] > (18*30))
popMatrix[popMatrix[ , "female"]==0 & popMatrix[ , "age"] > (18*30), "sounderId"] <- seq(1, soloMales)



#InitialPopulation <- function() {
#  # this functions returns an initial popMatrix
#
#
#  return(popMatrix)
#}

###############################################################################



###############################################################################
# Loops

#popMatrix <- InitialPopulation()

#for(d in 1:365) {
#  popMatrix <- Mortality()
#  popMatrix <- Reproduction()
#}


###############################################################################
