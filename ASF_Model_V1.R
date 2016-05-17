
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

initialAbundance <- 50



# First line of inputs here

###############################################################################

# Triangle Package

#install.packages("triangle")
#library("triangle")

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
# Filling In Respective Columns

# Individual ID
popMatrix[, "id"] <- seq(1:nrow(popMatrix))

# Female
popMatrix[1:initialAdultFemales, "female"] <- 1
popMatrix[initialAdultFemales+1:initialJuvFemales, "female"] <- 1


# Sounder ID

# Location

# Age (Haven't Addressed Sampling Rule)

## Adult Females
# Need to fix... Needs to be from Triangular
popMatrix[1:initialAdultFemales, "age"] <- sample(19:96, initialAdultFemales, replace=TRUE)

## Juvenile Females
popMatrix[initialAdultFemales+1:initialJuvFemales, "age"] <- sample(7:10, initialJuvFemales, replace=TRUE)

## Adult Males

#Creating Male Indicies
startAdultMale <- initialAdultFemales+initialJuvFemales+1
endAdultMale <- startAdultMale+initialAdultMales-1

# Also need to fix (Triangular)
popMatrix[startAdultMale:endAdultMale, "age"] <- sample(19:72, initialAdultMales, replace=TRUE)

startJuvMale <- endAdultMale+1 
endJuvMale <- initialAbundance

## Juvenile Males
popMatrix[startJuvMale:endJuvMale, "age"]<- sample(7:10, initialJuvMales, replace=TRUE)









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
