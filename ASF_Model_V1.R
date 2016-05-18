
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
#     arguments or when indexing with vector or matrix e.g. matrix[1:(x+1), ]
#   - reference columns of any matrix by name rather than index number
#   - wrap lines of code longer than 79 characters (width of the # bars)
###############################################################################



rm(list=ls())

require("triangle")

set.seed(1)




###############################################################################
# Input Section

# Inputs for initial population
initialAbundance <- 1500
initialAdultFemales <- round(.25 * initialAbundance)
initialJuvFemales   <- round(.27 * initialAbundance)
initialAdultMales   <- round(.21 * initialAbundance)
initialJuvMales     <- round(.27 * initialAbundance)

traitList <- c( 'id', 'sounderId', 'location', 'age', 'female', 'mortProb')

###############################################################################











###############################################################################
# Functions

# Creating Initital popMatrix
  popMatrix <- matrix(0, nrow=initialAbundance, ncol=length(traitList))
  colnames(popMatrix) <- traitList

  # Individual ID
  popMatrix[, "id"] <- seq(1:nrow(popMatrix))

  # Female
  popMatrix[1:(initialAdultFemales + initialJuvFemales), 'female'] <- 1

  # Sounder ID

  # Location

  # Age  (Assume 30 Days in a Month)

  # assign age to adult females
  popMatrix[1:initialAdultFemales, "age"] <-
    round(rtriangle(initialAdultFemales, a=(19*30), b=(96*30), c=(19*30)))

  # assign age to juv females
  ageSeq <- c(rep(sample((30*7):(30*10), 
                            initialJuvFemales%/%3, replace=TRUE), 3),
                 rep(sample((30*7):(30*10), 1), initialJuvFemales%%3))
  popMatrix[(initialAdultFemales+1):(initialAdultFemales+initialJuvFemales),
            "age"] <- ageSeq
  
  # assign age to adult males
  popMatrix[(initialAdultFemales+initialJuvFemales+1):
            (initialAbundance-initialJuvMales), "age"] <- 
    round(rtriangle(initialAdultMales, a=(19*30), b=(72*30), c=(19*30)))
  

  # assign ages to juvenile males
  ageSeq <- c(rep(sample((30*7):(30*10), 
                             initialJuvMales%/%3, replace=TRUE), 3),
                  rep(sample((30*7):(30*10), 1), initialJuvMales%%3))
  popMatrix[(initialAbundance-initialJuvMales+1):initialAbundance, 
            "age"]<- ageSeq  # Aaron: I changed the 3rd line of code here
                             # for males you or I had: initialJuvFemales%%3, 
                             # I believe it needs to be initialJuvMales%%3



## Starting Group Structure 

## Creating Unique Solo-Male Sounders (Age > 18 Months)

soloMales <- sum(popMatrix[ , "female"]==0 & popMatrix[ , "age"] > (18*30))
popMatrix[popMatrix[ , "female"]==0 & popMatrix[ , "age"] > (18*30), "sounderId"] <- seq(1, soloMales)

## (Starting) Creating Female and Piglet Sounders 

# Necesscary objects for loop
idFill     <- soloMales + 1
numPiglets <- 15
juvFemRow  <- initialAdultFemales + 1
juvMaleRow <- initialAdultFemales + initialJuvFemales + initialAdultMales + 1 
j = numPiglets
i = 0

# Starting While Loop
    while (j > 1) {
      popMatrix[, "sounderId"][juvFemRow+i] <- idFill
      popMatrix[, "sounderId"][juvMaleRow+i] <- idFill
      j <- j - 2
      i <- i + 1
    } # Close While Loop
    
    if (j==1) {
      draw <- runif(1)
        if (draw > 0.5) {
          popMatrix[, "sounderId"][juvFemRow+i] <- idFill
          juvFemRow = juvFemRow + 1
             } # Close second if statement 
            else {
              popMatrix[, "sounderId"][juvMaleRow+i] <- idFill
              juvMaleRow = juvMaleRow + 1 
            } # Close else statment 
    } # Close first if statement  
    


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
