
###############################################################################
# Model for ASF in German wild boar
#
# Formatting notes:
#   - use '<-' for object assignment rather than '='
#   - explanatory comments should be all lower case
#   - use single quotes rather than double quotes
#   - use camel case, no periods or underscores when naming objects
#   - all objects begin with lower case letter, except use uppercase for first
#     letter of function names
#   - use descriptive names for objects
#     e.g. 'mortalityProb' rather than 'mp'
#   - indent should be two spaces
#   - use spaces around mathematical operators, except when specifying function
#     arguments
#   - reference columns of any matrix by name rather than index number
#   - always use a space after commas
#   - wrap lines of code longer than 79 characters (width of the # bars)
###############################################################################



rm(list=ls())

#install.packages('triangle')
require('triangle')

set.seed(1)




###############################################################################
# Input Section

# inputs for initial population
initialAbundance <- 200
initialAdultFemales <- round(.25 * initialAbundance)
initialJuvFemales   <- round(.24 * initialAbundance)
initialAdultMales   <- round(.21 * initialAbundance)
initialJuvMales     <- initialAbundance - 
                       sum(initialAdultFemales + initialJuvFemales + 
                           initialAdultMales)
initPigletsPerSounder <- 15

# inputs for sounders
maxFemalesPerSounder <- 7

traitList <- c( 'id', 'sounderId', 'location', 'age', 'female', 'mortProb')

###############################################################################











###############################################################################
# Functions

  # create empty matrix
  popMatrix <- matrix(0, nrow=initialAbundance, ncol=length(traitList))
  colnames(popMatrix) <- traitList

  # assign id numbers
  popMatrix[, 'id'] <- seq(1:nrow(popMatrix))

  # assign sex
  popMatrix[1:(initialAdultFemales + initialJuvFemales), 'female'] <- 1

  # assign age to adult females
  popMatrix[1:initialAdultFemales, 'age'] <-
    round(rtriangle(initialAdultFemales, a=(19*30), b=(96*30), c=(19*30)))

  # assign age to juv females
  ageSeq <- c(rep(sample((30*7):(30*10), 
                         initialJuvFemales%/%3, replace=TRUE), 3),
              rep(sample((30*7):(30*10), 1), initialJuvFemales%%3))
  popMatrix[(initialAdultFemales+1):
            (initialAdultFemales+initialJuvFemales),'age'] <- ageSeq
  
  # assign age to adult males
  popMatrix[(initialAdultFemales+initialJuvFemales + 1):
            (initialAbundance-initialJuvMales), 'age'] <- 
    round(rtriangle(initialAdultMales, a=(19*30), b=(72*30), c=(19*30)))
  
  # assign ages to juv males
  ageSeq <- c(rep(sample((30*7):(30*10), 
                         initialJuvMales%/%3, replace=TRUE), 3),
              rep(sample((30*7):(30*10), 1), initialJuvMales%%3))
  popMatrix[(initialAbundance-initialJuvMales+1):
            initialAbundance, 'age']<- ageSeq  

  # assign sounder id's to adult solo males
  soloMales <- sum(popMatrix[, 'female'] == 0 & 
                   popMatrix[, 'age'] > (18*30))
  popMatrix[popMatrix[, 'female'] == 0 & 
            popMatrix[, 'age'] > (18*30), 'sounderId'] <- seq(1, soloMales)

  # assign sounder id's to adult females
  # JORDAN: I replaced your loop with this. Sorry!
  for(i in 1:initialAdultFemales) {
    popMatrix[i, 'sounderId'] <- soloMales + (i-1) %/% maxFemalesPerSounder
  }

  # assign sounder ids' to piglets
  idFill         <- soloMales + 1
  juvFemRow      <- initialAdultFemales + 1
  juvMaleRow     <- initialAdultFemales + initialJuvFemales + initialAdultMales + 1 
  juvFemEndRow   <- initialAdultFemales + initialJuvFemales
  juvMaleEndRow  <- initialAbundance
  spotsRemaining <- initPigletsPerSounder
  m <- juvMaleRow  
  f <- juvFemRow
  outOfMalePiglets   <- 0
  outOfFemalePiglets <- 0
  stop <- 0


# Starting "outer" while loop

  while (outOfMalePiglets + outOfFemalePiglets != 2) {
    while (stop == 0 & spotsRemaining > 0) {
      if (spotsRemaining == 1 & 
          (outOfMalePiglets + outOfFemalePiglets == 0)) {
        stop <- 1
        draw <- runif(1)
        if (draw > 0.5) {
          popMatrix[, 'sounderId'][f]  <- idFill
          f <- min(f + 1, juvFemEndRow)
        } else {
          popMatrix[, 'sounderId'][m] <- idFill
          m <- min(m + 1, juvMaleEndRow)
        }
      } else { 
        popMatrix[, 'sounderId'][f] <- idFill
        popMatrix[, 'sounderId'][m] <- idFill
        spotsRemaining <- initPigletsPerSounder - 
                          sum(popMatrix[, 'sounderId'] == idFill &
                               popMatrix[, 'age'] < (10 * 30))
        if (m == juvMaleEndRow) {
          outOfMalePiglets <- 1
        }
        if (f == juvFemEndRow) {
          outOfFemalePiglets <- 1
        }
        if (outOfMalePiglets + outOfFemalePiglets == 2) {
          stop <- 1
        }
        m <- min(m + 1, juvMaleEndRow)
        f <- min(f + 1, juvFemEndRow)
      }
    }  # close inner while loop
    spotsRemaining <- initPigletsPerSounder 
    idFill <- idFill + 1
    stop <- 0
  }  # close outer while loop

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
