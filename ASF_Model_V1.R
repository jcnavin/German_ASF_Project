
# Test Comment for Forking

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

# Inputs for Cell Structure (HoneyComb) Function ##############################

R <- 1 #(Number of rings around center)

HoneyComb <- function(){
  # Takes argument R, returns 3 items: cellInfo, cellDet, & cellMatch
  
# cellInfo ####################################################################
  cellInfo <- matrix(0, nrow=(R+1), ncol=2)
  colnames(cellInfo) <- c("numCellsRing", "cellCount")
  cellInfo[1, ] <- 1
  for (i in 2:(R+1)){
    Ni <- 6 * (i-1)
    cellInfo[i, 1] <- Ni
    cellInfo[i, 2] <- Ni+(cellInfo[(i-1), 2])
  }
  
  N = cellInfo[(R+1), 2]
# cellDet #####################################################################
  cellDet <- matrix(0, nrow=N, 5)
  colnames(cellDet) <- c("cellNum", "ringNum", 
                         "inRingPosition", "relativeSidePos"
                         , "inSidePos")
  # First Col = cellNum
  cellNum <- seq(from = 1, to = N)
  cellDet[, 1] <- cellNum
  # Start Row Counter
  rowCount <- 1
  for (j in 1:(1+R)){
    for (i in 1:(cellInfo[j, 1])){
      cellDet[rowCount, 2]<- j-1 #2ndColumn is ring number
      cellDet[rowCount, 3]<- i   #3rdColumn is in-ring position
      rowCount <- rowCount + 1
    }
  }
  # center cell doesn't have a side
  cellDet[1,4] <- 0
  # center cell doesn't have an in-ring position
  cellDet[1,3] <- 0
  # Start over rowCount
  rowCount <-2
  for (j in 1:R){
    for (s in 1:6){
      for (t in 1:j){
        cellDet[rowCount, 4] <- s
        rowCount <- rowCount+1
      }
    }
  }
  # Column 5 is cell position on "side" 
  cellDet[1, 5]  <- 1
  cellDet[2, 5]  <- 1
  for (i in 3:N){
    if ((cellDet[i, 4])==(cellDet[(i-1), 4])){
      cellDet[i, 5] <- (cellDet[(i-1), 5])+1
    }
    else{
      cellDet[i, 5] <- 1
    }
  }
# cellMatch ###################################################################
  Z <- N * 6
  cellMatch <- matrix(0, nrow = Z, ncol = 4)
  colnames(cellMatch) <- c("baseCellNum", "baseCellZone",
                           "neighborCell", "neighborZone")
  # start row count @ 1
  r <- 1
  # add base cells
  for (n in 1:N){
    for (z in 1:6){
      cellMatch[r, 1] <- n
      cellMatch[r, 2] <- z
      r <- r+1
    }
  }
  # add zone match
  for (i in 1:Z){
    if ((cellMatch[i, 2]) <= 3){
      cellMatch[i, 4] <- (cellMatch[i, 2])+3
    }
    else{
      cellMatch[i, 4] <- (cellMatch[i, 2])-3
    }
  }
  # add cell match
  i <- 1
  for (i in 1:Z){
    # cell num
    n <- cellMatch[i, 1]
    # cell zone
    z <- cellMatch[i, 2]
    # cell ring
    r <- cellDet[n, 2]
    # cell side
    s <- cellDet[n, 4]
    # cell in-side position
    p <- cellDet[n, 5]
    # cell in-ring position
    pR <- cellDet[n, 3]
    # center cell search to correpsonding side of first ring
    if (r == 0){
      # first ring has same number as zone of (center+1)
      cellMatch[i, 3] <- z+1
    }
    # first ring
    else if(r==1){
      # interiors look to center cell
      if (z==4 & s==1){
        cellMatch[i, 3] <- 1
      }
      else if (z==5 & s==2){
        cellMatch[i, 3] <- 1
      }
      else if (z==6 && s==3){
        cellMatch[i, 3] <- 1
      }
      else if (z==1 && s==4){
        cellMatch[i, 3] <- 1
      }
      else if (z==2 && s==5){
        cellMatch[i, 3] <- 1
      }
      else if (z==3 && s==6){
        cellMatch[i, 3] <- 1
      }
    }
    # outer ring
    else if (r==R){
      if (z==s|z==(s+1)){
        cellMatch[i, 3] <- -1
      }
    }
    if (s==6 && (z==6 | z==1) ){
      cellMatch[i, 3] <- -1
    }
    if (s==6 && z==5 && p==1){
      cellMatch[i, 3] <- -1
    }
    if (p==1){
      if(s==1 && z==6){
        cellMatch[i, 3] <- -1
      }
      else if(z==(s-1)){
        cellMatch[i, 3] <- -1
      }
    }
    if (pR==1){
      if (z==4){
        cellMatch[i, 3] <- n-(cellInfo[r, 1])
      }
      else if (z==5){
        cellMatch[i, 3] <- cellInfo[(r+1), 2]
      }
      else if (z==6 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2]
      }
    }
    # side 1
    if (s==1){
      if (z==3){
        cellMatch[i, 3] <- n+1
      }
      else if (z==2 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2]+1+pR
      }
      else if (z==1 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2]+p
      }
      else if (p>1 && z==6){
        cellMatch[i, 3] <- (n-1)
      }
      else if (z==4 && r > 1){
        cellMatch[i, 3] <- cellInfo[(r-1), 2] + p
      }
      else if (p > 1 && z==5 && r > 1){
        cellMatch[i, 3] <- cellInfo[(r-1), 2]+ p - 1
      }
      else if (z == 6 && r < R){
        cellMatch[i, 3] = cellInfo[(r+1), 2] + 5*(r+1) + p + 1
      }
    }
    # side 2
    else if (s==2){
      if (z==4){
        cellMatch[i, 3] <- n+1
      }
      else if (z==2 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2]+r+1+p
      }
      else if (z==6 && p==1){
        cellMatch[i, 3] <- n-1
      }
      else if (p==1 && z==1 & r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2]+r+1
      }
      else if (z==3 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2]+2+r+p
      }
      else if (p < r && z==5){
        cellMatch[i, 3] <- cellInfo[(r-1), 2]+(r-1)+p
      }
      else if (p > 1 && z == 1){
        cellMatch[i, 3] <- n-1
      }
      else if (p==r && z==5 && r > 1){
        cellMatch[i, 3] <- cellInfo[(r-1), 2]+1+2*(r-1)
      }
    }
    # side 3
    else if (s==3){
      if (z==5){
        cellMatch[i, 3] <- n+1
      }
      else if (z==2 && p > 1){
        cellMatch[i, 3] <- n-1
      }
      else if (z==1 && p==1){
        cellMatch[i, 3] <- n-1
      }
      else if (z==3 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2] + p + 2*(r+1)
      }
      else if (p==1 && z==2 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2]+2*r + 2
      }
      else if (z==4 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2]+2*(r+1)+p+1
      }
      else if (p < r && z==6){
        cellMatch[i, 3] <- cellInfo[(r-1), 2]+2*(r-1)+p
      }
      else if(p==r && z==6 && r > 1){
        cellMatch[i, 3] <- cellInfo[(r-1), 2]+2*(r-1)+p
      }
    }
    # side 4
    else if (s==4){
      if (z==6){
        cellMatch[i, 3] <- n+1
      }
      else if (z==2 && p==1){
        cellMatch[i, 3] <- n-1
      }
      else if (z == 3 && p == 1 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2] + 3*(r) + p + 2
      }
      else if (z == 5 && r < R) {
        cellMatch[i, 3] <- cellInfo[(r+1), 2] + 1 + 3*(r+1) + p
      }
      else if (z == 1 && r > 1){
        cellMatch[i, 3] <- cellInfo[(r-1), 2] + 3*(r-1) + p
      }
      else if (z == 3 && p > 1){
        cellMatch[i, 3] <- n-1
      }
      else if (z == 4 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2] + 3*(r+1) + p
      }
      else if (z == 2 && p > 1 && r > 1){
        cellMatch[i, 3] <- cellInfo[(r-1), 2] + 3*(r-1) + p-1
      }
    }
    # side 5
    else if (s==5){
      if (z == 1){
        cellMatch[i, 3] <- n + 1
      }
      else if (z == 1){ 
        cellMatch[i, 3] <- n + 1
      }
      else if (z == 3 & p == 1){
        cellMatch[i, 3] <- n - 1
      }
      else if (z == 4 && p == 1 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2] + 4*(r+1)
      }
      else if (z == 5 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2] + 4*(r+1) + p
      }
      else if (z == 6 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2] + 4*(r+1) + p + 1
      }
      else if (z == 2 && r > 1){
        cellMatch[i,3] <- cellInfo[(r-1), 2] + 4*(r-1) + p
      }
      else if (z == 4 && p > 1 && r > 1){
        cellMatch[i, 3] <- n - 1
      }
    }
    # side 6
    else if (s == 6){
      if (z == 4 & p==1){
        cellMatch[i, 3] <- n - 1
      }
      else if (z == 2 && pR == cellInfo[(r+1), 1]){
        cellMatch[i, 3] <- (cellInfo[r, 2]) + 1
      }
      else if (z == 1 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2] + 5*(r+1) + p+1
      }
      else if (z == 5 && p == 1 && r < R){
        cellMatch[i, 3] <- cellInfo[(r+1), 2] + 5*(r+1)
      }
      else if (z == 6 && r < R){
        cellMatch[i, 3] = cellInfo[(r+1), 2] + 5*(r+1) + p
      }
      else if (z == 2 && p < r){
        cellMatch[i,3] <- n + 1
      }
      else if (z == 3 && p < r){
        cellMatch[i, 3] <- cellInfo[(r-1), 2] + 5*(r-1) + p
      }
      else if (z == 3 && p == r && r > 1){
        cellMatch[i, 3] <- cellInfo[(r-1), 2] + 1
      }
      else if (z == 5 && p > 1 && r > 1){
        cellMatch[i, 3] <- n-1
      }
    }
    cellMatch[cellMatch[, 3] == -1, 3] <- 0 
  }
  allInfo <- list(cellInfo, cellDet, cellMatch)
  return(allInfo)
} # Close Function
###############################################################################
allInfo <- HoneyComb()
cellMatch <- as.matrix(data.frame(allInfo[3]))
cellInfo <- as.matrix(data.frame(allInfo[1]))
cellDet <- as.matrix(data.frame(allInfo[2]))
###############################################################################
# Input Section

# inputs for init population
initAbundPerCell  <- 50
numberCells       <- max(cellInfo[, "cellCount"])
initAdultFemales  <- round(.25 * initAbundPerCell)
initPigletFemales <- round(.24 * initAbundPerCell)
initAdultMales    <- round(.21 * initAbundPerCell)
initPigletMales   <- initAbundPerCell - 
                     sum(initAdultFemales + initPigletFemales + initAdultMales)
adultFemStart   <- 1
adultFemEnd     <- initAdultFemales
pigletFemStart  <- initAdultFemales + 1
pigletFemEnd    <- initAdultFemales + initPigletFemales
adultMaleStart  <- initAdultFemales + initPigletFemales + 1
adultMaleEnd    <- initAdultFemales + initPigletFemales + initAdultMales
pigletMaleStart <- initAdultFemales + initPigletFemales + initAdultMales + 1
pigletMaleEnd   <- initAbundPerCell

initPigletsPerSounder <- 15

# inputs for sounders
maxFemalesPerSounder <- 3
maxPigletAge <- 10 * 30

traitList <- c( 'id', 'sounderId', 'cell', 'age', 'female')
###############################################################################



###############################################################################
# Functions

InitialPopulation <- function() {
  # This function returns an initial population matrix
  
  # create popMatrix
  popMatrix <- matrix(0, nrow=initAbundPerCell * numberCells, 
                      ncol=length(traitList))
  colnames(popMatrix) <- traitList
  
  idFill <- max(popMatrix[, 'sounderId']) + 1

  for(k in 1:numberCells) {
    # create location specific matrix
    celMatrix <- matrix(0, nrow=initAbundPerCell, ncol=length(traitList))
    colnames(celMatrix) <- traitList
    celMatrix[, 'cell'] <- k

    # assign sex
    celMatrix[adultFemStart:pigletFemEnd, 'female'] <- 1
    
    # assign age to adult females
    celMatrix[adultFemStart:adultFemEnd, 'age'] <-
      round(rtriangle(initAdultFemales, a=(19*30), b=(96*30), c=(19*30)))

    # assign age to piglet females
    ageSeq <- c(rep(sample((30*7):(maxPigletAge), 
                          initPigletFemales%/%3, replace=TRUE), 3),
                rep(sample((30*7):(maxPigletAge), 1), initPigletFemales%%3))
    celMatrix[pigletFemStart:pigletFemEnd,'age'] <- ageSeq
  
    # assign age to adult males
    celMatrix[adultMaleStart:adultMaleEnd, 'age'] <- 
      round(rtriangle(initAdultMales, a=(19*30), b=(72*30), c=(19*30)))
  
    # assign ages to piglet males
    ageSeq <- c(rep(sample((30*7):(maxPigletAge), 
                          initPigletMales%/%3, replace=TRUE), 3),
                rep(sample((30*7):(maxPigletAge), 1), initPigletMales%%3))
    celMatrix[pigletMaleStart:pigletMaleEnd, 'age']<- ageSeq  

    # assign sounder id's to adult females
    # JORDAN: I replaced your loop with this. Sorry!


    # assign sounder id's to piglets
    pigletFemRow     <- initAdultFemales + 1
    pigletMaleRow    <- initAdultFemales + initPigletFemales + initAdultMales + 1 
    pigletFemEndRow  <- initAdultFemales + initPigletFemales
    pigletMaleEndRow <- initAbundPerCell
    spotsRemaining   <- initPigletsPerSounder
    m <- pigletMaleRow  
    f <- pigletFemRow
    outOfMalePiglets   <- 0
    outOfFemalePiglets <- 0
    stop <- 0

    while (outOfMalePiglets + outOfFemalePiglets != 2) {
      while (stop == 0 & spotsRemaining > 0) {
        if (spotsRemaining == 1 & 
            (outOfMalePiglets + outOfFemalePiglets == 0)) {
          stop <- 1
          draw <- runif(1)
          if (draw > 0.5) {
            celMatrix[, 'sounderId'][f]  <- idFill
            f <- min(f + 1, pigletFemEndRow)
            if (f == pigletFemEndRow) {
              outOfFemalePiglets <- 1
            }
          } else {
            celMatrix[, 'sounderId'][m] <- idFill
            m <- min(m + 1, pigletMaleEndRow)
            if (m == pigletMaleEndRow) {
              outOfMalePiglets <- 1
            }
          }
        } else {
          if(outOfFemalePiglets == 0) {
            celMatrix[, 'sounderId'][f] <- idFill
          }
          if(outOfMalePiglets == 0) {
            celMatrix[, 'sounderId'][m] <- idFill
          }
          spotsRemaining <- initPigletsPerSounder - 
                            sum(celMatrix[, 'sounderId'] == idFill &
                                celMatrix[, 'age'] <= (10 * 30))
          if (m == pigletMaleEndRow) {
            outOfMalePiglets <- 1
          }
          if (f == pigletFemEndRow) {
            outOfFemalePiglets <- 1
          }
          if (outOfMalePiglets + outOfFemalePiglets == 2) {
            stop <- 1
          }
          m <- min(m + 1, pigletMaleEndRow)
          f <- min(f + 1, pigletFemEndRow)
        } # close else
      }  # close inner while loop
      spotsRemaining <- initPigletsPerSounder 
      idFill <- idFill + 1
      stop <- 0
    }  # close outer while loop
    
    firstNoPigletSounder <- max(celMatrix[, 'sounderId']) + 1
    
    
    # assign sounder id's to adult females
    idFill <- max(popMatrix[, 'sounderId']) + 1
    i <- adultFemStart
    while(i <= adultFemEnd) {
      femalesLeft <- adultFemEnd - i + 1
      if(idFill >= firstNoPigletSounder) {
        allocate <- min(sample(seq(4, 10), 1), femalesLeft)
        celMatrix[i:(i+allocate-1), 'sounderId'] <- idFill
        i <- i + allocate
        idFill <- idFill + 1
      } else {
        allocate <- min(3, femalesLeft)
        celMatrix[i:(i+allocate-1), 'sounderId'] <- idFill
        i <- i + allocate
        idFill <- idFill + 1
      }
    }  
  
    
    # assign sounder id's to adult solo males
    idFill <- 1 + max(celMatrix[, 'sounderId'])
    soloMales <- sum(celMatrix[, 'female'] == 0 & 
                       celMatrix[, 'age'] > (18*30))
    celMatrix[celMatrix[, 'female'] == 0 & 
                celMatrix[, 'age'] > (18*30), 'sounderId'] <- 
      seq(idFill, (idFill + soloMales - 1))
    idFill <- idFill + soloMales
    
    # put celMatrix into its place in popMatrix
    popMatrix[(initAbundPerCell * (k - 1) + 1):
              (initAbundPerCell * k), ] <- celMatrix
  }  # close for loop
  popMatrix[, 'id'] <- seq(1:nrow(popMatrix))
 
  return(popMatrix)
}

# Sounder Census Function ##################################################### 

SounderCensus <- function(){
  # Captures initial spatial distribution of individual sounders
  idEnd <- max(popMatrix[, "sounderId"])
  idStart <- min(popMatrix[, "sounderId"])
  sounderPop <- matrix(0, nrow = idEnd, ncol = 4)
  colnames(sounderPop) <- c("sounderId", "individPerSounder", 
                            "cellLocation", "triangLocation")
  
  N <- nrow(popMatrix)
  
  for (i in 1:N) {
    sounderPop[popMatrix[i, 2], 2] <- sounderPop[popMatrix[i, 2], 2] + 1
  }
  idSeq <- seq(from = idStart, to = idEnd)
  sounderPop[, "sounderId"] <- idSeq
  for (i in 1:N){
    sounderPop[popMatrix[i, "sounderId"], "cellLocation"] <- popMatrix[i, "cell"]
  }
  # triangle search
  tri <- c(1, 3, 5, 2, 6, 4)
  triSeq <- rep(tri, length.out = nrow(sounderPop))
  sounderPop[, "triangLocation"] <- triSeq
  
  return(sounderPop)
} 

##Triangle/Cell Specific Census Function#######################################

TriangleCensus <- function(){ 
  # Returns relative population densities of neighboring cell/triangle
  Z <- numberCells*6
  triangCount <- matrix(0, nrow=Z, ncol=6)
  colnames(triangCount) <- c("cell", "triangle", "count", "neighborCell", 
                             "neighborTriang", "neighborCount")
  N <- nrow(sounderPop)
  cellSeq <- seq(from = 1, to = numberCells)
  cellFill <- rep(cellSeq, each = 6)
  triangCount[, "cell"] <- cellFill
  tri <- c(1, 2, 3, 4, 5, 6)
  triRep <- rep(tri, length.out = Z )
  triangCount[, "triangle"] <- triRep
  for (i in 1:N){
   ii <- 6*(sounderPop[i, "cellLocation"]-1) + sounderPop[i, "triangLocation" ]
  triangCount[ii, "count"] <- 
    triangCount[ii, "count"] + sounderPop[i, "individPerSounder"]
  }
  triangCount[, "neighborCell"] <- cellMatch[, "neighborCell"]
  triangCount[, "neighborTriang"] <- cellMatch[, "neighborZone"]
  
  N <- nrow(triangCount)
  
  for (i in 1:N){
    ii <- 6*(triangCount[i, "neighborCell"]-1) + triangCount[i, "neighborTriang" ]
    if(ii > 0 ){
    triangCount[i, "neighborCount"] <- triangCount[ii, "count"]
    }
  }
  return(triangCount)
}
###############################################################################
###############################################################################
# Loops

popMatrix <- InitialPopulation()
popMatrix

sounderPop <- SounderCensus()
sounderPop

triangCount <- TriangleCensus()
triangCount

#for(d in 1:365) {
#  popMatrix <- Mortality()
#  popMatrix <- Reproduction()
#}


###############################################################################