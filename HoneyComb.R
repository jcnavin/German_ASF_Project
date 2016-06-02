rm(list = ls())

#Inputs####################################################

R <- 3  #(Number of rings around center)

HoneyComb <- function(){

#cellInfo##################################################
cellInfo <- matrix(0, nrow=(R+1), ncol=2)

colnames(cellInfo) <- c("numCellsRing", "cellCount")

cellInfo[1, ] <- 1


for (i in 2:(R+1)){
  
  Ni <- 6 * (i-1)
  cellInfo[i, 1] <- Ni
  cellInfo[i, 2] <- Ni+(cellInfo[(i-1), 2])

}

N = cellInfo[(R+1), 2]

#cellDet###################################################
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

# Center cell doesn't have a side
cellDet[1,4] <- 0

# Center cell doesn't have an in-ring position

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

#cellMatch#################################################

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

## Everything Right to this Point

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
## Right  
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

## Right  
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
   
   else if (z == 1){ ## possibly wrong?
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
return(cellMatch)
} # Close Function
###########################################################
cellMatch <- HoneyComb()
cellMatch