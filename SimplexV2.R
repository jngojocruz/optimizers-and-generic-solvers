# GOJO CRUZ, JAMLECH IRAM N.
# PROJECT - CMSC150, UPLB

#============================= SIMPLEX METHOD ===========================#
# ----------------------------- Dual Simplex --------------------------- #

col_cnt = 25 #number of columns

#INITIALIZATION OF TABLEAU
CreateTableu <- function(eq1, eq2, eq3, eq4, eq5, eq6, eq7, eq8, eq9){
  
  tableau = matrix(c(eq1, eq2, eq3, eq4, eq5, eq6, eq7, eq8, eq9), nrow=9, ncol=col_cnt, byrow=TRUE, 
                   dimnames=list(c(1:9), c("Ax", "Ay", "Az", "Bx", "By", "Bz", "Cx", "Cy", "Cz", "Dx", "Dy", "Dz", "Ex", "Ey", "Ez",
                                   "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "Z", "Solution")))
 return(tableau)
}

#COMPUTES TEST RATIO
ComputeTestRatio <- function(tableau){
  minValue = 0
  minIndex = 0
  i = 1
  while(i <= nrow(tableau)){ #find the minimum from the solution column (most likely a negative value)
    if(tableau[i, ncol(tableau)] < minValue){
      minValue = tableau[i, ncol(tableau)]
      minIndex = i
    }
    i = i+1
  }
  
  ratio = c()
  j = 1
  while(j <= ncol(tableau)-1){ #compute for the quotient of the z row and the pivot row
    if(tableau[minIndex, j] != 0 || tableau[minIndex, j] < 0){ #excluding the positive numbers and 0 values to be the dividend
      quotient = tableau[nrow(tableau), j] / tableau[minIndex, j]
    }
    else{
      quotient = 99999 #represents a very large number
    }
    ratio = c(ratio, quotient)
    j = j+1
  }
  
  pivotElement = min(which(ratio==min(ratio[ratio>0]))) #finds the minimum quotient among the computed values
  pivot = list(pcol=pivotElement, prow=minIndex) #this will determine the pivot row and column
}

#========================= GAUSS-JORDAN =========================#

#NORMALIZATION
Normalize <- function(mat, pivotRow, pivotCol){
  temp=1
  divisor = mat[pivotRow, pivotCol] #get the divisor
  while(temp <= ncol(mat)){
    mat[pivotRow, temp] = mat[pivotRow, temp] / divisor #divide the divisor/element in the diagonal to the pivot row
    temp = temp+1
  }
  
  return (mat)
}


#GAUSS-JORDAN                                                        
SimplexMethod <- function(f, verbose){
  augmatrix = f  #get the augmented coefficient matrix
  j=1; lenRow = nrow(augmatrix); lenCol = ncol(augmatrix); cnt=1
  mat_iterations = list()
  mat_iterations[[cnt]] = augmatrix
  
  if(verbose){
    print("INITIAL TABLEAU:", quote = FALSE)
    print(augmatrix)
  }
  
  while(sum(augmatrix[ ,lenCol] < 0)){
    tr = ComputeTestRatio(augmatrix)
    pivotRow = tr$prow
    pivotCol = tr$pcol
    
    augmatrix = Normalize(augmatrix, pivotRow, pivotCol) #normalize the pizot row
    
    i=1
    while(i <= lenRow){
      if(i!=pivotRow){ #if the current row is not the pivot row
        
        k=1
        tempvector = c()
        while(k <= lenCol){
          if(k!=pivotCol){
            multiplier = augmatrix[pivotRow,k] #value in the pivot row, same column 
            augmatrix[i,k] = augmatrix[i,k] - multiplier * augmatrix[i,pivotCol] #oldValue - (element in same col but in pivot row * same row but pivot col)
          }
          k = k+1
        }
        
        #for the pivot column; this is done to get rid of multiplication by 0
        multiplier = augmatrix[pivotRow,pivotCol]
        augmatrix[i,pivotCol] = augmatrix[i,pivotCol] - multiplier * augmatrix[i,pivotCol]
        
      }
      i = i+1
    }
    if(verbose){
      print(paste("ITERATION",cnt, sep = " "), quote = FALSE)
      print(augmatrix)
    }
    cnt = cnt+1
    mat_iterations[[cnt]] = augmatrix
  }
  
  xs = augmatrix[, lenCol] #unknown values would be the remaining values after elimination, in the right hand side
  forelem = augmatrix

  fvalue = augmatrix[lenRow,lenCol]
  gaussjordanResult <- list(forelem = forelem, xs = xs, fvalue = fvalue, mat_iterations = mat_iterations) #creates a list of the matrix and the unknown values
  return (gaussjordanResult) #return result
  
}