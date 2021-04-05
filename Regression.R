# GOJO CRUZ, JAMLECH IRAM N.
# PROJECT - CMSC150, UPLB

#============================= POLYNOMIAL REGRESSION ===========================#
ProductSummation <- function(vecX, vecY, degree){                     #solves for the RHS values
  sum = 0
  i=1
  while(i <= length(vecX)){
    sum = sum + (((vecX[i])^degree)*vecY[i])
    i = i+1
  }
  return(sum)
}

Summation <- function(vec, degree){                                  #solves the values for the matrix
  sum = 0
  for(x in vec){
    sum = sum + (x^degree)
  }
  return(sum)
}

Mean <- function(vec){                                               #solves for the mean of x or y vectors
  mean = Summation(vec, 1)/length(vec)
  return(mean)
}

RegressionAugMat <- function(vector_of_X, vector_of_Y, degree){      #setup the augmented matrix
  temp = c()
  z=1; y=1
  while(y <= degree+1){                                              #creates a zero vector
    while(z <= degree+1){
      temp = c(temp, 0)
      z = z+1
    }
    y = y+1
  }
  
  m = matrix(temp, nrow = (degree+1), ncol = (degree+1), byrow=TRUE) #create matrix
  row=1
  
  while(row <= (degree+1)){
    tempDeg = row-1
    col=1
    while(col <= (degree+1)){
      m[row, col] = Summation(vector_of_X, tempDeg)                  #insert the values on their corresponding row and column
      col = col+1
      tempDeg = tempDeg+1
    }
    row = row+1
  }
  
  
  rhs = c()
  k=1
  tempDeg = 0
  while(k <= (degree+1)){
    rhs = c(rhs, ProductSummation(vector_of_X, vector_of_Y, tempDeg)) #get the right hand side
    k = k+1
    tempDeg = tempDeg+1
  }
  
  augmat = cbind(m, rhs)                                            #bind the rhs to the created matrix
  
  return(augmat)
}

SetUp <- function(coeffs, degree){                                  #function in setting the expression
  i = 1; variables = c()
  exponents = c(as.character(0:degree))
  coeffs = c(as.character(coeffs))
  for(i in exponents) variables = c(variables, paste("x", i, sep="^"))
  
  expression = paste(coeffs, variables, sep="*", collapse=" + ")
  fxn = paste("function (x)", expression, sep=" ")
  
  return(fxn)
}


PolynomialRegression <- function(vector_of_X, vector_of_Y, degree, x){ #plots the points and the polynomial function
  if(degree < 1){
    print("Degree is invalid.")
    return(NA)
  }
  
  if(length(vector_of_X) != length(vector_of_Y)){
    print("Number of elements in x is not equal in y.")
    return(NA)
  }
  
  augmat = RegressionAugMat(vector_of_X, vector_of_Y, degree)
  print(augmat)
  result = GaussJordan(augmat, FALSE)
  mat_iter = result$mat_iterations
  coeffs = result$xs
  exps = SetUp(coeffs, degree)
  fxn = eval(parse(text=exps))
  cat("\n")
  print(exps)
  
  plot(vector_of_X, vector_of_Y, type="p", pch=20, col="red", main = "Regression", xlab = "X's", ylab = "Y's")
  curve(fxn, col="blue", add=TRUE)
  
  
  options(digits = 4)
  regression <- list(coefficients = coeffs, function1 = fxn, estimate = fxn(x), fxn_string = exps, mat_iter = mat_iter)
  return(regression)
  
  
}

Checker <- function(result, xvalue){ #function in evaluating the polynomial function given the value of x
  final = result$function1
  final = final(xvalue)
  print("Given the value of x, the result is:")
  print(final)
}

#========================= GAUSS-JORDAN =========================#

#PIVOTING FUNCTION
Pivoting <- function(mat, pivotRow, matCol){
  row = pivotRow
  col = pivotRow
  vec = c()
  
  if(row == 1){
    vec = c(vec,mat[,col])
  }
  else{
    vec = c(vec,mat[-(1:col-1), col])
  }
  
  while(row <= nrow(mat)){
    if(abs(mat[row,col]) == max(abs(vec))){
      temp = mat[row, ]
      mat[row, ] = mat[col, ]
      mat[col, ] = temp
    }
    row = row+1
  } 
  return (mat)
}

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
GaussJordan <- function(f, verbose){
  augmatrix = f  #get the augmented coefficient matrix
  j=1; lenRow = nrow(augmatrix); lenCol = ncol(augmatrix)
  pivotRow = 1;
  mat_iterations = list()
  mat_iterations[[j]] = augmatrix
  
  while(j <= lenCol-1){
    augmatrix = Pivoting(augmatrix, pivotRow, j) #apply pivoting 
    augmatrix = Normalize(augmatrix, j, j) #normalize the pizot row
    
    i=1
    while(i <= lenRow){
      
      if(i!=j){ #if the row is not equal to the column number, then it is not in the main diagonal
        multiplier = augmatrix[i,j]  #multiplier will be just the element you want to be zero
        
        k=1
        tempvector = c()
        while(k <= lenCol){
          augmatrix[i,k] = augmatrix[i,k] - multiplier * augmatrix[j,k]
          tempvector = c(tempvector, multiplier * augmatrix[j,k])
          k = k+1
        }
      }
      i = i+1
    }
    j = j+1
    mat_iterations[[j]] = augmatrix
    pivotRow = pivotRow+1
  }
  
  xs = augmatrix[, lenCol]  #unknown values would be the remaining values after elimination, in the right hand side
  forelem = augmatrix

  gaussjordanResult <- list(forelem = forelem, xs = xs, mat_iterations = mat_iterations) #creates a list of the matrix and the unknown values
  return (gaussjordanResult) #return result
  
}


