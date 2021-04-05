# GOJO CRUZ, JAMLECH IRAM N.
# PROJECT - CMSC150, UPLB

# ========================= QUADRATIC SPLINE INTERPOLATION ========================= #
QSI <- function(mat, x, verbose){
  rowcount=1; colcount=1
  i = 2; j = 1;
  n = nrow(mat); eqs = c()
  augmat = matrix(0, nrow=3*(n-1)-1, ncol=3*(n-1))

  #The function values of adjacent polynomials must be equal at the interior knots.
  while(i<n){
    a = (mat[i,1])^2
    b = mat[i,1]
    c = 1
    rhs = -(mat[i,2])
    sign1 = paste("a",j,sep=""); sign2 = paste("b",j,sep=""); sign3 = paste("c",j,sep="")
    sign4 = paste("a",j+1,sep=""); sign5 = paste("b",j+1,sep=""); sign6 = paste("c",j+1,sep="")
    eq1 = paste(a,"*",sign1," + ",b,"*",sign2," + ",c,"*",sign3," + ",rhs, sep = "")
    eq2 = paste(a,"*",sign4," + ",b,"*",sign5," + ",c,"*",sign6," + ",rhs, sep = "")
    eqs = c(eqs, eq1)
    eqs = c(eqs, eq2)
    i = i+1
    j = j+1
  }
  
  #The first and last functions must pass through the end points.
  j = n
  a = (mat[1,1])^2; b = mat[1,1]; c = 1; rhs = -(mat[1,2])
  sign1 = paste("a","1",sep=""); sign2 = paste("b","1",sep=""); sign3 = paste("c","1",sep="")
  eq1 = paste(a,"*",sign1," + ",b,"*",sign2," + ",c,"*",sign3," + ",rhs, sep = "")
  eqs = c(eqs, eq1)
  
  a = (mat[j,1])^2; b = mat[j,1]; c = 1; rhs = -(mat[j,2])
  sign1 = paste("a",j-1,sep=""); sign2 = paste("b",j-1,sep=""); sign3 = paste("c",j-1,sep="")
  eq1 = paste(a,"*",sign1," + ",b,"*",sign2," + ",c,"*",sign3," + ",rhs, sep = "")
  eqs = c(eqs, eq1)
  
  #The fi rst derivatives at the interior knots must be equal.
  i=2; j=1
  while(i<n){
    a = 2*(mat[i,1])
    b = 1
    c = 0
    rhs = 0
    sign1 = paste("a",j,sep=""); sign2 = paste("b",j,sep=""); sign3 = paste("c",j,sep="")
    sign4 = paste("a",j+1,sep=""); sign5 = paste("b",j+1,sep=""); sign6 = paste("c",j+1,sep="")
    eq1 = paste(a,"*",sign1," + ",b,"*",sign2," + ",c,"*",sign3,sep = "")
    eq2 = paste("-",a,"*",sign4," + -",b,"*",sign5," + -",c,"*",sign6," + ",rhs,sep = "")
    eq1 = paste(eq1,eq2,sep=" + ")
    eqs = c(eqs, eq1)
    i = i+1
    j = j+1
  }
  
  m = AugCoeffMatrix(eqs)
  result = GaussJordan(m,verbose)
  mat_iter = result$mat_iterations
  values = result$xs
  #values = format(round(values, 4), nsmall = 4)
  
  k=1
  def = "function(x) "
  interval_eqs = c()
  cnt = 1
  while(k<=(n-1)){
    if(k==1){
      eq = paste(def,values[cnt+(n-2)],"*x"," + ",values[(cnt+(n-2))*2],sep="")
    }
    else{
      eq = paste(def,values[cnt],"*(x^2)"," + ",values[cnt+(n-1)],"*x"," + ",values[cnt+((n-1)*2)],sep="")
      cnt = cnt+1
    }
    interval_eqs = c(interval_eqs, eq)
    k = k+1
  }
  
  
  l=1
  finalFxn = NULL; x_interval = NULL;
  while(l<nrow(mat)){
    if(x>=mat[l,1] && x<=mat[l+1,1]){
      finalFxn = interval_eqs[l]
      x_interval = paste(mat[l,1]," <= x <= ",mat[l+1,1], sep = "")
    }
    l = l+1
  }
  
  finalFxn2 = eval(parse(text=finalFxn))
  estimate = finalFxn2(x)
  answer = list(intervals=interval_eqs, fxn=finalFxn2, estimate=estimate, fxn_string = finalFxn, x_interval = x_interval, mat_iter = mat_iter)
  return (answer)
}


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

# ========================= GAUSS-JORDAN ========================= #

#AUGMENTED COEFFICIENT MATRIX
AugCoeffMatrix <- function(system){
  vars = c()      #variables
  coeff = c()     #coefficients
  RHS = c()       #vector for right hand side values
  rownames = c()  #row names (rank)
  fvars = c()     #empty vector
  temp = c()      #temp vector used for checking frequency of variables
  freq = c()      #stores the values of the frequency of the variables
  i=1
  while(i<=length(system)){
    tempx = system[i]
    tempy = strsplit(tempx[1], "\\+") #will get the list seperated by + sign
    
    j=1
    anotha = 0
    while(j <= length(tempy[[1]])){
      tempz = strsplit(tempy[[1]][j], "\\*") #will separate the coefficient to its variable
      
      if(j <= length(tempy[[1]])){
        if(is.na(tempz[[1]][2])){
          RHS = c(RHS, (as.numeric(tempz[[1]][1]))* -1) #if the current string is without a variable, then it is the RHS (multiply to -1)
        }
        
        else{
          
          z <- gsub(" ", "", tempz[[1]][2]) #remove space
          if(z != "a1"){
            anotha = anotha+1 #counter for the number of variables
            vars = c(vars, z) #append the variable to the vector
          } 
        }
      }
      
      j = j+1
    }
    
    i = i+1
  }
  
  
  #this loop will get the frequency of each variables stored
  x = 1
  temp = c(temp, vars)           
  
  while(x <= length(temp)){ #from exercise 01  
    z = 1
    
    if(temp[x] != -1){           
      y = x+1                   
      
      while(y <= length(temp)){
        
        if(temp[x] == temp[y]){     
          z = z + 1             
          temp[y] = -1            
        }
        
        y = y + 1
      }
      
      freq = c(freq, z)         
    }
    
    x = x + 1
  }
  
  freFac = factor(freq) #factor the frequency to get the unique values (by getting its levels)
  fq = levels(freFac)
  finalVars = factor(vars, exclude = "")
  fv = levels(finalVars)
  
  #creating a zero matrix, with nrow based on the number of variables, and ncol based on the number of variables
  m = matrix(0, nrow = length(system), ncol = length(system), byrow=TRUE) 
  
  
  #this loop will get the coefficients
  i=1
  while(i<=length(system)){
    tempx = system[i]
    tempy = strsplit(tempx[1], "\\+")
    
    j=1
    anotha = 0
    while(j <= length(tempy[[1]])){
      tempz = strsplit(tempy[[1]][j], "\\*")
      
      if(j <= length(tempy[[1]])){
        if(is.na(tempz[[1]][2])){
        }
        else{
          z <- gsub(" ", "", tempz[[1]][2])
          f=1
          while(f<=length(fv)){
            if(z == fv[f] && z != "a1"){
              conv = as.numeric(tempz[[1]][1])
              m[i,f] = conv
            }
            f = f+1
          }
        }
      }
      
      j = j+1
    }
    
    i = i+1
  }
  
  new = cbind(m,RHS) #add the RHS vector at the last column
  varlist = list()
  for(i in fv) varlist = list(varlist, i)
  return(new)
  
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
  augmatrix = f
  j=1; lenRow = nrow(augmatrix); lenCol = ncol(augmatrix)
  pivotRow = 1; pivotCol = 1;
  mat_iterations = list()
  mat_iterations[[j]] = augmatrix
  
  while(j <= lenCol-1){
    augmatrix = Pivoting(augmatrix, pivotRow, pivotRow) #apply pivoting if necessary
    augmatrix = Normalize(augmatrix, j, j) #normalize the pizot row
    
    i=1
    while(i <= lenRow){
      
      if(i!=j){ #if the row is not equal to the column number, then it is not in the main diagonal
        multiplier = augmatrix[i,j] #multiplier will be just the element you want to be zero
        
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
    pivotRow = pivotRow+1
    mat_iterations[[j]] = augmatrix
  }
  
  xs = augmatrix[, lenCol] #unknown values would be the remaining values after elimination, in the right hand side
  forelem = augmatrix
  
  gaussjordanResult <- list(forelem = forelem, xs = xs, mat_iterations = mat_iterations) #creates a list of the matrix and the unknown values
  return (gaussjordanResult) #return result
  
}

#SORTING VALUES
Sort <- function(mat){
  mat = mat[order(mat[,1]), ]
  return(mat)
}
