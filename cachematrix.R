
################################################################################################################################################################################################################################################################################################################################
##  Matrix inversion is usually a costly computation and there may be some
##benefit to caching the inverse of a matrix rather than compute 
##it repeatedly. Following pair of functions that cache the inverse of a matrix.  ##
################################################################################################################################################################################################################################################################################################################################


# R function that can cache potentially time consuming operations



# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # We assume that the Matrix we supply here is invertable
  inv <-  NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse )
  
}



#This Function caches the Inverse of the Matrix created by the above function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...) #solve function computes the inverse
  x$setInverse(inv)
  inv
}
  

#"To Test the script source the script in your environment and Test the same with square matrix as an input to function makeCacheMatrix
#Example:



  ###  pmatrix <- makeCacheMatrix(matrix(rnorm(9), nrow = 3 , ncol = 3))   
  ###  > pmatrix$get()                                                     
  ###             [,1]      [,2]      [,3]                                 
  ###  [1,]  0.6612762  1.701324 0.4329329                                 
  ###  [2,] -0.9370503 -1.003780 0.2710641                                 
  ###  [3,] -0.5803140  1.098916 1.7715871                                 
  ###  > pmatrix$getInverse()                                              
  ###  NULL                                                                
  ###  > cacheSolve(pmatrix)                                               
  ###            [,1]      [,2]      [,3]                                  
  ###  [1,] -4.273900 -5.225211  1.843928                                  
  ###  [2,]  3.093528  2.928805 -1.204109                                  
  ###  [3,] -3.318907 -3.528348  1.915386                                  
  ###  > pmatrix$getInverse()                                              
  ###            [,1]      [,2]      [,3]                                  
  ###  [1,] -4.273900 -5.225211  1.843928                                  
  ###  [2,]  3.093528  2.928805 -1.204109                                  
  ###  [3,] -3.318907 -3.528348  1.915386                                  
  ###  
  


