## making a function to set.get values of a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  myInverse <- NULL
  ## setting values
  setVal <- function(y){
    x <<- y
    myInverse <<- NULL   ## here we don't set the values of the inverse
  }
  
  ##getting the matrix
  getVal <- function()x  
  ##setting inverse
  setInv <- function(inverse) myInverse <<- inverse  
  
  ##getting inverse
  getInv <- function() myInverse 
  
  list(setVal = setVal, getVal = getVal, setInv = setInv, getInv = getInv)

}


## calculate the inverse of matrix it it was not calculated before

cacheSolve <- function(x, ...) {
  inverse <- x$getInv()
  if(!is.null(inverse)){
    message("Inverse already calculated")
    return(inverse)
  }
  ## inverse not calculated , has to be manually calculated
  matrix1 <- x$getVal()
  inverse <- solve(matrix1,...)
  x$setInv(inverse)
  return(inverse)
        
     
}


