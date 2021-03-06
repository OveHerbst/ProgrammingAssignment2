

## Cheers fellow R learner, I provided you with a comment for every major section of my formulas so you find it easy to follow. Have fun correcting!
## Generally speaking, we would like to identify a matrix, fixing it in position to use it for future caluclations, setting its value and also
## setting the value of the inverse of said matrix. I mainly followed the example structure given in the assignement information. 
## As you know, everything starting with ## is a markdown section and only helps you to understand my thinking. Thank you and have fun!
MakeCacheMatrix <- function(x = matrix()){
  ## assuming that the matrix is invertible
  inv <- NULL
  ## as the matrix is not defined yet, we need to assign a value to the matrix with the set function, so we can cache it later
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ## once the value is set, we need to extract the value to be able to compute further steps, so basically telling R this is a fixed matrix and you can cache its value and it inverse
  get <- function() {x}
  ## we continue with the same steps in the original matrix to set and get the inverse (every matrix unique twin)
  setinverse <- function(inverse) {inv <<- inverse}
  getvalinverse <- function() {inv}
  ## creating a list of all values helps in 1) seeing what I actually programmed and 2) subsetting a desired value with one line of code 
  list(set = set, get = get, setinverse = setinverse, getvalinverse = getvalinverse)
}



## As stated above, you will find my comments with every major section of the code :)

CacheSolve <- function(x, ...) {
  ## Note, that we are still disecting the same matrix as above, but now we actually set the inverse. 
  inv <- x$getvalinverse()
  ## see if matrix inverse is already calculated in the cache (might be through past calculations or similar), if yes use the cache, if no compute the inverse 
  if(!is.null(inv)) {
    message("Getting cached matrix value")
    return(inv)
  }
  ## This else section of my if-statement calculates the inverse of my matrix with the solve- function and the matrix itself as its single required argument
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  return(inv)
}

## Overall, all we did here, was defining a matrix, fixing it (since we might compute with it multiple times in the future) and getting its inverse to ensure 
## its uniqueness and calculability. I have to admit that I stumbled across some problems when testing my formulas with a couple matrices. For example the 4 by matrix
## created an error, stating that the inverse is perfectly singular. I will check my problem and see if I can improve the formula. It does its job for the most part.
