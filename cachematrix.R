## This function sets a matrix in order to convert it into a reversed matrix.

## The main tool for it is the inverse command in the first part, which 
## allows the second function to perform properly.

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y){
      x <<- y # setting our first change, double "<<" helps set values in different environments
      j <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) j <<- inverse # inverse is the core of setInverse here
    getInverse <- function() j 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}
  

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    j <- x$getInverse()
    if(!is.null(j)){
      message("getting cached data")
      return(j)
    }
    mat <- x$get()
    j <- solve(mat,...)
    x$setInverse(j)
    j
}

mat <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(mat)
cacheSolve(m1)
