## The aim is to create functions that cache the inverse of the matrix
## The following funcition are used:makeCacheMatrix and cachesolve
## The functions can be used as shown in the example below
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## > cacheSolve(m)
## [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5

## makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse.
## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )

}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
