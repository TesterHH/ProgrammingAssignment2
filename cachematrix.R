## There are two functions for performing cached matrix inversion.
## Based on Caching the Mean of a Vector code by R. D. Peng
## Usage:
## MyMatrixCached<-makeCacheMatrix(MyMatrix)  set up the caching functionality
## !!! please remind to run the command above after each update of MyMatrix,
## !!! the check of consistency of MyMatrix and MyMatrixCached values is not implemented
##
## cacheSolve(MyMatrixCached, ...)  runs inversion procedure solve(MyMatrix, ...)
## once pro makeCacheMatrix() update; on further runs, until next  makeCacheMatrix() update,
## just returns  the inverse matrix MyMatrix^{-1} stored in memory (inv)
##
##
##    Example:
## source("cachematrix.R")
## MyMatrixSize=1000  #better not to put it above 5000 for single-core calculation
## MyMatrix<-matrix(data = runif(MyMatrixSize*MyMatrixSize, min = 0, max = 1), nrow = MyMatrixSize, ncol = MyMatrixSize)
## here we create matrix MyMatrix of size MyMatrixSize*MyMatrixSize
## filled with uniformerly distributed random numbers.
##
## MyMatrixCached<-makeCacheMatrix(MyMatrix)
## # here we set up the caching functionality for MyMatrix
##
## cacheSolve(MyMatrixCached)
## # here we calculate the inverse of MyMatrix and return it
##
## cacheSolve(MyMatrixCached)
## # here we do not calculate the inverse of MyMatrix, just return it from cache
##
## MyMatrix<-matrix(data = runif(MyMatrixSize*MyMatrixSize, min = 0, max = 1), nrow = MyMatrixSize, ncol = MyMatrixSize)
## MyMatrixCached<-makeCacheMatrix(MyMatrix)
## # here we update MyMatrix and resetting cache (it is necessary after each matrix update),
## afterwards new inverse will be calculated and put to cache



## The makeCacheMatrix function: auxiliary function for setting up caching process
## first it purges cache: inv <- NULL
## $set: initialization
## $get: returns original matrix
## $setinv(inverse): put argument, with which it was called (inverse) to cache (inv) 
## $getinv returns cache (inv)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function:
## returns inverse of initial matrix (the one used as a first argument in
## the previously called makeCacheMatrix function)
##
## First it tries to return the cached value (inv) taking it as x$getinv(),
## if not sucseed (it means first call after makeCacheMatrix initialization),
## it calculate the inverse by inv <- solve(data, ...) taking original matrix by
## data <- x$get() and store calculated inverse to cache (x$setinv(inv))

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv  
        ## Return a matrix that is the inverse of 'x'
}
