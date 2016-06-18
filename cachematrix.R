### makeCacheMatrix
## makeCacheMatrix function  returns a list of 4 functions:
## No.1 - set: store inputted matrix in parent enviroment and sets mInv to NULL what is a trigger for 
##        cacheSolve function and let it check if any computation has been previuosly made for this, exact
##        matrix.
## No.2 - get: returns the the matrix.
## No.3 - setinverseM: saves the result of the computation in the parent environment as the inverseM variable.
## No.4 - getinverseM: returns inverseM.

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setinverseM <- function(inverseM) mInv <<- inverseM
  getinverseM <- function() mInv
  list(set = set, get = get,
       setinverseM = setinverseM,
       getinverseM = getinverseM)
}


### cacheSlove
## cacheSolve function checks mInv value (if is or is not NULL) and return stored inverse of inputted martix
## (when mInv is not NULL) or makes new computation of it (via solve() function which provide inverse for our 
##  matrix when mInv is NULL)
cacheSolve <- function(x, ...) {
  mInv <- x$getinverseM()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  data <- x$get()
  mInv <- solve(data, ...)
  x$setinverseM(mInv)
  mInv
}

