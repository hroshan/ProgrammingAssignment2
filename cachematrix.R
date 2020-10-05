## Here are two functions that enable us to cache a matrix inverse such that the
## overall processing/computing time/load decreases when we use an inverse of a 
## unchanged matrix more than one time. 

## The first function, makecacheMatrix, caches the inverse of a special "matrix"
## that the function creates.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set<- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The second function that follows, cacheSolve, calculates the inverse of the 
## special "matrix" that is provided by the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  matrix <- x$get()
  s <- solve(matrix, ...)
  x$setsolve(s)
  s
}