##makeCacheMatrix computes the inverse of input matrix and save it in cache
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#cachesolve checks the cache for the matrix's inverse first before computes it
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


##test both macros
c=rbind(c(1, -0.3), c(-0.3, 1))
tstm=makeCacheMatrix(c)
cacheSolve(tstm)
