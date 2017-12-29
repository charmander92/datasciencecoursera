makeCacheMatrix <-  function(X) {
  # This function creates a special "matrix" object that can cache its inverse.
  invX <- NULL
  set <- function(y) {
    X <<- Y
    invX <<- NULL
  }
  get <- function() X
  setInv <- function(inv) X <<- solve
  getInv <- function() X
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}

cacheSolve <- function(X,...) {
  # This function computes the inverse of the special "matrix" returned 
  # by makeCacheMatrix above. If the inverse has already been calculated 
  # (and the matrix has not changed), then the cachesolve should retrieve 
  # the inverse from the cache.
  invX <- X$getInv()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  data <- X$get()
  invX <- solve(data, ...)
  invX$setInv(invX)
  invX
  
}

A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
