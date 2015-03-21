##Assumption: Input matrix is always invertible##

## makeCacheMatrix function - To create a list containing a function to
# 1. Set matrix value
# 2. Get matrix value
# 3. Set inverse of matrix value
# 4. Get inverse of matrix value

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(i) {
    m <<- i
    inv <<- NULL
  }
  get <- function() {m}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  #list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  list(get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function - To get inverse of the matrix 
# Algorithm flow: Checks if the inverse is already computed. If YES, get already
# computed inverse. If NO, compute inverse and set inverse value in the cache

cacheSolve <- function(m, ...) {
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("Fetching cached data...")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data)
  m$setinverse(inv)
  inv
}