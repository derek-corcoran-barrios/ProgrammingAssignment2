## MakeCacheMatrix makes a list to be called by cacheSolve, both toghether calculate 
##the inverse of a matrix and store it in cache, if you calculate the 
##invers of a matrix twice, the second time it wont calculate ir but retrive it from the cache





## Creates a list of functions to be called by cacheSolve from matrix x

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

}


## if a matrix created by makeCacheMatrix is passed a first time, it calculates the inverse and stores it
##If the matrix was already passed through this function it returns it toghether whith the text
###getting cached data

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
