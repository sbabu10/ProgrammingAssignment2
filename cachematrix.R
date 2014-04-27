
## there are two functions
## usage: cacheSolve(makeCacheMatrix(inp))
## input paramter: square matrix
## returns inverse of square matrix

## makeCaheMatrix function 
## this function returns a list consisting of functions for 
##       setting matrix, 
##       getting matrix
##       setting matrix inverse
##       getting matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(sol) m <<- sol
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
  
}


## cacheSolve function
## input parameter: list returned by makeCacheMatrix function
## returns matrix inverse if already calculated and available
##                        else calculates and returns
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached matrix inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

