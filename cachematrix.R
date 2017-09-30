## Week 3 Assignment

## This little guy creates a weird matrix-like creature that can cache its own inverse
## with some help from his friend cacheSolve() down below

makeCacheMatrix <- function(x = matrix()) {
  
  ## Blank out the inverse n for now
  n <- NULL
  
  ## Create a function to set the value of the matrix 
  ## So if y is passed to set() from another function, this will be new matrix 
  ## Guarantee that if set() is used then inverse n is nullified, which will be an alert
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  
  ## Create a function to get the value of the matrix
  ## Grab x either from argument to big function or from set()
  get <- function() x
  
  ## Create a function to set the value of the matrix inverse
  ## So if inv is passed to setinv(), this will be the inverse
  setinv <- function(inv) n <<- inv
  
  ## Create a function to get the value of the matrix inverse
  ## Grab inverse n 
  ## Inverse n has either has been set with setinv() or is NULL
  getinv <- function() n
  
  ## Return all these beautiful babies
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## And this one is a buddy of the little guy above 
## Takes the weird matrix creature returned from above
## Checks to see if its inverse is already set and grabs it if so
## And if not, then calculates inverse and sets it
## Note: assuming matrix is always invertible (thank you, Roger)

cacheSolve <- function(x, ...) {
  
  ## Gets whatever is stored as the inverse from the little guy above
  ## Note: inverse n could be null if things have been neglected around the shop recently
  n <- x$getinv()
  
  ## If there's an actual inverse stored, grab and return that
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  
  ## If there wasn't an actual inverse stored, grab the matrix from little guy
  data <- x$get()
  
  ## And then -- finally! -- calculate the inverse of the matrix
  n <- solve(data, ...)
  
  ## Don't forget to store the inverse back with the little guy above
  x$setinv(n)
  
  ## And because we're nice we'll return the inverse to the console
  return(n)
}
