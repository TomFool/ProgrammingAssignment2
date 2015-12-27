## makeCacheMatrix creates a list containing matrix and a null placeholder for it's inverse, as well as supporting funcitons.
## cacheSolve fills in the above null placeholder with the matrix inverse the first time it is called on the list.
## If cacheSolve is called on the list again, it will used the stored value for the inverse rather than calculating it again.



## makeCacheMatrix returns a 4 item list containing the inputed matrix and supporting funcitons.
## The key supporting functions here are $setsolve and $getsolve which will both initially be set to null by this function but used in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function()x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(
    set = set,
    get = get,
    setsolve = setsolve,
    getsolve = getsolve
  )
}


## cacheSolve will take a 4 item list from makeCacheMatrix
## If this is the first time cacheSolve has been called on the list, it will calculate the inverse matrix, store it in the list, and return it.
## Otherwise, it will return the previously calculated inverse now stored within the list without calculating it again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

