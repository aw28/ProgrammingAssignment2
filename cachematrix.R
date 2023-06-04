## Overall, these two functions check if a matrix inverse has been computed. If yes, it
## returns the matrix inverse. If not, it computes it, caches it for later and returns it now

## The first function creates a list with four functions in it
## First, creates an empty object named m
## create a function called set that saves y to x one environment up and m to be empty
## create function "get" which returns x 
## create function setinverse which returns 
#  the inverse of matrix m and saves in one environment up to solve
## create function getinverse to return m
## output the 4 created functions into a list to be later accessed by the second function

## The second function takes the result of the first function and saves the output of getinverse
## If m is non-empty (the inverse has been computed before) it retrieves the saved data
## Otherwise, it returns runs the get() function (which returns the matrix x that was the object of the first function)
## and saves it as an object called 'data'
## Then it runs solve on data (which computes the inverse of matrix x) and saves it to m 
## Then it calls setinverse to store ("cache") the inverse of x so it won't need to compute it again
## Then it returns m (the inverse of x)

## This function takes a matrix, computes its inverse and saves the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks if the inverse of x has been computed before. If not, it will compute it
## and cache it for later

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}