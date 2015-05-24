## Put comments here that give an overall description of what your
## functions do

## This function returs a list of 4 separate functions: "set", "get", "setSolve" and "getSolve" 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # initializes m
  set <- function(y) { # function that sets x to externaly provided matrix y and m to empty
    x <<- y
    m <<- NULL
  }
  get <- function() x # function that returs x 
  setSolve <- function(Inverse) m <<- Inverse # function that sets m to externaly provided inverse of y 
  getSolve <- function() m # function that returs m 
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve) # function makeCacheMatrix() returs list of 4 functions which can be called by their names 
}
}


#function that calculates and stores the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve() # assigns m to the matrix stored in x$getSolve()
  if(!is.null(m)) { # check if m has been calculated already
    message("getting cached data") # print the message
    return(m) #return the value of m stored in x$getSolve() end exit the function cacheSolve()
  }
  data <- x$get() # in case m is empty retrive the matrix stored in x$get()
  m <- solve(data, ...) # find the inverse of m
  m # return m 
}
