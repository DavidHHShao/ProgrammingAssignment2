## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix: This function creates a special "matrix" object
##                    that can cache its inverse.
##  cacheSolve: This function computes the inverse of the special "matrix" 
##              returned by makeCacheMatrix above. If the inverse has already
##              been calculated (and the matrix has not changed), then the 
##              cachesolve should retrieve the inverse from the cache.




## Write a short comment describing this function

## makeCacheMatrix creates a special "Matrix", which is really a list containing 
## a function to
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse of the matrix
##     get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  }
  
  


## Write a short comment describing this function
##  The following function calculates the inverse of the special "matrix" created with 
##  the above function. 
##  First, it checks to see if the inverse of the matrix has already been calculated. 
##  If so, it gets the inverse of the matrix from the cache and skips the computation. 
##  Otherwise, it calculates  the inverse of the matrix of the data and sets the value 
## of the  the inverse of the matrix  in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$set(m)
  m
}
  
  