## The makecache function  encloses set ,get, setmatrix and getmatrix functions i.e. 
## the enclosed functions can  be subsetted using superset, $, subset.
## The makecache function accepts a Marix input and can be overwritten by "SET" fnction
## The GET function gets the latest matrix
## The SETMATRIX function accepts  a matrix or inverse
## The GETMATRIX function gets the accepted matrix (or inverse )


makeCacheMatrix <- function(x = matrix()) {
  i_mat <- NULL
  set <- function(y) {
    x <<- y
    i_mat <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) i_mat <<- solve
  getmatrix <- function() i_mat
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
  
}


## This finction checks the existance of a inverse and if not
## calcuclates the inverse  using "SOLVE" base finction

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 i_mat <- x$getmatrix()
  if(!is.null(i_mat)) {
    message("getting cached data")
    return(i_mat)
  }
  data <- x$get()
  i_mat <- solve(data, ...)   #calculates inverse of a square matrix
  x$setmatrix(i_mat)
  i_mat
}
