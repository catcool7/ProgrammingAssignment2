## Overall Description: Compute the inverse of a matrix, cache the inverse, and retrieve the inverse.
## Note: Matrix 'x' cannot be a singular matrix, and must be a square matrix to invert.

## makeCacheMatrix function creates an object that is the inverse of the matrix 'x'. 
## It must calculate the inverse of 'x' first.

makeCacheMatrix <- function(x = matrix()) {
     imatrx <<- NULL # imatrx is inverse matrix name. Initially make sure it does not exist (NULL)
     set.matrx <- function(y) { # Use this to change the 'x' matrix initially entered to a new one:
          x <<- y # change the initial matrix
          imatrx <<- NULL # make sure the inverse matrix does not exist anymore (to solve inverse of matrix 'y')
     }
     get.matrx <- function() x    # cache(store) the matrix 'x'
     setsolve <- function(solve) imatrx <<- solve # solve the inverse of matrix 'x'
     getsolve <- function() imatrx # cache(store) the inverse of matrix 'x' just calculated
     list(set.matrx = set.matrx, get.matrx = get.matrx, setsolve = setsolve, getsolve = getsolve)
     # one line up stored the 4 functions
}


## cacheSolve function returns the inverse of the matrix 'x' that was cached in makeCacheMatrix.
## If the inverse of the matrix was not cached already, then this function will calculate
## the inverse of the matrix 'x' and return it.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     imatrx <- x$getsolve() # get the solved inverse matrix of matrix 'x'
     if(!is.null(imatrx)) { # if the above line resulted in an inverse matrix (not NULL), then:
          message("getting cached data") # this message will pop up first
          return(imatrx) # cached/stored inverse matrix of 'x' will be returned(& function stops)
     } # if no inverse matrix was created, then compute the inverse following steps below:
     data <- x$get.matrx() # get the original matrix
     imatrx <- solve(data,...) # assign the name of inverse matrix to function
     x$setsolve(imatrx) # calculate the inverse matrix
     imatrx # print the inverse matrix
}
