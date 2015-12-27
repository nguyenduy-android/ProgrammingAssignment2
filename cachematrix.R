## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.

## This function create a list of 4 functions 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Reset inverseMatrix
  invM <- NULL
  ## Set new Matrix
  set <- function(y) {
    x <<- y
    ## Reset inverseMatrix
    invM <<- NULL
  }
  ## Get this Matrix
  get <- function() { x }
  ## Keep inverseMatrix after computing the inverse of matrix
  setinverse <- function(inverse) { invM <<- inverse }
  ## Get inverseMatrix
  getinverse <- function() { invM }
  ## Create function list
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of a matrix. If the inverse have already been computed, 
## it gets the result and overcome the computation, else it computes the inverse by 'solve' function 
## and save the result via setinverse.

## This function assumes that the matrix is always invertible (square matrix, col = row)
## 1. x is the function vector created from 'makeCacheMatrix'.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Get cached inverse if existed
  invM <- x$getinverse()
  if(!is.null(invM)) {
    ## Existed!!!
    message("Aha!getting cached data.")
    return(invM)
  }
  ## Get the matrix  
  data <- x$get()
  ## Compute the inverse of matrix
  invM <- solve(data)
  ## Save the result
  x$setinverse(invM)
  invM
}

## Sample
## x <- matrix(c(4,3,3,2), 2,2)
## m <- makeCacheMatrix(x)
## cacheSolve(m)
## Show the inverse
## m$getinverse()
## Check matrix % * % inverse = identity matrix
## m$getinverse() %*% m$get()
## Eureka
