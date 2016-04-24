## The functions in this file should be used to create basic matrix objects and
## to compute the inverse of square invertible matrices which will be stored in
## cache memory to avoid its expensive recomputation in case of large matrices.

## This function creates a "matrix" object that caches its inverse.
## The matrix has 2 attibutes, namely "x" that stores the actual matrix value
## and "inv" that stores the inverse of the matrix value. Each of the 
## attributes has a "getter" and a "setter" function. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the matrix object passed as its first
## argument. If the inverse has already been computed, then this function
## retrieves the inverse from the cache. This function assumes that the matrix
## stored in the "matrix" object is invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}
