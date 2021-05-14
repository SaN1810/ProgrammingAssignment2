## Coursera R programming Week3 assignment ; makeCacheMatrix, cacheSolve

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set=set, get=get, setinverse=setinv, getinverse=getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message("get cached data")
    return(inv)
  }
  
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, )
  x$setinv(inv)
  inv
}

