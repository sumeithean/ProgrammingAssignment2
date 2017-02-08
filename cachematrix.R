## As matrix inversion is usually a costly computation, there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following functions below are used to cache matrix inversion.

## makeCacheMatrix creates a list containing a function to:-
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inversion of the matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The inverse of the matrix is returned in the following function. Firstly, it 
## checks if the inverse has already been computed. If so, the results are 
## generated and it skips all computation. However, if the inverse is yet to be 
## computed, the function will compute it and set the value in the cache via the
## setinverse function. 

## Note: This function assumes that the matrix is ALWAYS invertible.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- inv(data,...)
      x$setinverse(inv)
      inv
}

## Sample Run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## > cacheSolve(m)
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
