
### Calculating  inverse square  matrix
### return: a list containing functions to
###              1. set  matrix
###              2. get  matrix
###              3. set  inverse
###              4. get  inverse
### This list is used as the input to cacheSolve()

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
#calculating insverse of matrix


###  output of makeCacheMatrix() is input of cachesolve() --> this fn returns:
### return: inverse of the original matrix input to makeCacheMatrix()
###
### The following function returns the inverse of the matrix. It first checks if
### the inverse has already been computed. If so, it gets the result and skips the
### computation. If not, it computes the inverse, sets the value in the cache via
### setinverse function.
###
### This function assumes that the matrix is always invertible.



cacheSolve <- function(x, ...) {
          
          
          inv <- x$getinverse()
          
          
          if(!is.null(inv)) {
                    
                    
                    message("getting cached data.")
                    
                    
                    return(inv)
                    
          }
          data <- x$get()
          inv <- solve(data)
          x$setinverse(inv)
          inv
}
 ### sample run
 ###> x = rbind(c(1, -1/2), c(-1/2, 1))
 ###> m = makeCacheMatrix(x)
####> m$get()
#[,1] [,2]
#[1,]  1.0 -0.5
#[2,] -0.5  1.0
# No cache 
#> cacheSolve(m)
#[,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333

# from cache
#getting cached data.
#[,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333
