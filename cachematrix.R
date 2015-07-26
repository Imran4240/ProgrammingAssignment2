
#inverse of matrix

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


cacheSolve <- function(x, ...) {


inv <- x$getinverse()


if(!is.null(inv)) {


message("getting cached data.")


return(inv)
#calculating insverse of matrix

makeCacheMatrix <- function(x = matrix()) {
          rev <- NULL
          set <- function(y) {
                    x <<- y
                    rev <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) rev <<- inverse
          getinverse <- function() rev
          list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
          rev <- x$getinverse()
          if(!is.null(rev)) {
                    message("getting cached data.")
                    return(inv)
          }
          stor <- x$get()
          rev <- solve(stor)
          x$setinverse(rev)
          rev
}


