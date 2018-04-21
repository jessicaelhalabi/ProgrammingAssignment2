#cache to inverse matrix

## Write a short comment describing this function: set the value of the matrix, get the value of the matrix
##set the value of the inverse of the matrix, and get the value of the inverse of the matrix 

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



## Write a short comment describing this function: If the inverse has already been computed --> skips computation
##If not, it computes the inverse 

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