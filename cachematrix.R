## The interation between both functions allows for the computation of the
## inverse of a matrix (it's assumed that the matrix is invertible) and
## storing it as an in-memory object. When the inverted matrix is required 
## again, it is retrieved from the cache instead of being recomputed, 
## preventing potentially time-consuming computations.
## The functions allow for the computation of the inverse of a new matrix
## and cache it, replacing the inverted matrix previously stored in-memory.

## Creates a special matrix object and stores in-memory the inverse of 
## the matrix computed by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(result) inverse <<- result
        getinverse <- function() inverse
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## Returns the inverted matrix stored in-memory in makeCacheMatrix()
## or calculates the inverted matrix of a new matrix, stores it in-memory
## and returns it

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("retrieving inverse from the cache")
                return(inverse)
        }
        message("calculating,setting and returning inverse")
        new_matrix <- x$get()
        inverse <- solve(new_matrix,...)
        x$setinverse(inverse)
        inverse
}

