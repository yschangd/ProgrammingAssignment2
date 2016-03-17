#1. make special vector by makeVector function
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverseM <- function(inverseM) inv <<- inverseM
        getinverseM <- function() inv
        list(set = set, get = get,
             setinverseM = setinverseM,
             getinverseM = getinverseM)
        
}

#2. cache inverse by passing #1 matrix
cacheSolve <- function(x, ...) {
        inv <- x$getinverseM()
        if(!is.null(inv)){
                message("getting cahced data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) #inverse function
        x$setinverseM(inv)
        inv
}
