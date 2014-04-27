## Creates a special "matrix" object that can cache its inverse and computes the inverse of the special "matrix" 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){ ## set the value of the vector
                x <<- y
                s <<- Null
        }
        
        get <- function() x  ## get the value of the vector
        setInverse <- function(solve) s <<- solve ## set the value of the Inverse
        getInverse <- function() s  ## get the value of the Inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse() ## query the x vector's cache
        if(!is.null(s)){  ## if there is a cache
                message("getting cached data")
                return(s) ## prompting a message and return the cache, no computation needed
        }
        data <- x$get() ## if there's no cache
        s <- solve(data) ## compute the inverse of "x"
        x$setInverse(s) ## save the result back to x's cache
        s  ## return the result
}
