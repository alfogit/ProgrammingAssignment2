## Caching the inverse of a Matrix
## Alfredo Maldonado for R Programming coursera course

## Creates the cache matrix object and gets and sets matrix and its inverse
makeCacheMatrix <- function(x = matrix()) 
{
        minv <- NULL
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(minverse) minv <<- minverse
        getinverse <- function() minv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of a matrix and caches the result. Returns cached version each time it is called.
cacheSolve <- function(x, ...) 
{
        minv <- x$getinverse()
        if(!is.null(minv)) 
        {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinverse(minv)
        minv
}
