## An assignment at Data Science MOOC on coursera. R-Course assignment 2
## makeCacheMatrix function creates, returns and updates a matrix 
## Furthermore the function stores and returns the inverse matrix.
## cacheSolve calculates the inverse to the matrix after checking that it
## does not already exist in the cache

## The principal function for handlign the matrix
makeCacheMatrix <- function(x = matrix()) {
        ## clearing inverse cache as it might have been previously used
        cache <- NULL
        ## set function
        set <- function(y) {
                ## setting value to x in the parent environment
                x <<- y
                ## clearing inverse cache as we now have a new matrix
                cache <<- NULL 
        }
        ## get function returns the matrix stored
        get <- function() x
        ## setinverse function sets cache variable in parent environment
        setinverse <- function(inverse) cache <<- inverse
        ## getinverse function sets cache variable in parent environment        
        getinverse <- function() cache
        ## return a list with all functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The fuction that handles the calculations for inverse functions or returns 
## the inverse from cache.
cacheSolve <- function(x, ...) {
        ## begin by getting inverse from makeCacheMatrix function
        cache <- x$getinverse()
        ## checking if cache exists
        if(!is.null(cache)) {
                ## cache exists...
                message("getting cached data")
                ## ...so return cache and exit function
                return(cache)
        }
        ## we've reached here which means cache was empty so we get the matrix
        matrix <- x$get()
        ## doing the solve calculation and assign it to cache
        cache <- solve(matrix, ...)
        ## set the inverse cache in makeCacheMatrix environment
        x$setinverse(cache)
        ## return the cache
        cache
}
