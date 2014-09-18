## An assignment at Data Science MOOC on coursera. R-Course assignment 2
## functions for handling matrixes, calculate inverse and caching.
## -----
## Commenting each line mostly to make sense and remember it better.

## The principal function for handling the matrix: creates, returns and
## updates a matrix and its cache values
makeCacheMatrix <- function(x = matrix()) {
        ## clearing cache as it might have been previously used
        ## by another "matrix"
        cache <- NULL
        ## set function
        set <- function(y) {
                ## setting value to x in the parent environment
                x <<- y
                ## clearing cache as we now have a new matrix
                cache <<- NULL 
        }
        ## get function returns the matrix stored, note that as x is not
        ## initialized in current environment, it will look in the parent
        get <- function() x
        ## setcache function sets cache in parent environment
        setcache <- function(inverse) cache <<- inverse
        ## getcache function sets cache in parent environment        
        getcache <- function() cache
        ## return a list with all functions
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}


## The function that handles the calculations of inverse matrix or returns 
## the inverse from cache.
cacheSolve <- function(x, ...) {
        ## x is the list that makeCacheMatrix function produces
        ## begin by getting inverse from makeCacheMatrix function
        cache <- x$getcache()
        ## checking if cache exists
        if(!is.null(cache)) {
                ## cache exists...
                message("getting cached data")
        } else {
        ## we've reached here which means cache was empty so we get the matrix
        matrix <- x$get()
        ## doing the solve calculation and assign it to cache
        cache <- solve(matrix, ...)
        ## set the inverse cache in makeCacheMatrix function
        x$setcache(cache)
        }
        ## return the cache
        cache
}
