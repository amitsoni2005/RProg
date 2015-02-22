#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.  
#makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the Matrix
#get the value of the Matrix
#cacheInverse the value cacheInverse
#getCacheInverse the value of the mean
makeCacheMatrix <- function(x = numeric()) {
		# intialize to null
        cacheInverse <- NULL

        # Set the matrix to the passed value
        setMatrix <- function(y) {
                x <<- y
                #need to reset the cacheInverse to NULL because tis is a new matrix
                cacheInverse <<- NULL
        }

        # return the matrix already stored.
        getMatrix <- function() x

        # Store the passed cache value passed.
        cacheInverse <- function(cacheSolve) cacheInverse <<- cacheSolve

        # compute the inverse and store it in cacheInverse.
        getCacheInverse <- function() cacheInverse

        # return the references to the functions.
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             cacheInverse = cacheInverse,
             getCacheInverse = getCacheInverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been 
#calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}