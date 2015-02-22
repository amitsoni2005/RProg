#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.  
#makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the Matrix
#get the value of the Matrix
#cacheInverse the value cacheInverseMatrix
#getCacheInverse the value of the inverse stored in cache.
makeCacheMatrix <- function(x = numeric()) {
  # intialize to null
  cacheInverseMatrix <- NULL
  
  # Set the matrix to the passed value
  setMatrix <- function(matrix) {
    x <<- matrix
    #need to reset the cacheInverseMatrix to NULL because this is a new matrix
    cacheInverseMatrix <<- NULL
  }
  
  # return the matrix already stored.
  getMatrix <- function() x
  
  # Store the passed cache value.
  cacheInverse <- function(cacheSolve) cacheInverseMatrix <<- cacheSolve
  
  # compute the inverse and store it in cacheInverseMatrix.
  getCacheInverse <- function() cacheInverseMatrix
  
  # return the references to the functions.
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       cacheInverse = cacheInverse,
       getCacheInverse = getCacheInverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been 
#calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(matrix, ...) {
  #get the cached inverse value
  cacheInv<- matrix$getCacheInverse()
  
  #if the value is already cached no need to compute return that value
  if(!is.null(cacheInv)) {
    return(cacheInv)
  }
  #compute the inverse cacheInv is null
  data <- matrix$getMatrix()
  # compute the inverse
  cacheInv <- solve(data)
  # cache the inverse
  matrix$cacheInverse(cacheInv)
  
  #return the cache Inverse Matrix
  cacheInv
}