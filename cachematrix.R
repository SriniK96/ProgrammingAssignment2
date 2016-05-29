## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     mat <- NULL
     set <- function(y) {
         x <<- y
         mat <<- NULL
         }
         get <- function() x
      setinv <- function(inv) mat <<-inv
      getinv <- function() mat
     list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
   } 
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'mat <- x$getinv()
        mat <- x$getinv()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        newmat <- x$get()
        mat <- solve(newmat)%*%newmat
        x$setinv(mat)
        mat
}
