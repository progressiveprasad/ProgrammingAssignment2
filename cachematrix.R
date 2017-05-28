## There are two functions here, makeCacheMatrix() which creates a "special" matrix object
## which can cache its inverse and second one, cacheSolve() calculates the inverse of this "special" matrix
## using R's solve() function. If the inverse has already been calculated and the contents
## of the original matrix reamain unchanged, then the cached matrix is retrieved and the message
## "getting cached matrix" is displayed. This is achieved using the <<- operator which keeps
## looking up parent environments to find the name/variable

## makeCacheMatrix() creates a "special" object. It contains a list which sets and gets
## the value of the matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
minv <- NULL
set <- function(y){
  x <<- y
  minv <<- NULL
}
get <- function() x
setInv <- function(inversem) minv <<- inversem
getInv <- function() minv
list (set = set, get = get,
      setInv = setInv,
      getInv = getInv)
}


## cacheSolve() calculates the inverse of the matrix created using makeCacheMatrix()
## if the matrix is already created and unchanged, then the cached matrix is retrieved.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getInv()
  if(!is.null(minv)){
    message("getting cached matrix")
    return(minv)
}
matrix1 <- x$get()
minv <- solve(matrix1, ...)
x$setInv(minv)
minv
}
