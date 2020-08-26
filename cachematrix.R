## MakeCacheMatrix contains 4 functions, a set function that can change the matrix that was originally inputted,
## a get function that returns the matrix x, a setinverse function that sets the inverse of the matrix x, and 
## a getinverse function that returns the inverse matrix. The makeCacheMatrix returns a list containing these
## four functions.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(matrix) {
    x <<- matrix
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(newinverse) inverse <<- newinverse
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## CacheSolve takes in a special 'matrix' object. It first tries to find if a inverse has been calculated and if
## it has, to return it. If not, it will fetch the original input matrix from makeChacheMatrix and will find the
## inverse of it using the solve function. It will then assign the set inverse function in the special 'matrix' 
## object to the calculated inverse and it will return the inverse.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(is.null(inverse) == FALSE) {
    message('Getting cached matrix')
    return(inverse)
  }
  getmatrix <- x$get()
  calculated_inverse <- solve(getmatrix)
  x$setinverse(calculated_inverse)
  calculated_inverse
        
}
