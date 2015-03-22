## Computes inverse of a matrix using caching method

## This function caches the inverse of a matrix
makeCacheMatrix <- function(mat = matrix()) {
  inverseMatrix <- NULL
  set <- function(x) {
    mat <<- x;
    inverseMatrix <<- NULL;
  }
  get <- function() mat;
  setInverse <- function(inverse) inverseMatrix <<- inverse;
  getInverse <- function() inverseMatrix;
  return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## This function computes the inverse of a matrix using solve function
cacheSolve <- function(mat, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- mat$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- mat$get()
  m <- solve(data)
  mat$setInverse(m)
  m
}
