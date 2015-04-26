
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv) 
}




cacheSolve <- function(x, ...) {
  matrix <- x$getInv()
  if(!is.null(matrix)) {
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data, ...)
  x$setInv(matrix)
  matrix
}
