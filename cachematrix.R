# this function is to create objects that stores the
# value of the matrix when passed on to it for the first time
# from cache Solve

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() {x} 
  setinvmat <- function(invMat) {mat <<- invMat}
  getinvmat <- function() {mat}
  list(set = set, get = get,
       setinvmat = setinvmat,
       getinvmat = getinvmat)
}

# this function returns inverse of the matrix passed on as an arguement
# first time - create an inverse matrix object "mat" and stores the mat 
# into setMean function created earlier (lexical scoping)
# next time when the same object is called getintmat function returns the
# cached object


cacheSolve <- function(x, ...) {
  mat <- x$getinvmat()
  if(!is.null(mat)) {
    message("getting inverse matrix from cache")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data)
  x$setinvmat(mat)
  mat
}