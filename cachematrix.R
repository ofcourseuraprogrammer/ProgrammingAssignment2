## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  
  get <- function() x
  setcache <- function(solve) m <<- solve
  getcache <- function() m
  list(set = set, get = get, 
       setcache = setcache, getcache = getcache)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting the cached data")
    return(m)
  }
  data <- x$get()
  m <- tryCatch( solve(data), warning = function(w){print("WARNING")}, 
                 error = function(e) {print("Singularity")})
  x$setcache(m)
  m
  
}
