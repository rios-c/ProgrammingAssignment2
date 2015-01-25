## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

# Function to cache a Matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Using makeVector as a template
  # This will create a copy of a matrix and save its inverse.
  m <- NULL

  # Create the get and set functions for the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  # create the get and set for the matrix inverse.
  setinverse <- function(i) m <<- i
  getinverse <- function() m
  
  # return as a list the getters and Setters for the matrix and inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## Write a short comment describing this function

# Function to calculate the inverse of a cached square matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # check it is a square matrix
  if ( dim(m)[1] != dim(m)[2] ) {
      stop("Not a square matrix.")
  }
  m <- x$getinverse()
  
  # If m is not null get the caches inverse value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If m is null then calculate the inverse and save
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
