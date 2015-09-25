## The below funtions take a matrix as an argument and compute the inverse of that matrix
## Results is either cacluated or obtains the from cache if found

# The makeCacheMatrix function creates a special matrix with the below sub funtions enabled -
# 1. setmat = Sets the values to defalts
# 2. getmat = returns the matrix
# 3. setinverse = caches the inverse of the matrix
# 4. getinverse = returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  # Initialize
  m <- matrix(data = NaN, nrow = 1, ncol = 1)
  
  # Define setmat
  setmat <- function(y) {
    x <<- y
    m <<- matrix(data = NaN, nrow = 1, ncol = 1)
  }
  
  # Define getmat
  getmat <- function()
    x
  
  # Define setinverse
  setinverse <- function(inv_mtr)
    m <<- inv_mtr
  
  # Define getinverse
  getinverse <- function()
    m
  
  # Return the list of above defined functions
  list(
    setmat = setmat, getmat = getmat,
    setinverse = setinverse,
    getinverse = getinverse
    )
  
}


# The cacheSolve funtion calculates and returnd the inverse of a matrix
# it either cacluates the inverse or obtains the same from cache if found

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # Try to get the inverse from Cache
  m <- x$getinverse()
  
  # If the matrix is obtained from cache, print a confirmation message
  # and return the inverse matrix
  if (!is.nan(m[1]))
  {
    message("getting cached data")
    return(m)
  }
  
  # If the reverse matrix is not found in Cache, calculate the same using solve()
  data <- x$getmat()
  m <- solve(data)
  
  # Update cache with the above calculated inverse matrix
  x$setinverse(m)
  
  # Return the inverse matrix
  m
  
}