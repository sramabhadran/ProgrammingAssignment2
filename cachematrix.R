## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix takes in a matrix and returns a "special" matrix object 
## with inv element and set/get/setinv/getinv functions

makeCacheMatrix <- function(x = matrix()) {
  
  #initialize value of inverse matrix to NULL
  inv <- NULL
  
  # This function is built to (re-)set the matrix object
  # and therefore (re-)set inverse to NULL
  
  set <- function(y) 
  {
    
    x <<- y
    inv <<- NULL
    
  }
  
  # This function is just to "play back" the matrix object
  get <- function() x
  
  # This function can (re-)set the inverse
  setinv <- function(inverse) inv <<-inverse
  
  # This function to "play back" whatever inverse currently is
  getinv <- function() inv

  # Now send back the "special object"
  list(set = set, get = get, setinv = setinv, getinv=getinv)

}


## cacheSolve takes in an object of the type created by makeCacheMatrix()
## and either calculates inverse on fly or returns cached value if available

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
        
  ## Have we figured out the inverse already?
  inv <- x$getinv()
  
  if (!is.null(inv))
    {
     message("Not NULL - getting cached inverse matrix")
     return(inv)    
  }
  
  ## if we are here, the inv was NULL so nothing cached
  ## we need to actually calculate the inverse matrix
  
  message("Inverse is NULL...time to calculate...")
  orig <- x$get()
  
  this_inv <- solve(orig)
  
  x$setinv(this_inv)
  
  ## return this value
  this_inv
}
