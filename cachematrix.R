makeCacheMatrix <- function(x = matrix()){
  i <- NULL  ## create a variable to store the inverse of original matrix y
  set <- function(y){
    x <<- y    ## set the value of the target matrix y to the created "matrix" object x
    i <<- NULL  ## create a variable to store the inverse of original matrix y
  }
  
  get <- function() x ## return the specital "matrix" object
  setinverse <- function(inverse) i <<- inverse  ## set the value of i, which is the inverse matrix of target matrix
  getinverse <- function() i  ## get the inverse matrix of original target matrix
  
  list(   ## list of all the component functions
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


cacheSolve <- function(x, ...){
  i <- x$getinverse()   ## from the "matrix" object x, get inverse matrix
  if (!is.null(i)){
    message("getting cached data") ## if the inverse matrix is already calculated, get it
    return(i)
  }
  
  ## if the inverse matrix is not calculated, calculate it
  data <- x$get() ## get the original matrix
  i <- solve(data, ...)  ## calculate the inverse matrix
  x$setinverse(i)  
  i
}
