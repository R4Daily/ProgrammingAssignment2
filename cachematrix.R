## The makeCacheMatrix function creates a list representing a matrix object and functions for setting and
## retrieving the matrix object and for setting and retrieving a cached copy of its inverse.
## If a matrix is passed to the function as an argument, its value is cached otherwise, an empty matrix object
## is created and cached. On creation, the inverse of the matrix is set to NULL.
## 
##  The returned list contains the following functions:
##     set(matrix) :  set the matrix object to the one supplied and reset the inverse matrix to NULL.
##     get()       :  get (return) the matrix object.
##     setInverse(inverse)    :  cache the matrix inverse
##     getInverse()    :  get (return) the cached inverse
##     
##  

## A short comment describing the makeCacheMatrix function:
## This function creates a list object as a structure for representing a matrix object and its (calculated)
## inverse matrix. It does not perform the calculation on the inverse, it only provides methods of caching
## and accessing the respective matricies.
##
## This function relies on a matrix being stored. This may be supplied as an 
## argument, but if none is supplied, an empty matrix is created and may be
## subsequently overwritten using the set() function. The environment of the
## object created by makeCacheMatrix() is used to store the matrix and its
## inverse (which is set to NULL initially). When set() is called to update
## the matrix, the <<- operator is used to access the variables as they are
## in it's parent environment and not in set()'s local scope. If the usual
## <- assignment operator were used, only variables local to set() would be
## assigned and would be lost on exit from the function.
##
## The get() method uses the lexical rules scoping in R to access the parent
## environment to resolve the value of x (the matrix object) as get() has no
## local variable x.
##
## The setInverse() and getInverse() functions operate in the same manner to
## set() and get() regarding access to the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL    # initialise the inverse matrix to be cached
  set <-function(y){
    x <<- y       # set the (locally bound) formal parameter matrix x passed
    # to the enclosing function to y
    invm <<- NULL # clear any cached inverse matrix
  }
  get <- function() x    # get the (locally) stored matrix object
  
  setInverse <- function(inverse) invm <<- solve
  getInverse <- function() invm
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse) # return an instance of CacheMatrix
}


## A short comment describing the cacheSolve function:
## This function takes a list object created by makeCacheMatrix above as an
## argument, and returns the previously calculated inverse if it exists
## (invm returnd from getInverse() is not NULL), otherwise it calculates it
## using the solve() function and caches the value back in the list object
## using its setInverse() function.
##
## Although the requirement is to calculate the inverse matrix, the function
## accepts the ... argument and passes this through to the solve() function
## so all of the solve() operations are available. 
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getInverse()
  if(!is.null(invm)) {
    message("getting cached inverse matrix")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data,...)
  x$setInverse(invm)
  invm
}
