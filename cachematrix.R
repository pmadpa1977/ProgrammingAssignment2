## In this program, we duplicate two example functions written by Dr. Peng
## and available at the github repository 
## (see https://github.com/rdpeng/ProgrammingAssignment2).
## In particular, we adapt the example functions "makeVector" and "cachemean" to generate
## the functions makeCacheMatrix and cacheSolve, respectively.

## The first function, makeCacheMatrix, takes as input a matrix 
## and returns as output a list of functions.
## The function "set," defined below, defines a pair of GLOBAL variables 
## (note that the <<- operator defines these variables in the global environment,
## not in the function environment)

makeCacheMatrix <- function(x = numeric()) {
  if(!is.matrix(x)) stop("Input must be a matrix") ##check that the input is a matrix
  matrix <- NULL ## set initial value
  set <- function(y) {
    x <<- y
    matrix <<- NULL ## define a pair of GLOBAL variables (x and matrix)
  }
  get <- function() x ## 
  setinv <- function(solve) matrix<<-solve ## assigns a GLOBAL value to matrix
  getinv <- function() matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) ## returns a list of four functions
}


## cacheSolve replicates and adapts the code for cachemean (one of the example functions
## from the above repository)
## to first check if a matrix's mean has already been computed and stored in the cache
## and if not, cacheSolve computes the inverse and returns it
## The input to cachesolve is the ouput of makeCacheMatrix

cacheSolve <- function(x, ...) { ## the input to cachesolve is t
                                  ## the output from makeCacheMatrix.R 
  matrix <- x$getinv()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  } ## check to see if the matrix inverse has already been computed, and if so
  ## return it 
  data <- x$get() ## obtain the original matrix to be inverted
  matrix <- solve(data, ...) ## compute the inverse using the "solve" function
                              ## we should check here whether the matrix is 
                              ## invertible, but we were told to assume this
                              ## and the "solve" function will return an error
                              ## for a noninvertible matrix anyway
  x$setinv(matrix)
  matrix
}
## To use these two functions, on, say, an invertible matrix x,
## first compute makeCacheMatrix(x) (and you can assign a new variable 
## to this, say w), and then compute cacheSolve on the output
## i.e. cacheSolve(makeCacheMatrix(x))
## note that if you input a singular matrix, the built-in "solve"
## function itself will return an error


