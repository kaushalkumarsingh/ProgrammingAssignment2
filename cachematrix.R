## Put comments here that give an overall description of what your
## functions do

## A function to create a list of 4 functions: set,get,setInverse,
## getInverse. The <<- operator which can be used to assign a value to an 
## object in an environment that is different from the current environment
makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseMatrix <<- inverse
        getInverse <- function() inverseMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Calculates the inverse of the matrix if it is not cached.
## Else returns the cached inverse of matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        ## Else returns the cached inverse of matrix.
        if(!is.null(inverseMatrix)) {
                message("getting cached matrix")
                return(inverseMatrix);
        }
        data <- x$get()
        ## Calculates the inverse of the matrix if it is not cached.
        inverseMatrix<-solve(data)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
