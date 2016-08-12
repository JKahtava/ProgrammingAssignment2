## makeCacheMatrix and cacheSolve are two functions that are used to create 
## a special list object that stores a numeric matrix and caches its inverse.
## First, makeCacheMatrix builds a set of functions which are returned as a list
## to the parent environment.
## Second, cacheSolve calculates the inverse for the special "matrix" created by
## makeCacheMatrix, or retrieves it from the cache if the inverse is already solved.


## makeCacheMatrix creates a list containing a function to 1) set the value of the matrix,
## 2) get the value of the matrix, 3) set the inverse of the matrix, 4) get the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## For this assigment we assume that the matrix supplied is always invertible.
        ## Therefore, we do not check whether it is square and has full rank.
        
        matrix_inverse <- NULL
        set <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matrix_inverse <<- inverse
        getinverse <- function() matrix_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix. 
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inverse <- x$getinverse()
        if (!is.null(matrix_inverse)) {
                message("getting cached data")
                return(matrix_inverse)
        }
        data <- x$get()
        matrix_inverse <- solve(data)
        x$setinverse(matrix_inverse)
        matrix_inverse
}
