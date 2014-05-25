## Code for computing and hashing inverses of square matrices. 
## Function explanations:
##      - makeCacheMatrix
##              - Input: A square matrix with numeric elements
##              - Output: A list of functions for storing and
##                        retrieving the input matrix and the 
##                        corresponding inverse in and from cache
##      - cacheSolve
##              - Input: A square matrix with numeric elements,
##                       ev. other input is ignored
##              - Output: The inverse of the input matrix
##
## Written as part of the Coursera course "R Programming", spring 2014
## v.1.1, written by A.Shamsgovara, last update 26 may 2014, 1.09 AM CEST

## Function for getting functions for storing cache keys and
## values, where both keys and values are matrices

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ## Setting the key
                x <<- y
                m <<- NULL
        }     
        get <- function() x ## Retrieving the key
        setinverse <- function(solve) m <<- solve ## Setting the stored value
        getinverse <- function() m ## Retrieving the stored value
        list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## Function for computing the inverse of an input 
## square nonsingular matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse() ## Try to get the inverse from cache
        if(!is.null(m)) { ## Inverse already in cache, return it
                return(m)
        }
        data <- x$get() ## Get the matrix
        m <- solve(data, ...) ## Compute inverse
        x$setinverse(m) ## Store in cache memory
        m ## Return inverse
}
