## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## set the matrix
        ## get the matrix
        ## set the inverse
        ## get the inverse
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setInverse <- function() inv_matrix <<- solve(x)
        getInverse <- function() inv_matrix
        list(set = set, get = get , setInverse = setInverse, getInverse = getInverse)
}

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
        inv_matrix <- x$getInverse() ## returns matrix if already done inverse
        if(!is.null(inv_matrix)) {      ## if the matrix is not null,  return original inverse matrix 
                message("getting cached data")
                return(inv_matrix)
                
        }
        origin_mat <- x$get()  ## gets most recent cached matrix, which would be inverse if 
        inv_matrix <- solve(origin_mat, ...) ## makes the inverse matrix again
        x$setInverse(inv_matrix)        ## seems redundant? 
        inv_matrix
}
        
}
