## The first function, makeVector function creates a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL 
	set <- function(y){ 	## set the value of the matrix
		x <<- y 
		m <<- NULL
	}
	get <- function() x  	## get the value of the matrix
	setSolve <- function(solve) m <<- solve 	## m is set as inverse of the matrix
	getSolve <- function() m  	## get the inverse of the matrix
	list(set=set,get=get,setSolve=setSolve,getSolve=getSolve) 	## the matrix is really a list
}
## The second function, cacheSolve computes the inverse of 
## the special "matrix" created with the first function.
cacheSolve <- function(x, ...) {
        m <- x$getSolve()  	## the getSolve function gets the inverse of 
				## the matrix without recomputing
        if(!is.null(m)) {  	## Check to see if the inverse of the matrix 
				## has already been calculated.
                message("getting cached data")
                return(m) 
        }
        data <- x$get() 	## get the matrix in to the data
        m <- solve(data, ...) 	## m is the inverse of the matrix 
				## calculated via the solve function
        x$setSolve(m) 		## m is set as the inverse of the matrix
        m 	## Return a matrix that is the inverse of x
}
