## The following functions could  be used during calculation of matrix inversion, which can be time-consuming operation. Instead of performing the same computation few times for same matrix, the result can
## be cached and used in later operations. "makeCacheMatrix" and "cacheSolve" functions provide that functionality.

## "makeCacheMatrix" function creates a special "matrix", which later is used as an argument in "cacheSolve" function.
## "makeCacheMatrix" function returns a list containing a functions to: set the value of matrix, get the value of matrix, set the value of inverse and get the value of inverse

makeCacheMatrix <- function(mt = matrix()){
        m <- NULL
        set <- function(y){				## setter - setting the makeCacheMatrix variables
                mt <<- y
                m <<- NULL
        }         
		
	get <- function() mt				## getter - getting mt (makeCacheMatrix argument) value
		
	setInv <- function(solve) m <<- solve		## setter - setting m (inverse matrix)
        getInv <- function() m				## getter - getting m value
        list(set = set, get = get,		        ## returned list of functions
             	setInv = setInv,
             	getInv = getInv)
}

## "cacheSolve" function takes as an argument list returned by "makeCacheMatrix". 
## Firstly, it checks if the inverse has already been calculated. In case it was, function returns the cached value of inverse. Otherwise, inverse matrix is calculated and the result is returned.

cacheSolve <- function(x, ...){
        m <- x$getInv()					      ## get the cached value
	
	if(!is.null(m)){				      ## if the value is not empty, cache is returned
		message("Getting cached data")
		return(m)
	}
	
	data <- x$get()					      ## otherwise, get matrix 
	m <- solve(data, ...)		              	      ## and calculate inverted matrix 
	x$setInv(m)					      ## set cached value 
	m											
}