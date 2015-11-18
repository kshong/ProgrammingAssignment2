## This is a creator function that create a special matrix with a 
## list containing four functions: set, get, set_inverse, get_inverse.
## set: set the value of matrix
## get: get the value of matrix
## set_inverse: set the inverse matrix value of matrix 
## get_inverse: get the inverse matrix value of matrix

makeCacheMatrix <- function(x = matrix()) { 

  	inver <- NULL  
		
   	set <- function(y) {  ## set squre data to special matrix
						## initialize inverse matrix value to null
                x <<- y
                inver <<- NULL
        }
	
	get <- function() x	 ## get data from special matrix 	

	## set caculated inverse matrix data to special matrix
 	set_inverse <- function(inverse) inver <<- inverse

	## get cached inverse matrix value from special matrix
	get_inverse <- function() inver
	
	## assign each function pointer to a list

	list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)


 
} 

## The following function caculates the inverse matrix value of special 
## matrix created by makeCacheMatrix function.  However, it first checks 
## to see if the inverse matrix result has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips 
## the computation. Otherwise, it calculates the inverse matrix
## of the data and sets the inverse matrix result in the cache 
## via the set_inverse function.

 
cacheSolve <- function(x, ...) { 
        	## Return a matrix that is the inverse of 'x' 
	
	inver <- x$get_inverse()

	if(!is.null(inver)) { ## if the matrix has no inverse matrix value, 
					     ## caculate it.
                message("getting cached inverse")
                return(inver)
        }
	
	data <- x$get()
	inver <- solve(data, ...)
	x$set_inverse(inver)
	inver      ## return inverse matrix value


} 
