## The functions can used to store a matrix and its inverse.
## If the inverse of the matrix is not stored, it calculate 
## the inverse and store it. If the inverse is already stored
## the function can read the stored information directly.


## This function returns a list of four functions:
## 1.set: set the matrix
## 2.get: obtain the matrix
## 3.setinverse: calculate the inverse of the matrix
## 4.getinverse: return the inverse

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
		set <- function(y){
				x <<- y
				inverse <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) inverse <<- solve
    	getinverse <- function() inverse
    	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function first check whether the inverse matrix has already
## been calculated and stored. If not, the function calculates, 
## stores and returns it.

cacheSolve <- function(x, ...) {
		inverse <- x$getinverse()
      	if(!is.null(inverse)) { ##determine whether inverse is defined
                message("getting cached data")
                return(inverse)
        }
        data <- x$get() 
        inverse <- solve(data, ...) ##solve() gives the inverse of the matrix
        x$setinverse(inverse)
        inverse
}
