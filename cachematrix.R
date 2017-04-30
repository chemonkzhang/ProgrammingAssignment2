## Put comments here that give an overall description of what your
## functions do

## "makeCacheMatric" is a function that creates a special matrix which can cache the inverse of its own. 

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y) {
		x<<-y
		i<<-NULL
	}
	get<-function() {
		x
	}
	setInverse<-function(inverse) {
		i<<-inverse
	}
	getInverse<-function() {
		i
	}
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## "cacheSolve" is a function that computes the inverse of the special matrix gotten from "makeCacheMatrix". If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i<-x$getInverse()
	if (!is.null(i)) {
		message("Cached data:")
		return(i)
	}
	m<-x$get()
	i<-solve(m,...)
	x$setInverse(i)
	i
}
