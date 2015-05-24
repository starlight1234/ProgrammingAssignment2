## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	localinv <- NULL
	localx <- NULL
	save <- function(i = matrix()) {
		localx <<- i;
		localinv <<- NULL;
		message("Saved matrix value")
		message(i)
	}
	fetch <- function() return(localx);
	saveinv <- function(y) {
		localinv <<- y;
		message("Saved inverse value")
		
	}
	fetchinv <- function() return(localinv);
	return(list(save=save,fetch=fetch,saveinv=saveinv,fetchinv=fetchinv))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$fetchinv()
	if(!is.null(inv)) {	
		message("Cache hit!")
		return (inv) }
	message("Cache miss!")
	value <- x$fetch()
	inv <- solve(value)
	x$saveinv(inv)
	return(inv)
}
