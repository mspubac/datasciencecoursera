## Get the inverse of a matrix.. return the inverse from cache 
## if it is calulated and stored in cache otherwise calculate the inverse and return
## makeCacheMatrix - creates a matrix objects and stores the matrix inverse in the cache

makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
	set <- function(y){
		x <<- y
		invx <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) invx <<- inv
	getinverse <- function() invx
	list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of the matrix from cache, if it not available in cache
## creates the matrix and returns the inverse

cacheSolve <- function(x, ...) {
	invx <- x$getinverse()
	if(!is.null(invx)){
		message("Return the inverse from cache");
		return(invx)
	} else {
		invx <- solve(x$get())
		x$setinverse(invx)
		return(invx)
	}
}

## How to test the same
#> a <- makeCacheMatrix(matrix(7:10,2))
#> a$get()
#     [,1] [,2]
#[1,]    7    9
#[2,]    8   10
#> a$getinverse()
#NULL
#> a$set(matrix(10:13,2))
#> a$get()
#     [,1] [,2]
#[1,]   10   12
#[2,]   11   13
#> cacheSolve(a)
#     [,1] [,2]
#[1,] -6.5    6
#[2,]  5.5   -5
#> cacheSolve(a)
#Return the inverse from cache
#     [,1] [,2]
#[1,] -6.5    6
#[2,]  5.5   -5
#> a$getinverse()
#     [,1] [,2]
#[1,] -6.5    6
#[2,]  5.5   -5
