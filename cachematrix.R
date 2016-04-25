makeCacheMatrix <- function(x = matrix()) {
                i <- NULL                               ## set the initial inverse to be NULL
                set <- function(matrix){
                        x <<- matrix
                        i <<- NULL
                }
                get <- function() x                     ## return the maxtrix
                
                setinverse <- function(inverse) i <<- inverse        
                
                getinverse <- function() i              ##return i when receiving getinverse command

                list(set = set, get = get, 
                setinverse = setinverse, getinverse = getinverse) 
}

cacheSolve <- function(x, ...) {
                i <- x$getinverse()                             
                
                if(!is.null(i)){                       ##test to see if the inverse of the matrix is null
                
                message("getting cached data")
                return (i)                             ##if i (the inverse) is not null, print out i
                
                }
                
                mat <- x$get()                        ##if the inverse is NULL, need to solve the inverse of the matrix and then print out i 
                i <- solve(mat, ...)
                x$setinverse(i)
                return (i)
}

