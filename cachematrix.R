## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

                #initialize the inverse to null        
                i <- NULL
                
                #set the matrix and set inverse to null
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                
                #get the matrix
                get <- function() x

                #set inverse 
                setinverse <- function(inverse) i <<- inverse
                
                #retrieve the inverse of the matrix
                getinverse <- function() i
                
                #return a list of 4 functions 
                list(set = set, get = get,
                     setinverse=setinverse,
                     getinverse = getinverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

                #retrieve inverse of matrix 
                #if the inverse hasn't been calculated before it will return null 
                #and will not enter the if loop the inverse is calculated using solve(x)
                #function where x is the matrix whose inverse is to be calculated
                #if "x$getinverse()" returns a value it means that the inverse was 
                #calculatedbefore and so cached value is used
                
                i <- x$getinverse()
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }
                
                #get the value of matrix
                data <- x$get()
                
                #calculate inverse
                i <- solve(data) 
                
                #set inverse
                x$setinverse(i)
                i
        
}
