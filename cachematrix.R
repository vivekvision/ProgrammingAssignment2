## Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix

## Function makeCacheMatrix: creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
        
        i <- NULL
        
        ## 1. Function to set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## 2. Function to get the value of the matrix 
        get <- function(){
                x
        }
        
        ## 3. Function to set the value of Inverse matrix
        setInv <- function(inv){
                i <<- inv
        }
        
        ## 4. Function to get the value of Inverse matrix 
        getInv <- function() {
                i
        }
        
        # Return a list containing Functions (1 to 4)
        list(set = set, 
             get = get, 
             setInv = setInv,
             getInv = getInv)
        
}


## Function cacheSolve : computes the inverse of the special "matrix" created by function makeCacheMatrix 

cacheSolve <- function(x,....){

        ## If the inverse has already been calculated (and the matrix has not changed), 
        ## then just retrieve and return the inverse
        i <- x$getInv()
        
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        ## If not already in cache, Retrieve the matrix and calculate the inverse
        data <- x$get()
        i <- solve(data)
        
        ## set the calculated inverse matix in cache for future use
        x$setInv(i)
        
        ## Return the calculated inverse matrix
        i
}

