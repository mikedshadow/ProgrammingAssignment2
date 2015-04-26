## makeCacheMatrix is a function that stores a list of functions in this case 'set', 'get', 'setinverse' and getinverse
## 'get' just returns the input vector x 
## 'set' replaces x with the input and resets the inv (inverse) that is stored so it will need to be recalculated
## 'setinverse'  stores the value of inv in a variable inverse of the main function makeCacheMatrix
## 'getinverse' returns the value stored by getinverse

makeCacheMatrix <- function(x = matrix()) 
  { inv <- NULL 
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)                                            

  }


## The function 'cacheSolve' takes input that is the object of makeCacheMatrix
## it first checks if inv the output of getinverse is NULL if not it returns inv as the inverse matrix
## if it is NULL (the else case) then it  assigns data to be the  vector from makeCacheMatrix
## assigns inv  to be the inverse (solve function) and stores the newly computed inverse as inv  using the 
## setinverse function from makeCacheMatrix, then returns the matrix in inv.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

