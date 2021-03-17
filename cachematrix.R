## Setting up caching for a matrix inverted
## -----------------------------------------------------

## Create a "special matrix" matrix inversion 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  is.atomic(x)  
  set <- function(y){
    x <<- y
    m <- NULL
  }
  
  get <- function() x
  
  setInverse <- function(matrix_inverse) m <<- matrix_inverse
  
  getInverse <- function() m
  
  list( set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Function to invert a matrix using the "special martix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}

## Test
## Setup data
a1 <- c(3,6,1,9,4)
a2 <- c(2,3,7,8,2)
a3 <- c(5,8,9,3,1)
a4 <- c(6,4,6,7,6)
a5 <- c(3,7,4,1,8)

A <- rbind(a1,a2,a3,a4,a5)

b1 <- c(3,6,7,8,2)
b2 <- c(2,3,9,3,1)
b3 <- c(5,8,6,7,6)
b4 <- c(6,4,4,1,8)
b5 <- c(3,7,1,9,4)

B <- rbind(b1,b2,b3,b4,b5)

## Calculate the inverise and put in cache
myMtx = makeCacheMatrix(A)
A_cache <- cacheSolve(myMtx)
print(A_cache)

A_fetched <- cacheSolve(myMtx)
print(A_fetched)
print(cat("     "))

myMtx <- makeCacheMatrix(B)
B_cache <- cacheSolve(myMtx)
print(B_cache)

B_fetched <- cacheSolve(myMtx)
print(B_fetched)
