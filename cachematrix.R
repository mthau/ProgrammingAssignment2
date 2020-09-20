##makes a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){ 
 inv <- NULL 
  set <- function(y){ 
  x <<- y 
   inv <<- NULL 
  } 
   get <- function() {x} 
   setInverse <- function(inverse) {inv <<- inverse} 
   getInverse <- function() {inv} 
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
 } 

  
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...){ 
     inv <- x$getInverse() 
  if(!is.null(inv)){ 
     message("getting cached data") 
       return(inv) 
     } 
     mat <- x$get() 
   inv <- solve(mat, ...) 
    x$setInverse(inv) 
    inv 
} 

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
makeCacheMatrix(m1)
cacheSolve(makeCacheMatrix(m1))
