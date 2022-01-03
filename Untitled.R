#1 class reference
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


my_matrix1 = matrix(1:16, ncol = 4, nrow =4)

#Assignment
#1#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  set <- function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function()x
  setinverse<-function(invmatrix) inverse<<-invmatrix
  getinverse<-function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}





#2:cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...){
  inverse <-x$getinverse()
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data<-x$get()
  inverse <-solve(data,...)
  x$setinverse(inverse)
  inverse
}

my_matrix1 = matrix(1:16, ncol = 4, nrow =4)
makeCacheMatrix(my_matrix1)
cacheSolve(my_matrix1)

#given reference in course
#2

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

