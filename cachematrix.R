# there is a function makeCacheMatrix
#makeCacheMatrix consists of set,get,setmean,getmean
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                #initialise m as null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() (x)             #function to get matrix x
  setmean <- function(mean) (m <<- mean)
  getmean <- function() (m)                #function to get the inverse of the matrix
  list(set = set, get = get,setmean = setmean,getmean = getmean)
}

#this is used to get the cache data
cacheSolve <- function(x, ...) {           #gets cache data
  m <- x$getmean()
  if(!is.null(m)) {                     #checking wheather m is null
    message("getting cached data")
    return(m)                            #returns the value of m
  }
  data <- x$get()
  m <- solve(data, ...)                  #calculates m value
  x$setmean(m)
  m
}
