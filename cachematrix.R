source("cachematrix.R")

## The following 2 functions caches the inverse of 
## a matrix instead of calculating it repeatedyly.  

## MakeCacheMatrix is used to create a matrix object that can cache its inverse. 
## The inverse will be cached with the help of this matrix. 

makeCacheMatrix <- function(x = matrix()) {
  ## i will hold the Inverse of the matrix
  i<-matrix(list(), 2, 2)
  get<-function(){x}  ## get will run the function to retrieve the input matrix that has to be inversed
  setinverse<-function(inverse) {i<<-inverse} ##setinverse will store the calculated inverse matrix in i
  getinverse<-function(){i} ## getinverse will run the function to retrieve the value of the calculated inverse
  list(get = get, setinverse = setinverse, getinverse = getinverse) ## creating a list to store the values of the variables set in this function
  
} 

## cacheSolve will check if the inverse of the input matrix has already been calculated. 
## If it is, it will return cached data by outputting a message saying "Getting cached data". 
## If the matrix inverse is not yet available, then it will return the inverse. 


cachesolve <- function(x, ...) {
  i<-x$getinverse() ## i gets the calculated inverse of matrix 
  ## checks to see if the retrieved value of i is null or not. If not, it will return the inverse of the matrix
  data<-x[get] ## This function will calculate the inverse of the matrix
  if(!is.null(i)){  
    message("getting cached data")
    return(i)
  }
  data<-x$get() 
  i<-solve(x)
  x$setinverse(i)
  i
  }  
