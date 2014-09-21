##Two functions are being defined; makeCacheMatrix() and cacheSolve()




##makeCacheMatrix will create a list of values namely:

##set, which will set the value of the matrix
##get, which will ouput the value of the matrix
##set_inverse, which sets the value of inverse of the matrix.
##get_inverse outputs the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  ##setting inverse to be NULL
  i <- NULL
  
  ##set function which will set the variable to be the given matrix
  set <- function(y){
    x <<- y
  }
  
  ##get function will output the matrix
  get <- function(){
    x
  }
  
  ##set_inverse function will store the given value inside the variable "i" as inverse
  set_inverse <- function(inverse){
    i <<- inverse
  }
  
  ##get_inverse function outputs the inverse
  get_inverse <- function(){
    i
  }
  
  ##finally, storing all the above variables with their values
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}



## cacheSolve will return the inverse of the matrix.

## If the inverse already exists, it will check and return that value.
## If it doesn't exist, it will find the inverse, store and return the value.

cacheSolve <- function(x, ...){
  ##storing inverse in the variable "i"
  i <- x$get_inverse()
  
  
  ##checking if "i" has a value. If it has a value, "i" is returned by the function.
  if(!is.null(i)){
    message("Returning inverse of the Matrix..")
    return(i)
  }
  
  
  ##setting the variable "mat" to  the value of the matrix
  mat <- x$get()
  
  ##getting inverse of "mat" by solve() function and storing it in "i"
  i <- solve(mat)
  
  ##storing the value of the inverse inside the vector/list
  x$set_inverse(i)
  
  ##outputing the inverse
  i
  
}
