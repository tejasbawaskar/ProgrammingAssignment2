#Two functions are created. 'makeCacheMatrix' creates a cache inverse and has 4
#different functions, a list and two seperate objects. 'cacheSolve' checks for  
#cached inverse matrix and displays the result if it has been earlier stored. If
#not, it later solves it and stores it in the list in the 'makeCacheMatrix' function.

# Function is described on each line

#here the function is defined as a matrix data type, which will be useful with the 
#help of lexical scoping
makeCacheMatrix <- function(x = matrix()) {  
    inv <- NULL #object is initialized in the makeCacheMatrix() environment 
    set <- function(y) { #function is used to set the values in an object
      x <<- y #matrix is stored in an object in the parent environment
      inv <<- NULL #object is nullified to overwrite any previous data that was 
                   #cached in the memory
    }
    get <- function() x #x is retrieved from from the makeCacheMatrix environment 
                        #and stored in 'get'
    setinv <- function(e) inv <<- solve(x)#the inverse of the matrix is solved 
                                  #and stored in 'inv' in the parent environment
    getinv <- function() inv #inverse is retrieved from the parent 
                            #environment and stored in 'getinv'
    list(set= set,get = get,#list created has functions assigned as elements in it
         setinv = setinv,
         getinv = getinv)
}


# Function is described on each line

#To access the makeCacheMatrix environment, it should be of the same type
cachesolve <- function(x, ...) { 
  inv <- x$getinv() #using lexical scoping we store the inverse of x in inv from 
                    #the 'makeCacheMatrix' environment
  if(!is.null(inv)) {#if the value in 'inv' isn't null, the function is executed 
                    #further
    message("getting cached data")#gives a message to the reader
    return(inv) # the cached inverse of the matrix is returned
  }
  data <- x$get() #if the inverse isn't cached, it is stored in the object data 
                  #and retrieved from the list within element get
  inv <- solve(data, ...)#the inverse of the matrix is stored in inv
  x$setinv(inv)#using lexical scoping we set the inverse for that matrix in 
               #setinv
  return(inv) #the inverse matrix is returned
}