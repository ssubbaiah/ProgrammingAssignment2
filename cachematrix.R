###################################################################################
#Prog Name          Author         Date       Description
#cachematrix.R		Som Subbaiah - 5/23/2015 - R Programmign - Week 3 
###################################################################################

####################################################################################
## makecachematrix is function that returns functions as result
## This function returns four other functions
##  set - Set the matrix
##  get - returns the matrix saved
##  setinverse - caches inverse of the matrix
##  getinverse - returns the cahced inverse of the matrix
###################################################################################
###### NOTE
######    Inverse works only on square matrices
###################################################################################

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
    
# Set the initial cache value to NULL    
    cacheinv <- NULL

# Set/Store the matirx    
    set <- function (y) {
        x <<- y
        cacheinv <<- NULL
    }
    
# return the stored matrix
    get <- function() x
    
# store the inverse for future use
    setinverse <- function (inverse) cacheinv <<- inverse

# return the stored inverse
    getinverse <- function() cacheinv
    
# set the return values (in this case list of functions) of the function makeCahceMatrix
    list (
        set = set,
        get = get, 
        setinverse = setinverse,
        getinverse = getinverse
    )
    
} #makecachematrix


#############################################################################
## cacheSolve - Creates inverse of a stored matrix
## A matrix is created using makeCacheMatrix, which then will be passed as an
## argument to cacheSolve
#############################################################################
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

#  Call the function getinverse to get stored inverse matrix    
    inv <- x$getinverse()    
#   Check the inverse obtained in the previous step - if it is not null
#   then we got the inverse created earlier - no need to recalculate
#   so print a message and return inverse to the caller
    if  (!is.null(inv)) {
        message("Cached inverse exists is being returned")
        return (inv)
    } # if

  
# We are here - because the inverse got earlier is null
   message("No cache exists, computing inverse")
#  Get the (stored) matrix
    m<-x$get()

# call solve function to get inverse
    inv<-solve(m,...)

# Store/Cache the inverse for future use 
    x$setinverse(inv)

#  return the inverse
    inv
} # cacheSolve


##########################################################################
## Testing this function
## Step 1  - Create a matrix by calling makeCacheMatrix
##    m <- makeCacheMatrix(matrix(c(5, 0,0,5), c(2, 2)))
## Step 2 - Call cacheSolve  - being first call inverse will be computed
##     cacheSolve(m)
## Output - Begin
##     No cache exists, computing inverse
##       [,1] [,2]
##  [1,] 0.2  0.0
##  [2,] 0.0 0.2
## Output - End
##
## Step 3 - Call cacheSolve again  - being second call cached inverse will be returned
##     cacheSolve(m)
## Output - Begin
##     Cached inverse exists is being returned
##       [,1] [,2]
##  [1,] 0.2  0.0
##  [2,] 0.0  0.2
## Output - End
##
#####################################################################


####################### End of cachematrix.R ################################