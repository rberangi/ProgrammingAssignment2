
## makeCacheMatrix is an R function that gets a matrix and
## returns a named list containing 3 functions:
## 	1. getMatrix= a function to get the matrix itself
## 	2. setInverse= A function  to update the cached matrix
##       inverse with the already computed inverse 
## 	3. getMatrixInverse= A function  to show the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
        Inversecache <- NULL
        getMatrix <- function() x 
        setInverse <- function(Inverse) Inversecache <<- Inverse
        getMatrixInverse <- function() Inversecache
        list(getMatrix = getMatrix, setInverse = setInverse,
		 getMatrixInverse = getMatrixInverse)

}


## cacheSolve is an R function that gets the list provided by
## the makeCacheMatrix function and
## 1. checks weather the matrix inverse exist
## 2. if not! computes the matrix inverse and updates the inverse cache

cacheSolve <- function(y) {
	## step 1: get the matrix inverse cache
 
      Inversecache <- y$getMatrixInverse ()

      if(!is.null(Inversecache )){
           message("Matrix cache exist and though returned")
           return(Inversecache)
    	     }
	else {
        	message("Inverse does not exist and to be copmuted")
        	Matrix <- y$getMatrix() 
        	Inversecache <- solve(Matrix) 
        	y$setInverse (Inversecache )  ## update the cache value
        	return(Inversecache )
    		}
}
