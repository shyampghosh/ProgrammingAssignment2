## makeCacheMatrix: sets and returns the value of the underlying
	## matrix vector as well as the inverse matrix
##cacheSolve : Computes the inverse of a matrix and returns the value

makeCacheMatrix <- function(matData = matrix()) {

##Set the value of the matInv as NULL when the makeCacheMatrix is used
	##to initialise any vector for the first time

	matInv<- NULL


##Setting the value of the original matrix, matData
	set<- function(newData){
		
	##setting the changed matrix value
		matData <<- newData		
		
	##Cached inverse matrix no longer valid since the matrix value changes
		matInv <<- NULL
	}

##Getting the value of the original matrix, matData
	get<- function(){
		
	## return the original matrix, matData
		return(matData)
	}

## setting the matInv vector to the value passed as argument to the function
	setInverse<- function(inverse){
		matInv<<- inverse
	}

##return the cached value of inverse matrix when getInverse function called
	getInverse<- function(){
		return(matInv)
	}

##Create a vector of the makeCacheMatrix type
	makeCacheMatrixObj<- list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

##return the makeCacheMatrixObj when makeCacheMatrix type vector is initialised
	##Elements of makeCacheMatrix function not passed within makeCacheMatrixObj
	##cannot be accessed outside the makeCacheMatrix function
	return(makeCacheMatrixObj)
}




## compute the inverse of the matrix after checking if inverse of matrix 
##already cached or not

cacheSolve <- function(matObj, ...) {

##get the inverse of matrix as stored. May be Null or have valid data
	##depending on the prior steps used
	
	matInv<- matObj$getInverse()

##If cached data of inverse matrix exists
	if(!(is.null(matInv))){
		print("Cached value exists. Fetching...")
		return(matInv)
	##Control passes out of the function
	}

##If cached version doesn't exist
	
##Get the underlying vector matrix from the makeCacheMatrix 
	data<- matObj$get()

##Calculate the inverse matrix and keeping provision to pass any other 
	##arguments passed to cacheSolve() to the solve function
	matInv <- solve(data,...)

##set the inverse of the matrix in the cache by using the setInverse function
	##So that we don't need to calculate it again for same matrix vector
	matObj$setInverse(matInv)

	return(matInv)
}
