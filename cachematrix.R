## OVerall I used two functions very similar the cachemean function provided as an example.  The first function, makeCacheMatrix, takes a matrix as an input and returns a list.  This list has cached data saved to it.
## The 2nd funtion, cachesolve, takes input list in the format of the output of makeCacheMatrix.  If the matrix has been previously tested the the cached inverse matric will be returned.  If this is a new matrix then the cachesolve will determine the inverse.



## makeCacheMatrix function follows.  This function takes an input matrix and returns a list. 
#1.  x is the input matrix, the default is an empty matix.
#2.  The first step is define the function set that sets the initial value of x and m.  The <<- operator means this information is called object in an environment that is different from an enviroment different from the current function.
#3.  The 2nd step defines the get function as makine equal to the input matrix
#4.  The next steps assign the inverse of the matrix to m
#5.  Lastly, the output list is created.  set data (x & m), get (input matrix), setsolve (the inverse), and getsolve (new cached m data)
#6.  By naming the objects in the list, $ can be used to select parts.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## cachesolve funtion takes an input object is the format of the ouptuf of makeCacheMatrix (a list).  If the original matrix has been previously tested then cached data is returned along with a note.  If not this functin calculates the inverse.
#1.  The first step is to determine if there is cached data.  If so a note and the cached data is returned
#2.  The next step retuns the inverse if not cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
}

##I used the following 4 matrices the test the data
matrix1 <- matrix(c(3, 3.2, 3.5, 3.6), 2, 2)
matrix1
test1<-makeCacheMatrix(matrix1)
test1inv<-cacheSolve(test1)
test1inv
matrix1%*%test1inv #verify the result is an inverse by multiplying the original matrix and the inverse
cacheSolve(makeCacheMatrix(matrix1))  #used a nested function to test

matrix2 <- matrix(c(4, 2, 7, 6), 2, 2)
matrix2
test2<-makeCacheMatrix(matrix2)
test2inv<-cacheSolve(test2)
test2inv
matrix2%*%test2inv
cacheSolve(makeCacheMatrix(matrix2))

matrix3 <- matrix(c(6,2,8,4), 2, 2)
matrix3
test3<-makeCacheMatrix(matrix3)
test3inv<-cacheSolve(test3)
test3inv
matrix3%*%test3inv
cacheSolve(makeCacheMatrix(matrix3))

matrix4<-matrix(c(1,0,5,2,1,6,3,4,0), 3,3)   #3 by 3 matrix
matrix4
test4<-makeCacheMatrix(matrix4)
test4inv<-cacheSolve(test4)
test4inv
matrix4%*%test4inv
cacheSolve(makeCacheMatrix(matrix4))


