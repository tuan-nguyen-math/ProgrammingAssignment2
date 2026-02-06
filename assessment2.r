

makeMatrix <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}



cacheinv <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

# 1. Create a simple invertible matrix
my_mat_object <- makeMatrix(matrix(c(1, 2, 3, 4), 2, 2))

# 2. First run: Calculates the inverse
print(cacheinv(my_mat_object))

# 3. Second run: Retrieves from cache (much faster)
print(cacheinv(my_mat_object))
# Output: 
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5












