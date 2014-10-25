makeCacheMatrix <- function (x = numeric(), y = integer(), z = integer()) {
        
        ## Create Local matrix
        mat <- NULL
        
        
        ## 1st CALL FUNCTION_SET: Create Matrix
        setmat <- function (a, b, c) {
                
                a <- x
                b <- y
                c <- z
                
                mat <<- matrix(a, b, c)
        
                tam <<- NULL
        }
        
        ## 2nd CALL FUNCTION_GET: Return Local Matrix
        getmat <- function() {
                
                return(mat)
        }
        
        ## 3rd CALL FUNCTION_SET: Solve for Inverse of Local Matrix; Assign to Cached Matrix
        settam <- function(tmp) {
                
                tmp <- mat
                
                tam <<- solve(tmp)
        }
        
        ## 4th CALL FUNCTION_GET: Return Inverse Matrix
        gettam <- function() {
                
                return(tam)
        }
        
        ## MAIN FUNCTION: Call & Set all Matrix variables Local and Cached
        list(setmat = setmat, getmat = getmat, settam = settam, gettam = gettam)
        
}


## FUNCTION: Return / Solve for inverse of Cached Matrix
cacheSolve <- function(x) {
        
        ## Search for Cached Inverse Matrix;
        tam <- x$gettam()
        
        ## IF !NOT(IS.NULL(tam) == TRUE);  
        if(!is.null(tam)) {
                
                ## PRINT message while GET cache
                message("Retrieving cached data.")
                ## Return Cached Inverse Matrix
                return(tam)
        }
        ## ELSE
        ## Create Local variable to store Local Matrix using call to  getmat() 
        data <- x$getmat()
        
        ## Create local variable to store Inverse of Matrix
        tam <- solve(data)
        
        ## Call to settam() to assign Inverse Matrix to Cached variable.
        x$settam(tam)
        
        ## Return Cached Inverse Matrix
        return(tam)
}


