transformid <- function(n){
 if (n<10){n <- paste("00",n, sep ="")}
 else if (n<100 & n > 9){n <- paste("0",n, sep ="")}  
 n
}
