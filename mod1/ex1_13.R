a <- c(10,30,10)
b <- c(40,30,30)
c <- c(5,0,5)
d <- c(1,2,6)
A <- matrix(c(a,b,c,d),nrow=4,byrow=T)
b <- c(1200,75,55)
x <- solve(A[2:4,],b)
A[1,]%*%x
