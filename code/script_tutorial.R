x <- c(1, 2)
x

y <- c(3, 4)
y

## produce 100 random numbers that follows a normal distribution
x <- rnorm(100, mean = 0, sd = 1)

## estimate mean
mean(x)

## estimate SD
sd(x)


# exercise 1 -------------------------------------------------------------

#create a vector with length 10 assign it to z 
z<-1:10
#z <- seq(1,10, length=10)
#z <- LETTERS[1:10]

#create a matrix with 2 rows x 2 columns, assign to m
m<- matrix(1:4,nrow=2, ncol=2)

#m<- matrix(c(1,2,3,4),nrow=2,ncol=2)
#m<- cbind(c(1,2),c(3,4))
#m<- rbind(c(1,2), c(3,4))

# dataframe
df0<- data.frame(c("Smith", "John", "Kate","Akira"), c(154,170,156,175))
df0


