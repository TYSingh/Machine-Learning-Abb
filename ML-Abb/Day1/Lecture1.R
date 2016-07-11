c(1,1,3,5)
c("India","UAE","USA")
vec1=c(1,3,"u")
class(vec1)
vec1[2]
# update second element in vec1
vec1[2]= "b"
vec1 <- "b"
vec1
a = data.frame(col1 = 1:4,col2=c(3,5,6,9))
a
colnames(a) = c("colA","colB")
a
m = matrix(c(3,4,-1,4,5,6),nrow=2,ncol=3,byrow=T)
m
class(m)
length(vec1)

##no. of col in m
dim(m)[2]
seq(1,10,by=2)
1:10
rnorm(10)
rnorm(10,-2,4)
set.seed(1)
rnorm(10)
#reading data from the file

data <- read.csv (file.choose(), header = T)    
data
head(data,10)
attach(data)
data[1:3, 4]
#How many observations has cylinders equal to 8
cars_8cyl = data[data$cylinders == 8,]
cars_8cyl
dim(cars_8cyl)
x = subset(data,cylinders == 8)
x
sum(data$cylinders ==  8)       # will give you the count
x = rnorm(100)
y = rnorm(100)
plot(x,y)
plot(x,y, xlab = " Iam Xaxis",ylab = "Iam Yaxis",col = 6, pch=4)

plot(cylinders,mpg, xlab ="Cylinders", ylab ="MPG")
# cylinders and mpg are numeric, so it will create a scatter plot
class(data$cylinders)
# converting the type of cylinder to categorical

cylinders = as.factor(cylinders)
plot(cylinders,mpg, xlab ="Cylinders", ylab ="MPG")
head(data)
var(cylinders == 6)
var(mpg[data$cylinders == 6])
var(mpg[data$cylinders == 4])

# barchart for different cars with different no. of cylinders we are looking at categorical variable
plot(cylinders)
# histogram
hist(mpg)
# using log will make it to normal distribution
hist(log(mpg))
pairs(data)
pairs(~mpg +weight+acceleration,data)
