data <- read.table("", header=T)
names(data)
attach(data)
summary(data)
plot(y)

#Plotting against "index" can show outliers
y[50] <- 21.79386
plot(y)
#Which function can show which data point is the outlier
which(y>10)
y[50] <- 2.179386

#Boxplot is a visual representation of "summary" function
boxplot(y,ylab="data values")

#Histograms display data well
hist(y)
#For continuous data how to divide the x axis is often a problem, solved by using the length of the y table
length(table(y))
#Diff and range functions can be used to help identify where the bin denomination is
(max(y)-min(y))/10
diff(range(y))/11

#The Normal Distribution
hist(runif(10000)*10,main="")
#Make a histogram of the means
means <- numeric(10000)
for (i in 1:10000){
	means[i] <- mean(runif(5)*10)
}
hist(means,ylim=c(0,1600))
#Calculating the parameters of the normal distribution in this case
mean(means)
sd(means)
#Generating values to draw a normal curve
xv <- seq(0,10,0.1)
#Scaling the normal p.d.f to sample size
yv <- dnorm(xv,mean=4.997527,sd=1.286002)*5000
lines(xv,yv) 

#Standard Normal Distribution
nd <- seq(-3,3,0.01)
y <- dnorm(nd)
plot(nd,y,type="l")

#Calculating the probability that a value falls at a point = calculating area under the curve
pnorm(-2)
#Means less than 2.5% of values will be lower than -2
pnorm(-1)
#To get the probability that the mean will be MORE
1-pnorm(3)
#To reverse the calculations above we give a probability and want to find the associated deviate
qnorm(c(0.025,0.975))
