yvals <- read.table("", header=T)
attach(yvals)
hist(y)
total <- sum(y)
n <- length(y)
ybar <- total/n
ybar

# Function for the arithmetic mean
arithmetic.mean <- function(x){
	sum(x)/length(x)}
arithmetic.mean

data <- c(3,4,6,7)
arithmetic.mean(data)

arithmetic.mean(y)

mean(y)

sorted <- sort(y)		#Calculating the median of a dataset with odd number of values
length(y)/2
ceiling(length(y)/2)
sorted[20]
sort(y)[ceiling(length(y)/2)]

#Calculating the median of a dataset with even number of values
y.even <- y[-1]	#Remove the first element from vector
length(y.even)
(sort(y.even)[length(y.even)/2]+sort(y.even)[1+length(y.even)/2])/2

#Use of 'modulo' function
38%%2
39%%2

#General function to calculate medians
med <- function(x){
	odd.even <- length(x)%%2
	if(odd.even==0)(sort(x)[length(x)/2]+sort(x)[1+length(x)/2])/2
	else sort(x)[ceiling(length(x)/2)]
}

med(y)
med(y.even)

median(y)
median(y.even)

#Hand calculating geometric means
100000^0.2
insects <- c(10,1,1000,1,10)		#Example of arithmetic mean
mean(insects)

#Geometric mean calculation using logarithms
exp(mean(log(insects)))	#Use the antilog (exp)


#Calculating the harmonic mean
v <- c(1,2,4,1)
length(v)/sum(1/v)

1/mean(1/v)	# Alternatively this is possible


detach(yvals)







