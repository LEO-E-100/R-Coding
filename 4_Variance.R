y <- c(13,7,5,12,9,15,6,11,9,7,12)
plot(y,ylim=c(0,20))
range(y)

#Sum of Squares Calculation
y-mean(y)
(y-mean(y))^2
sum((y-mean(y))^2)

#Using sum of squares to give Variance
#Variance = Sum of Squares / Degrees of Freedom

variance <- function(x) sum((x-mean(x))^2)/(length(x)-1)
variance(y)

var(y)

#A worked example (p.39)
ozone <- read.table("", header=T)
attach(ozone)
ozone

mean(gardenA)
gardenA-mean(gardenA)
(gardenA-mean(gardenA))^2
sum((gardenA-mean(gardenA))^2)							#Sum of Squares
sum((gardenA-mean(gardenA))^2)/(length(gardenA)-1)		#Variance

mean(gardenB)
gardenB-mean(gardenB)
(gardenB-mean(gardenB))^2
sum((gardenB-mean(gardenB))^2)						
sum((gardenB-mean(gardenB))^2)/(length(gardenB)-1)		

mean(gardenC)
gardenC-mean(gardenC)
(gardenC-mean(gardenC))^2
sum((gardenC-mean(gardenC))^2)							
sum((gardenC-mean(gardenC))^2)/(length(gardenC)-1)

#Carry out F-test to test difference between variance in gardenB and gardenC	
f.test<- var(gardenC)/var(gardenB)
f.test
#Look up probability of getting this value by chance if the two variances were really the same
#Cumulative probability of the F-distribution
#Test is two tailed
#pf function requires variance ratio, numerator df and denominator df
2*(1-pf(f.test,9,9))
#Value is less than 5% so it is possible to conclude a significant difference in the variances
var.test(gardenB, gardenC)


#Looking at Variance and Sample size
plot(c(0,30),c(0,15),type="n",xlab="Sample size",ylab="Variance")
for(df in seq(3,31,2)){
	for(i in 1:30){
		x <- rnorm(df,mean=10,sd=2)
		points(df,var(x))}}
#Shows that variance at low sample size is not hugely important

#Calculating the Standard Error of Means
sqrt(var(gardenA)/10)		
sqrt(var(gardenB)/10)
sqrt(var(gardenC)/10)

#Calculating Confidence Intervals from Student's t-distribution
qt(.025,9)	#(probability,df)
qt(.975,9)

qt(.975,9)*sqrt(var(gardenB)/10)	#ConfInt=t-value*std.err

#Bootstrapping
data <- read.table("", header=T)
attach(data)
names(data)
#Simulate sample sizes (k) between 5 and 30 taking 10,000 independent samples from data using "sample" function with replacement (replace=T)
plot(c(0,30),c(0,60),type="n",xlab="Sample size",ylab="Confidence interval")
for(k in seq(5,30,3)){
	a <- numeric(10000)
	for(i in 1:10000){
		a[i] <- mean(sample(values,k,replace=T))
	}
	points(c(k,k),quantile(a,c(.025,.975)),type="b")
}

#Bootstrapped CI based on 10,000 simulations 
quantile(a,c(.025,.975))

#Bootstrapped intervals compared with the intervals calculated from the Normal (solid line)
xv <- seq(5,30,0.1)
yv <- mean(values)+1.96*sqrt(var(values)/xv)
lines(xv,yv)
yv2 <- mean(values)-1.96*sqrt(var(values)/xv)
lines(xv,yv2)
#Students t-distribution (dotted line)
yv3 <- mean(values)-qt(.975,xv)*sqrt(var(values)/xv)
lines(xv,yv3,lty=2)
yv4 <- mean(values)+qt(.975,xv)*sqrt(var(values)/xv)
lines(xv,yv4,lty=2)




	