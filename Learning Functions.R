for (i in 1:10){
	print (i*i)
}

one.to.six <- c("one", "two", "three", "four", "five", "six")
for (word in one.to.six){
	if (nchar(word)==3){
		print(word)
	}
}

squares <- function (n){
	s <- vector('numeric')
	for (i in 1:n){
		s[i] <- i*i
	}
	return(s)
}