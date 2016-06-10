#this function defines the recurrence relation I will mostly be using (logistic map)
F<-function(x,r){
    r*x*(1-x)
}

#this function will take a recurrence relation and use it to plot both a
#cobweb diagram and also how the population changes over time
cobweb.plot<-function(F,r, gen, x0, xmax=1){
                                        #those inputs are F, the recurrence relation function
                                        #r, the value of the parameter r
                                        #gen, the number of generations to run 
                                        #x0, the starting value for the population size
                                        #xmax, for plotting purposes, the upper limit of the x-axis range. 											#This is set
                                        #to have a default value of 1, so won't need to assign a value to it 										#when
                                        #you run the program

                                        #set plotting parameters so two plots are shown alongside one 												#another in the window
    par(mfrow=c(1,2))
                                        #range of x-values for plotting the curve - a sequence of length 1000 from 0 to 1
    x<-seq(0,xmax,length.out=1000)
                                        #find the corresponding y values (next gen) for this sequence of x values
    y<-F(x,r)
                                        #and plot y against x
    plot(x,y,type="l",xlab="Current popn size",ylab="Popn size in next generation",ylim=c(0,1))
                                        #the function points adds points or lines to an existing plot.
                                        #add the line y=x
    points(x,x,type="l")
                                        #set up a (currently empty) vector which will contain a the popn size in each generation
                                        #and then fill in the first couple of values already
    pop.size<-vector('numeric')
    pop.size[1]<-x0
    pop.size[2]<-F(x0,r)
                                        #plot the first couple of lines of the cobweb
    points(c(x0,x0),c(0,F(x0,r)),type="l",col="red")
    points(c(x0,F(x0,r)),c(F(x0,r),F(x0,r)),type="l",col="red")

                                        #now for each following generation up until the last generation that we entered,
                                        #we want to draw the lines up from the current point on y=x to the logistic map curve
                                        #and then across from the logistic curve to y=x
                                        #we can write a 'for loop' to do this: for each value of i from 2 to gen the code inside
                                        #the brackets is implemented sequentially
                                        #points(c(x1,x2),c(y1,y2),type="l") will plot a straight line from (x1,y1) to (x2,y2)
    for (i in 2:gen){
        points(c(pop.size[i],pop.size[i]),c(pop.size[i],F(pop.size[i],r)),type="l",col="red")
        points(c(pop.size[i],F(pop.size[i],r)),c(F(pop.size[i],r),F(pop.size[i],r)),type="l",col="red")
                                        #calculate and record the value of the population size in the next generation
        pop.size[i+1]<-F(pop.size[i],r)
                                        #this line causes R to pause for 0.05 secs, so that you can see the cobweb growing
        Sys.sleep(0.05)
    }
                                        #plot the population size in each generation in a separate plot
    plot(1:(gen+1),pop.size, type="l",xlab="Generation",ylab="Population size")
    cat("Population size in last 10 generations is:", pop.size[(gen-9):gen], "\n")
}
