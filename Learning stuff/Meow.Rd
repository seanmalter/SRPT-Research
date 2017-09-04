Xi <- function(){ 
	
	# randomly assigns a value to X_i based on the discrete probability given
	
	x=rbinom(1,1,2/3)
	if(x==1){
		return(1)
	}
	else {
		return(-1)
	}
}

St <- function(t){
	
	# calcualtes the value of S(t) for some value of t.
	
	if(t<1){
		return(0)
	}
	
	i=1     # iteration vairable
	s = 0   # running total for our S(t)
	while( i <= t ){
		s = s + Xi()
		i = i + 1
	}
	return(s)
	
}

plot_St <- function(start_time, end_time, number_of_points, n){
	
	#plots bar S^(n) (t)
	
	time_step = (end_time - start_time) / number_of_points
	
	time_vector = seq(start_time, end_time , by=time_step)
	
	i = 1  #iteration vairable
	
	y_values = vector(length=0)   # this is our vector of outcomes of S(nt)/n
	
	while( i <= number_of_points){
		
		time = start_time + time_step * i
		
		if( n * time < 1){
			
			# adds a zero to our outcome if n*t < 1
			y_values = c( y_values ,0)
			i = i+1
		}
		else{
			if(length( y_values )==0){
				
				# initilizes the first value so we can use recursion
				
				y_values[ i ] = St(n*start_time) / n
				i = i + 1
			}
			else{
				y_values[ i ] = St( floor(n* time) - floor( n * (time - time_step)))/n + y_values[ (i-1) ]
				i = i + 1
			}
		}
	}
	time_vector = time_vector[-1]
	
	#png(filename = "Dropbox/SRPT/Sean/Graphs/wowzer.png",
	#width = 400, height = 400, units = "px")
	
	plot(time_vector, y_values, type = "l")
	
	#dev.off()
}