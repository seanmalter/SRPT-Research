my_super_function <- function(n, prob_of_one){
	positive_ones = rbinom(1,n, prob_of_one)
	negative_ones = n - positive_ones
	return(positive_ones - negative_ones)
}

expected_value_discrete <- function(prob_of_one){
	expected_value = prob_of_one - (1- prob_of_one)
	#note this is the expected value of a discrete RND vairable with 1 and -1 as the two outcomes.
	
	return(expected_value)
}

stnd_dev_discrete <- function(expected_value){
	
	stnd_dev = sqrt( 1 - expected_value^2 )
	return (stnd_dev)
}

simpler_function <- function(n){
	
	#calcualtes the S(nt)/n from t = 0 , to 100
	
	y_values = 0 #initilize y
	
	time_vector = seq(0,100, by = 0.1) # creates a vector of lenght 1001, with 0 , 0.1 , 0.2, ... , 100 as the componets
	
	delta_time = 0.1
	
	i = 2 # start our i at 2 since y_values has something in the first place.
	
	while( i <= 1001){
		
		if( n * delta_time * i < 1){
			
			y_values[i]=0
			i = i+1
		}
		else{
			
			y_values[i] = my_super_function( floor( n * delta_time * i ) - floor( n * delta_time * ( i - 1)), 2/3)/n + y_values[( i - 1 )]
			i = i + 1
			}
		
	}
	
	plot(time_vector,y_values, type = "p")
	title(main = c("woot got something with n =  ", n))
}

something_is_wrong <- function(n){
	y_values = 0
	
	time_vector = seq(0, 100, by = 1)
	
	delta_time = 1
	
	i = 2
	
	while( i <= 101){
		
		if ( n * delta_time * (i-1) < 1){
			y_values[i] = 0
			i = i +1
		}
		else{
			y_values[i] = my_super_function( floor( n * delta_time * (i-1) ) - floor( n * delta_time * ( i - 2))) + y_values[ i-1 ] - (floor(n * delta_time * (i-1) ) / 3)
			i = i +1
		}
	}
	png(filename = "Dropbox/SRPT/Sean/Graphs/bad motion.png", width = 400, height = 400, units = "px")
	plot(time_vector,y_values, type = "p")
	title(main = c("woot got something with n =  ", n))
	dev.off()
}
brownian_motion <- function(n, prob_of_one){
	
	y_values = 0
	
	S_nt_values = 0
	
	time_vector = seq(0, 100, by = 0.1)
	
	delta_time = 0.1
	
	i = 2
	
	while( i <= 1001){
		
		if ( n * delta_time * (i-1) < 1){
			S_nt_values[i] = 0 + S_nt_values[i-1]
		}
		
		else{
			S_nt_values[i] = my_super_function( floor( n * delta_time * (i-1) ) - floor( n * delta_time * ( i - 2)), prob_of_one) + S_nt_values[i-1]
		}
		
		y_values[i] =  ( S_nt_values[i] - floor(n*delta_time*(i-1)) * expected_value_discrete(prob_of_one)) / ( sqrt(n)*stnd_dev_discrete( expected_value_discrete(prob_of_one) ))
		i = i +1
	}
	return(y_values)
	#plot(time_vector,y_values, type = "s")
}

brownian_graph <- function(n,prob_of_one_vector){
	
	#make the probabilty vector smaller than 4 just to keep it simple
	
	i = 2
	
	#plot.window(c(0,100),c(-30,30))
	
	plot(seq(0, 100, by = 0.1), brownian_motion(n,prob_of_one_vector[1]),type = "l")
	
	while(i <= length(prob_of_one_vector)){
		Y = brownian_motion(n,prob_of_one_vector[i])
		lines(seq(0, 100, by = 0.1), Y , col = i)
		i = i + 1
	}
}