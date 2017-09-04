mm1srpt_Error_Table <- function(maxTime, lambda, i)
{
	#lambda is the mean arrival rate and service rate
	# time goes from zero to 1, then n(time) goes from 0 to n.
	# i allows us to create multiple graphs with the same parameters 
    cur.time = 0
    Que_length = 0
    Wt = 0
    dAt = rexp(1, rate = lambda)
    dT = dAt
    Rt = vector(length=0)
    n = maxTime
    
    Error = 0
    time_of_error=0
    
    C = lambda
    
    while(cur.time < maxTime) {
      	cur.time = cur.time + dT
        if(dT == dAt) {
            if(Que_length > 0) {
                 Rt[1] = Rt[1] - dT
                 Wt = Wt - dT
            }
            Que_length = Que_length + 1
            S = rexp(1, rate = lambda)
            Wt = Wt + S
            Rt = append(Rt,S)
            Rt = sort(Rt,decreasing=FALSE)
            dAt = rexp(1, rate = lambda)
            dT = min(Rt[1],dAt)
        }
        else {
            dAt = dAt - dT
            Que_length = Que_length - 1
            Rt = Rt[-1]
            if (Que_length == 0) {
                  dT = dAt 
                  Wt = 0
            }
            else {
            	Wt = Wt - dT
                dT = min(Rt[1],dAt)
            }
       }
       
       #our guesses for Qt and Wt
       Qt = Que_length * (log(sqrt(n)))/sqrt(n)
       work_load = Wt * C / sqrt(n)
       
       if( Error < abs( work_load - Qt)){
       	Error = abs( work_load - Qt)
       	time_of_error = cur.time/n
       }
      }
      write.table(rbind( c(n, lambda, Error, time_of_error)), file = paste("Dropbox/SRPT/Sean/Tables/Table lambda = ", lambda, ", n = ", n,".txt" , sep = ""), append=TRUE, sep=" ", quote=FALSE, row.names = FALSE, col.names= FALSE)
      
}

create_table <- function( n, lambda, N){
	# creates N entries with parameters n and lambda
	i = 1
	while( i <= N){
		mm1srpt_Error_Table( n, lambda, i)
		i = i + 1
	}
}
create_table(100000, 0.3, 100)
create_table(1000000, 0.3, 100)
create_table(10000000, 0.3, 100)