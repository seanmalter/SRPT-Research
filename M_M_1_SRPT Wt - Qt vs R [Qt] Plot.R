mm1srpt <- function(maxTime, lambda, i)
{
	#lambda is the mean arrival rate and service rate
	# time goes from zero to 1, then n(time) goes from 0 to n.
	# i allows us to create multiple graphs with the same parameters 
    cur.time = 0
    Qt = 0
    Wt = 0
    dAt = rexp(1, rate = lambda)
    dT = dAt
    Rt = vector(length=0)
    Max_Res_Service_Time = 0
    write.table(rbind( c( cur.time, Qt, Wt, Max_Res_Service_Time),deparse.level=0), file= paste("Documents/Tables/table", i," Lambda = ",lambda, " Max Time = ", maxTime/lambda, ".txt", sep = "" ), sep=" | ", quote=FALSE, append=FALSE, col.names=FALSE)
	
    while(cur.time < maxTime/lambda) {
      	cur.time = cur.time + dT
        if(dT == dAt) {
            if(Qt > 0) {
                 Rt[1] = Rt[1] - dT
                 Wt = Wt - dT
            }
            Qt = Qt + 1
            S = rexp(1, rate = lambda)
            Wt = Wt + S
            Rt = append(Rt,S)
            Rt = sort(Rt,decreasing=FALSE)
            #Max_Res_Service_Time = Rt[length(Rt)]
            dAt = rexp(1, rate = lambda)
            dT = min(Rt[1],dAt)
        }
        else {
            dAt = dAt - dT
            Qt = Qt - 1
            Rt = Rt[-1]
            if (Qt == 0) {
                  dT = dAt 
                  Wt = 0
            }
            else {
            	Wt = Wt - dT
                dT = min(Rt[1],dAt)
            }
       }
       if(length(Rt)==0){
       	Max_Res_Service_Time = 0
       }
       else{
       	Max_Res_Service_Time = Rt[length(Rt)]
       	}
       write.table(rbind(c(cur.time,Qt,Wt, Max_Res_Service_Time),deparse.level=0), file= paste("Documents/Tables/table", i," Lambda = ",lambda, " Max Time = ", maxTime/lambda, ".txt", sep = "" ), sep=" | ", quote=FALSE, append=TRUE, col.names=FALSE)
    }
}

read.and.plot <- function( n, lambda, i )
{
    maxTime = n/lambda
    
    C = lambda
    
	Xt <- read.table( paste("Documents/Tables/table", i," Lambda = ",lambda, " Max Time = ", maxTime, ".txt", sep = "" ),
				colClasses = c("NULL","NULL","numeric","NULL","integer","NULL","numeric","NULL","numeric"),
				comment.char = "")

	Xt[,1] = Xt[,1]/n # scales time

	Xt[,3] = Xt[,3]/sqrt(n) # Wt

	Xt[,2] = (Xt[,2]*(log(sqrt(n)))/(C*sqrt(n))) # Qt
	
	Xt[,4] = Xt[,4]/sqrt(n)  # R[ Qt ] / sqrt(n)
	
	Xt[,5] = Xt[,3] - Xt[,2] # Wt - Qt at time t

	xrange <- range(Xt[,1])
	yrange <- range(c(Xt[,4],Xt[,5]))

	png(filename = paste("Dropbox/SRPT/Sean/Graphs/plot ", i, ", C = ",C, ", Lambda = ", lambda,", n = ", n,".png", sep="" ),
    		width = 800, height = 600,
			units = "px")

	plot( x=Xt[,1], y=Xt[,5], type="S", xlim=xrange, ylim=yrange, col="green", xlab="Time", ylab="Wt,Qt")
	points(x=Xt[,1],y=Xt[,4],type="s",col="red")

	title(main= bquote(atop("M | M | 1 | SRPT", lambda == .(lambda)*", n = "*.(n)*", C = "*.(C)*", Max( Abs( Wt - Qt)) = "*.(max(abs(Xt[,5]))) )))
	
	legend("topright",c("R[Qt]","Wt - Qt"),col=c("red","green"),lwd=1)

	dev.off()
}

create_graph <- function(n,lambda){
	
	#takes a fixed n, and a vector of lambdas and creates graphs with the different lambdas
	
	i= 1
	while( i <= length(lambda)){
		mm1srpt(n , lambda[i], NULL)
		read.and.plot(n, lambda[i], NULL)
		i = i + 1
	}
}

multi_graphs <- function( n, lambda, N){
	
	# creates N graphs with parameters n and lambda
	i = 2
	while( i <= N){
		mm1srpt( n, lambda, i)
		read.and.plot( n, lambda, i)
		i = i + 1
	}
}
create_graph(100000,c(2,2.3,3,4,5,6,7,8,9,15,15.1,15.2,0.1,0.2,0.3,0.4,0.45,0.47,0.01))