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
    write.table(rbind(c(cur.time,Qt,Wt),deparse.level=0), file= paste("Documents/Tables/table", i," Lambda = ",lambda, " Max Time = ", maxTime/lambda, ".txt", sep = "" ), sep=" | ", quote=FALSE, append=FALSE, col.names=FALSE)
	
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
       write.table(rbind(c(cur.time,Qt,Wt),deparse.level=0), file= paste("Documents/Tables/table", i," Lambda = ",lambda, " Max Time = ", maxTime/lambda, ".txt", sep = "" ), sep=" | ", quote=FALSE, append=TRUE, col.names=FALSE)
    }
}

read.and.plot <- function( n, lambda, i )
{
	#plots Wt / Qt and gets rid of all Qt = 0
    maxTime = n/lambda
    
    C = 1
    
	Xt <- read.table( paste("Documents/Tables/table", i," Lambda = ",lambda, " Max Time = ", maxTime, ".txt", sep = "" ),
				colClasses = c("NULL","NULL","numeric","NULL","integer","NULL","numeric"),
				comment.char = "")
				
	Xt = subset(Xt, Xt[,2]!=0) #get rid of zeros in Qt


	Xt[,1] = Xt[,1]/n # scales time

	Xt[,3] = Xt[,3]/sqrt(n) # Wt

	Xt[,2] = (Xt[,2]*(log(sqrt(n)))/(C*sqrt(n))) # Qt   ,,,was log(sqrt(...))^(1/a)
	
	Xt[,4] = Xt[,3] / Xt[,2] # Wt / Qt
	

	xrange <- range(Xt[,1])
	yrange <- range(Xt[,4])

	png(filename = paste("Dropbox/SRPT/Sean/Graphs/plot ", i, ", C = ",C, ", Lambda = ", lambda, ", Max Time = ", maxTime ,".png", sep="" ),
    		width = 800, height = 600,
			units = "px")

	plot(x=Xt[,1],y=Xt[,4],type="S",xlim=xrange,ylim=yrange,col="green",xlab="Time",ylab="Wt,Qt")
	
	#points(x=Xt[,1],y=Xt[,3],type="s",col="red")

	title(main= bquote(atop("M | M | 1 | SRPT "* W(t) / Q(t) , lambda == .(lambda)*",  n = "*.(n)*", C = "*.(C)* ", mean = " * .( mean(Xt[,4]))* ", " * sigma* " = "* .( sd(Xt[,4])) )))
		
	#legend("topright",c("Wt","Qt"),col=c("red","green"),lwd=1)

	dev.off()
}
read.and.table <- function( n, lambda, i){
	
	#creates a table of mean and SD
    maxTime = n/lambda
    
    C = 1
    
	Xt <- read.table( paste("Documents/Tables/table", i," Lambda = ",lambda, " Max Time = ", maxTime, ".txt", sep = "" ),
				colClasses = c("NULL","NULL","numeric","NULL","integer","NULL","numeric"),
				comment.char = "")
				
	Xt = subset(Xt, Xt[,2]!=0) #get rid of zeros in Qt


	Xt[,1] = Xt[,1]/n # scales time

	Xt[,3] = Xt[,3]/sqrt(n) # Wt

	Xt[,2] = (Xt[,2]*(log(sqrt(n)))/(C*sqrt(n))) # Qt   ,,,was log(sqrt(...))^(1/a)
	
	Xt[,4] = Xt[,3] / Xt[,2] # Wt / Qt
	
	mu = mean(Xt[,4])
	
	sigma = sd(Xt[,4])
	
	write.table(rbind( c(lambda, mu, sigma)), file = paste("Dropbox/SRPT/Sean/Tables/Ratio Table lambda = ", lambda, ", n = ", n,".txt" , sep = ""), append=TRUE, sep=" ", quote=FALSE, row.names = FALSE, col.names= FALSE)
}

create.table <- function(n, lambda){
	
	#similar to create_graph
	i= 1
	while( i <= length(lambda)){
		mm1srpt(n , lambda[i], NULL)
		read.and.plot(n, lambda[i], NULL)
		i = i + 1
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
create.table(1000000,c(0.1,0.5,0.7,1,2,5,13))
