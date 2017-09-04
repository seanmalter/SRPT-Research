mw1srpt <- function(a=1, lambda = 1, maxTime=10000)
{ 
    b = 1/gamma(1+1/a)
    cur.time = 0
    Qt = 0
    Wt = 0
    dAt = rexp(1,rate = lambda)
    dT = dAt
    Rt = vector(length=0) 
    Qtt = c(cur.time,Qt,Wt)
    Qtt = rbind(Qtt,deparse.level=0)
    colnames(Qtt) = c("Time","Qt","Wt")
    
    #cat("Time=", cur.time,
    #   ", Qt=", Qt,
    #   ", Wt=", Wt,
    #   ", dT=", dT,
    #   ", dAt=", dAt, 
    #   "\n", sep="")
    #cat("Rt=")

	while(cur.time < maxTime) {
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
       Qtt = rbind(Qtt,c(cur.time,Qt,Wt),deparse.level=0)
    
        #cat("Time=", cur.time,
        #   ", Qt=", Qt,
        #   ", Wt=", Wt,
        #   ", dT=", dT,
        #   ", dAt=", dAt, 
        #   "\n", sep="")
        #cat("Rt=")
        #print(format(Rt,digits=5))
    }
    Qtt
}
Xt = mw1srpt()
yrange <- range(c(Xt[,2],Xt[,3]))
plot(Xt[,c(1,2)], type="s", ylim=yrange, col="green", xlab="Time", ylab="Wt,Qt")
points(Xt[,c(1,3)],type="s",col="red")
title(main="SRPT M/W/1 with a=",a)
legend("topright",c("Qt","Wt"),col=c("green","red"),lwd=1)