exp_input <- function(mean){
	return(rexp(1, rate = 1/mean))
}

plot <- function(){
	upvar<-rnorm(10)+seq(1,1.9,by=0.1)
	downvar<-rnorm(20)*5+19:10
	par(mar=c(5,4,4,4))
	plot(6:15,upvar,pch=1,col=3,xlim=c(1,20),xlab="Occasion",ylab="",main="Dual ordinate plot")
	mtext("upvar",side=2,line=2,col=3)

}