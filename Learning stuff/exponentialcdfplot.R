graphingexp <- function()
{
	vals <- seq(0, 10, length = 10000000)

	png(filename = "/Users/apuha/Dropbox/SRPT/Sean/Expcdf3.png",
    		width = 400, height = 400,
			units = "px")

	plot(vals,expcdf(vals),type="s",xlab="x",ylab="F(x)")

	title(main="Exponential Complementary Distribution Function")

	dev.off()
}
expcdf <- function(n)
{
	mu = 10
	value = (exp(1))^(-1 * mu * n)
	value
}
graphingexp()