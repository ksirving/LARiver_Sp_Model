## r tutorials

# Add a Normal Curve 
# x <- mtcars$mpg
# h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon",
#         main="Histogram with Normal Curve")
# xfit<-seq(min(x),max(x),length=40)
# yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
# yfit <- yfit*diff(h$mids[1:2])*length(x)
# lines(xfit, yfit, col="blue", lwd=2)
# yfit
# mtcars$mpg

# Kernel Density Plot
d <- density(mtcars$mpg) # returns the density data
plot(d) # plots the results

# Compare MPG distributions for cars with
# 4,6, or 8 cylinders
install.packages("sm")
noyes
library(sm)
attach(mtcars)

# create value labels
cyl.f <- factor(cyl, levels= c(4,6,8),
                labels = c("4 cylinder", "6 cylinder", "8 cylinder"))

# plot densities
sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")
title(main="MPG Distribution by Car Cylinders")

# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f))))
# legend(locator(1), levels(cyl.f), fill=colfill)


require(graphics)

dnorm(0) == 1/sqrt(2*pi)
dnorm(1) == exp(-1/2)/sqrt(2*pi)
dnorm(1) == 1/sqrt(2*pi*exp(1))

## Using "log = TRUE" for an extended range :
par(mfrow = c(2,1))
plot(function(x) dnorm(x, log = TRUE), -60, 50,
     main = "log { Normal density }")
curve(log(dnorm(x)), add = TRUE, col = "red", lwd = 2)
mtext("dnorm(x, log=TRUE)", adj = 0)
mtext("log(dnorm(x))", col = "red", adj = 1)

plot(function(x) pnorm(x, log.p = TRUE), -50, 10,
     main = "log { Normal Cumulative }")
curve(log(pnorm(x)), add = TRUE, col = "red", lwd = 2)
mtext("pnorm(x, log=TRUE)", adj = 0)
mtext("log(pnorm(x))", col = "red", adj = 1)

## get actual probability

pp <- function(x) {
  print(paste0(round(x * 100, 3), "%"))
}
# likelihood of depth=50?
pp(pfit_df$density[pfit_df$Depth == 50])


plot(Year, Sale, main="Scatterplot", xlab="Year ", ylab="Number of Book That Is Sold", pch=19)

set.seed(0)
x <- rnorm(1000)
d <- density.default(x, n = 512, cut = 3)
str(d)
#    List of 7
# $ x        : num [1:512] -3.91 -3.9 -3.88 -3.87 -3.85 ...
# $ y        : num [1:512] 2.23e-05 2.74e-05 3.35e-05 4.07e-05 4.93e-05 ...
# ... truncated ...
# We want to compute the area under the curve to the right of x = 1:
  
  plot(d); abline(v = 1, col = 2)
  
  
  xx <- d$x  ## 512 evenly spaced points on [min(x) - 3 * d$bw, max(x) + 3 * d$bw]
  dx <- xx[2L] - xx[1L]  ## spacing / bin size
  yy <- d$y  ## 512 density values for `xx`
  
  C <- sum(yy) * dx  ## sum(yy * dx)
  C
  # [1] 1.000976
  # Since Riemann Sum is only an approximation, this deviates from 1 (total probability) a little bit. We call this C value a "normalizing constant".
  # 
  # Numerical integration on [1, Inf] can be approximated by
  
  p.unscaled <- sum(yy[xx >= 1]) * dx
  p.unscaled
  # [1] 0.1691366
  # which should be further scaled it by C for a proper probability estimation:
    
    p.scaled <- p.unscaled / C
  # [1] 0.1689718
  # Since the true density of our simulated x is know, we can compare this estimate with the true value:
    
    pnorm(x0, lower.tail = FALSE)
  # [1] 0.1586553
  
  
  
  #Reproducible sample data 
  set.seed(0)
  x <- rnorm(1000)
  
  #Create empirical cumulative distribution function from sample data
  d_fun <- ecdf (x)
  d_fun
  ?ecdf
  #Assume a value for the "red vertical line"
  x0 <- 1
  
  #Area under curve less than, equal to x0
  d_fun(x0) 
  # [1] 0.837
  
  #Area under curve greater than x0
  1 - d_fun(x0)
  # [1] 0.163
  
  df<- data.frame(x=c(sample(6:9, 50, replace=TRUE), sample(18:23, 25, replace=TRUE)))
  
  dens<- density(df$x)
  
  integrate(approxfun(dens), lower=3, upper=7)
  0.258064 with absolute error < 3.7e-05
  
  ## Consistency check
  integrate(approxfun(dens), lower=0, upper=30)
  0.9996092 with absolute error < 1.8e-05
  
  
  
  # Children's IQ scores are normally distributed with a
  # mean of 100 and a standard deviation of 15. What
  # proportion of children are expected to have an IQ between
  # 80 and 120?
  
  mean=100; sd=15
  lb=80; ub=120
  
  x <- seq(-4,4,length=100)*sd + mean
  hx <- dnorm(x,mean,sd)
  
  plot(x, hx, type="n", xlab="IQ Values", ylab="",
       main="Normal Distribution", axes=FALSE)
  
  i <- x >= lb & x <= ub
  lines(x, hx)
  polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")
  
  area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
  result <- paste("P(",lb,"< IQ <",ub,") =",
                  signif(area, digits=3))
  mtext(result,3)
  axis(1, at=seq(40, 160, 20), pos=0)
