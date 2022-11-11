## practice with R data
model <- glm(formula= vs ~ wt + disp, data=mtcars, family=binomial)
summary(model)

newdata = data.frame(wt = 2.1, disp = 180)
predict(model, newdata, type="response")


model_weight <- glm(formula= vs ~ wt, data=mtcars, family=binomial)
summary(model_weight)
range(mtcars$wt)
length(mtcars$wt) # 32
# [1] 1.513 5.424

xweight <- seq(0, 6, 0.01)
length(xweight) # 601
yweight <- predict(model_weight, list(wt = xweight),type="response")
plot(mtcars$wt, mtcars$vs, pch = 16, xlab = "WEIGHT (g)", ylab = "VS")
lines(xweight, yweight)
length(yweight)      


### plotting logistic regression

set.seed(24601) # setting this so the random results will be repeatable 

library(MASS)
covmat <- matrix(c(1.0,   0.2,   0.6, 
                   0.2,   1.0,  -0.5, 
                   0.6,  -0.5,   1.0), nrow=3) # the true cov matrix for my data
data <- mvrnorm(300, mu=c(0,0,0), Sigma=covmat) # generate random data that match that cov matrix
colnames(data) <- c("X1", "X2", "DV")
data <- as.data.frame(data)
data$group <- gl(n=3, k=ceiling(nrow(data)/3), labels=c("a", "b", "c", "d"))
# add some group differences and interaction stuff...
data$DV <- with(data, ifelse(group=="c" & X1 > 0, DV+rnorm(n=1, mean=1), 
                             ifelse(group=="b" & X1 > 0, DV+rnorm(n=1, mean=2) , DV)))
# make DV binary
data$DV <- ifelse(data$DV > 0, 1, 0)
head(data)

contrasts(data$group)

model <- glm(DV ~ (X1 + X2 + group)^2, 
             data=data, na.action="na.exclude",  family="binomial") 

summary(model)
model$coef
# save the coefficient values so we can use them in the equations

b0 <- model$coef[1] # intercept
X1 <- model$coef[2]
X2 <- -model$coef[3]
groupb <- model$coef[4]
groupc <- model$coef[5]
X1.X2 <- model$coef[6]
X1.groupb <- model$coef[7]
X1.groupc <- model$coef[8]
X2.groupb <- model$coef[9]
X2.groupc <- model$coef[10]

X1_range <- seq(from=min(data$X1), to=max(data$X1), by=.01)
X1_range

X2_val <- mean(data$X2) # by plugging in the mean as the value for X2,
#I'll be generating plots that show the relationship between X1 and the outcome 
# "for someone with an average X2".

a_logits <- b0 + 
  X1*X1_range + 
  X2*X2_val + 
  groupb*0 + 
  groupc*0 + 
  X1.X2*X1_range*X2_val + 
  X1.groupb*X1_range*0 + 
  X1.groupc*X1_range*0 + 
  X2.groupb*X2_val*0 + 
  X2.groupc*X2_val*0 # the reference group

b_logits <- b0 + 
  X1*X1_range + 
  X2*X2_val + 
  groupb*1 + 
  groupc*0 + 
  X1.X2*X1_range*X2_val + 
  X1.groupb*X1_range*1 + 
  X1.groupc*X1_range*0 + 
  X2.groupb*X2_val*1 + 
  X2.groupc*X2_val*0

c_logits <- b0 + 
  X1*X1_range + 
  X2*X2_val + 
  groupb*0 + 
  groupc*1 + 
  X1.X2*X1_range*X2_val + 
  X1.groupb*X1_range*0 + 
  X1.groupc*X1_range*1 + 
  X2.groupb*X2_val*0 + 
  X2.groupc*X2_val*1

# Compute the probibilities (this is what will actually get plotted):
a_probs <- exp(a_logits)/(1 + exp(a_logits))
b_probs <- exp(b_logits)/(1 + exp(b_logits))
c_probs <- exp(c_logits)/(1 + exp(c_logits))
#Plot time!
  # We'll start by plotting the ref group:
  plot(X1_range, a_probs, 
       ylim=c(0,1),
       type="l", 
       lwd=3, 
       lty=2, 
       col="gold", 
       xlab="X1", ylab="P(outcome)", main="Probability of super important outcome")


# Add the line for people who are in the b group
lines(X1_range, b_probs, 
      type="l", 
      lwd=3, 
      lty=3, 
      col="turquoise2")

# Add the line for people who are in the c group
lines(X1_range, c_probs, 
      type="l", 
      lwd=3, 
      lty=4, 
      col="orangered")

# add a horizontal line at p=.5
abline(h=.5, lty=2)

