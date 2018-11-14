b00 <- 0.2
b01 <- 0.3
b12 <- 0.4
t1 <- 5
t2 <- 10
par(mfrow = c(1, 1))
x <- seq(0,15,by=0.2)
s <- numeric()
for(i in 1:length(x)){
if(x[i] <= 5){
  s[i] <- b00+(b01*x[i])
  
}else if (x[i]  >5 & x[i] <= 10){
  s[i] <- b00+(b01*x[i])+(b12*(x[i]-t1)^2)
}else if (x[i] > 10){
  s[i] <- b00+(b01*x[i])+(b12*(x[i]-t1)^2)-(b12*(x[i]-t2)^2)
}
}
plot(s~x)


#question no :3
Data <- read.table("ChildSmoking.txt")
fit <-lm(fev~age,data = Data)
summary(fit)
plot(fitted.values(fit),rstudent(fit),xlab = "fitted values",ylab = "studentized residuals",main="scatter plot")
qqnorm(residuals(fit))
boxcox(fit)
BC <- boxcox(fit,lambda = seq(-2,10,1/10))
BC$x[BC$y==max(BC$y)]
y <- sqrt(Data$fev)
plot(y~Data$age,xlab="Age",ylab = "Square root of fev",main="scatter plot")

fit2 <-lm(I(sqrt(fev))~age,data = Data)
summary(fit2)
plot(fitted.values(fit2),rstudent(fit2),xlab = "fitted values",ylab = "studentized residuals",main="scatter plot")
qqnorm(residuals(fit2))

#
fit3 <- lm(I(sqrt(fev))~age+sex+smoke+(age*sex)+(age*smoke)+(sex*smoke)+(sex*smoke*age),data = Data)
summary(fit3)
fit4 <- lm(I(sqrt(fev))~age+sex+(age*sex),data = Data)
summary(fit4)
anova(fit3,fit4)

