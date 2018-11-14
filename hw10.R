# read in the data
solar <- read.table("Solar.txt", header = TRUE)
library(leaps)
attach(solar)
all <- regsubsets(x=cbind(x1,x2,x3,x4,x5), y=y,  method = "forward", all.best = FALSE)
summary(all)
all2 <- regsubsets(x=cbind(x1,x2,x3,x4,x5), y=y,  method = "backward", all.best = FALSE)
summary(all2)
# Forward variable selection
fit.0 <- lm(y~1, data = solar)
add1(fit.0, y~x1 + x2 + x3 + x4+x5, test = "F")
#added x4
fit.1 <- lm(y~x4, data = solar)
add1(fit.1,y~x1+x2+x3+x4+x5,test = "F")
#added x3
fit.2 <- lm(y~x4+x3,data = solar)
add1(fit.2,y~x1+x2+x3+x4+x5,test = "F")
#added x2
fit.3 <- lm(y~x4+x3+x2,data = solar)
add1(fit.3,y~x1+x2+x3+x4+x5,test = "F")
#added x1
fit.4 <- lm(y~x4+x3+x2+x1,data = solar)
add1(fit.4,y~x1+x2+x3+x4+x5,test = "F")

#backward variable selection
fit.4 <- lm(y~x1 + x2 + x3 + x4 + x5, data = solar)
drop1(fit.4, y~x1 + x2 + x3 + x4 + x5, test = "F")
#remove x5
fit.3 <- lm(y~x1 + x2 + x3 + x4 , data = solar)
drop1(fit.3, y~x1 + x2 + x3 + x4 , test = "F")
#remove x1
fit.2 <- lm(y~x2 + x3 + x4 , data = solar)
drop1(fit.2, y~x2 + x3 + x4 , test = "F")
#remove x2
fit.1 <- lm(y~ x3 + x4 , data = solar)
drop1(fit.1, y~ x3 + x4 , test = "F")

#transformed model

x5transformed <- (solar$x5-13.145)^2
solartransformed <- cbind(solar,x5transformed)
allforward <- regsubsets(data=solartransformed,x=cbind(x1,x2,x3,x4,x5transformed), y=y,  method = "forward", all.best = FALSE)
summary(allforward)
allbackward <- regsubsets(data=solartransformed,x=cbind(x1,x2,x3,x4,x5transformed), y=y,  method = "backward", all.best = FALSE)
summary(allbackward)

allexhustive <- regsubsets(data=solartransformed,x=cbind(x1,x2,x3,x4,x5transformed), y=y,  method = "exhaustive", all.best = T, nbest = 3)
Cp <- summary(allexhustive)$cp
AdjR2 <- summary(allexhustive)$adjr2
SSRes <- summary(allexhustive)$rss
R2 <- summary(allexhustive)$rsq
Matrix <- summary(allexhustive)$which
p <- apply(Matrix,1, sum)
MSE <- SSRes/(29-p)
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSE, Cp)
colnames(output)[3:7] <- c("x1", "x2", "x3", "x4","x5t") 
output

fit <- lm(y~x1+x2+x5transformed,data = solartransformed)
summary(fit)





cor(solartransformed[2:7])
coef(allexhustive,1:13)

model1 <- lm(y~x5transformed,data = solartransformed)
model2 <- lm(y~x1+x5transformed,data = solartransformed)
model3 <- lm(y~x1+x2+x5transformed,data = solartransformed)
par(mfrow=c(1,3))
qqnorm(rstudent(model1),main = "model 1")
abline(0,1)
qqnorm(rstudent(model2),main = "model 2")
abline(0,1)
qqnorm(rstudent(model3),main = "model 3")
abline(0,1)
par(mfrow=c(1,3))
plot(fitted.values(model1),rstandard(model1),main = "model 1")
plot(fitted.values(model2),rstandard(model2),main = "model 2")
plot(fitted.values(model3),rstandard(model3),main = "model 3")

# Forward variable selection
fit.0 <- lm(y~1, data = solartransformed)
add1(fit.0, y~x1 + x2 + x3 + x4 + x5transformed, test = "F")
#added x5transformed
fit.1 <- lm(y~x5transformed, data = solartransformed)
add1(fit.1,y~x1+x2+x3+x4+x5transformed,test = "F")

#backward variable selection
fit.5 <- lm(y~x1+x2+x3+x4+x5transformed, data = solartransformed)
drop1(fit.5, y~x1+x2+x3+x4+x5transformed, test = "F")
#remove x3
fit.4 <- lm(y~x1 + x2 + x4 +x5transformed , data = solartransformed)
drop1(fit.4, y~x1 + x2 + x4 +x5transformed , test = "F")
#remove x4
fit.3 <- lm(y~x1+x2+ x5transformed , data = solartransformed)
drop1(fit.3, y~x1 + x2 + x5transformed , test = "F")
#remove x2
fit.2 <- lm(y~  x1+x5transformed , data = solartransformed)
drop1(fit.2, y~ x1 + x5transformed , test = "F")
# remove x1
fit.1 <- lm(y~x5transformed,data = solartransformed)
drop1(fit.1,y~x5transformed,test="F")


# Stepwise variable selection 
fit.0 <- lm(y~1, data = solartransformed)
add1(fit.0, y~x1 + x2 + x3 + x4+x5transformed, test = "F")
#add x5transformed 
fit.1 <- lm(y~x5transformed, data = solartransformed)
drop1(fit.1, y~x5transformed, test = "F")
#no more dropping
#adding 
add1(fit.1, y~x1 + x2 + x3 + x4+x5transformed, test = "F")
# no further adding 
