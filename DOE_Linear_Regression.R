# p 475 . 10.1
# y: Tensile strength of a paper
# x: amount of hardwood

y<-matrix(c(160,171,175,182,184,181,188,193,195,200))
x<-matrix(c(10,15,15,20,20,20,25,25,28,30))

## Estimation of beta
# strength=beta_0+beta_1*percent_hardwood

# beta=(x'*x)^(-1)*x'*y
X=cbind(rep(1,10),x)
beta=solve(t(X)%*%X)%*%t(X)%*%y

fit<-lm(y~x)
# strength=143.824+1.879*percent_hardwood
# beta_0: when percent_hardwood is zero, then strength is 143.824
# beta_1: strength increases by 1.879 as percent_hardwood increases by a unit

summary(fit)
anova(fit)
plot(x,y,xlab='Percent_Hardwood',ylab='Strength')
abline(fit)

plot(fit,1)
plot(fit,2)
confint(fit)
