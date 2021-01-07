################ t-test ######################
# t.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 0, 
# paired = FALSE, var.equal = FALSE, conf.level = 0.95)

##### one sample t-test ######
ex) 
null: mu=0.3
alternative: mu>0.3

x = c(0.593, 0.142, 0.329, 0.691, 0.231, 0.793, 0.519, 0.392, 0.418)
t.test(x, alternative = "greater", mu = 0.3)


##### two sample t-test ######
null: mu_1-mu_2=0
alternative: mu_1-mu_2<0

ex) 
Control = c(91, 87, 99, 77, 88, 91)
Treat = c(101, 110, 103, 93, 99, 104)
t.test(Control,Treat,alternative="less", var.equal=TRUE) # pooled t-test
t.test(Control,Treat,alternative="less", var.equal=FALSE) # Welsh t-test


##### Paired t-test ######
reg = c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
prem = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
t.test(prem,reg,alternative="greater", paired=TRUE)



##### ANOVA ########
getwd() # check the current working directory
setwd("C:/Users/user/Google 드라이브/Lecture notes/DOE/2020") # change directory
data = read.csv("data2.csv", header = TRUE)
fit <- aov(y ~ factor(group), data=data)
aov.fit<-anova(fit)
## residual analysis ###
par(mfrow=c(1,2)) # layout of figure
plot(fit,1) # Check the homoscedasticity assumption
plot(fit,2) # Check the normality assumption

# Test of Equal variance
bartlett.test(y ~ factor(group), data = data)

# Extract the residuals
aov_residuals <-  residuals(object = fit)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

boxplot(y~factor(group),data=data)

### LSD test ###
install.packages("agricolae")
library(agricolae)
LSD<-LSD.test(fit,"factor(group)",group=F)

### Tukey test ###
TukeyHSD(fit,trt= "factor(group)", conf.level=0.95)

### Scheffe test ###
Sch<-scheffe.test(fit,"factor(group)", group=F)
