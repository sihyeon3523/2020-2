#PP. 225 5.4
# Pressure, Temperature, response=yield of chemical process
data<-as.data.frame(t(matrix(c(150, 200, 90.4,
150, 215, 90.7,
150, 230, 90.2,
150, 200, 90.2,
150, 215, 90.6,
150, 230, 90.4,
160, 200, 90.1,
160, 215, 90.5,
160, 230, 89.9,
160, 200, 90.3,
160, 215, 90.6,
160, 230, 90.1,
170, 200, 90.5,
170, 215, 90.8,
170, 230, 90.4,
170, 200, 90.7,
170, 215, 90.9,
170, 230, 90.1),3,18)))

data

names(data)[names(data) == "V1"] <- "Temperature"
names(data)[names(data) == "V2"] <- "Pressure"
names(data)[names(data) == "V3"] <- "y"
# colnames(data)<-c('Temperature','Pressure','y')

# a) Analyze the data and draw conclusions. use alpha=0.05
# numerical variable을 categorical로 바꾸기 위해 factor() 추가  
model<-aov(y~factor(Pressure)*factor(Temperature),data=data)
#model<-aov(y~factor(Pressure)+factor(Temperature)+factor(Pressure):factor(Temperature),data=data)

# summary: 각 variable에 대한 p-value 값이 나옴 
# 이 p-value를 바탕으로 각 factor가 유의미한지 아닌지를 판단 
summary(model)
# interaction effect is not significant, but main effects are significant
# we do not consider the interaction effect in this model 

#b) prepare appropriate residual plots and comment on the model's adequacy
# q-q plot: to check the normal assumption
# residual plot: to check the homoscedecity x: fitted value, y=residual value 

par(mfrow=c(2,2))
plot(model)
# => normal assumption, homoscedecity 둘 다 만족함을 확인할 수 있다. 

#c) Under what conditions would you operate this process?
# Which condition can we maximize the yield of the chemical process?
# 어떤 조건이 optimize the y를 할 수 있는 값이냐? 
interaction.plot(data$Pressure,data$Temperature,data$y)
# interaction.plot(x-axis, trace_factor, y-axis)
# combination of pressure=215, temperature=170 일 때 가장 높은 y 값을 가질 수 있다.(maximize the y)
# 세개의 그래프가 similar slope 를 show the similar slope, the slope is not significant different.
# there's no significant interaction effect between temperature and pressure 

# response curves and surfaces 
=> how to treat the quantitative factor(양적 변수, 수치형 변수)
when we have a quantitative factor, we can use the regression model to 
analyze the data 보통은 범주형 factor 이다. 이때는 regression model을 사용하지 않는다. 
input variabel이 양적 변수가 아니기 때문 ! 우리가 범주형 변수를 가지고 있을 경우 dummy variable로
변환할 수도 있다. 하지만 일반적으로 we do not use the regression model in the anova procedure
하지만 temperature, pressure은 수치형 변수이기 때문에 regression model 사용할 수 있다. 

interaction term is not significant, full model은 very close to reduced model (interaction term을 제외한)
interaction term이 없어지면 linear 한 형태가 된다. 하지만 interaction까지 고려하면 모델의 모양의 quadratic(curvature) 해진다. 

# 예: A: materical type(qualitative 범주), B:linear effect of temperature(quantitative 양적) 
일 때 , 수치형 변수 B를 제곱시켜주는 B^2(quadratic effect)까지도 고려해준다.
그래서 A,B, AB, AB^2, B^3(cubic effect of temperature) 를 고려해줌 
=> ANOVA 테이블로 각 factor의 유의미함을 판단한 뒤에 regression model을 만들어준다. 
(각 범주에 따라 다른 regression model이 만들어짐)

# Fit a Linear regression model 
# y~+beta_0+temp+pressure+temp*pressure+temp^2+pres^2
# Response surface analysis
Temp2<-data$Temperature^2
Pres2<-data$Pressu re^2

data2<-cbind(Temp2,Pres2,data)
model2<-lm(y~Temperature*Pressure+Temp2+Pres2,data=data2)
summary(model2)
# Yield=48.546 -0.6404*Temperature+0.8675926*Pressure+0.0024*Temp2-0.0018*Pres2 -0.00058*Temperature*Pressure

# HW 5.11, 5.24, 5.41, 10.1, 10.3 due date: Nov 30th