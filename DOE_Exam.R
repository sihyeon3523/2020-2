data1<-as.data.frame(t(matrix(c(1*10^20, 900, 4.60,
1*10^20, 950, 10.15,
1*10^20, 1000, 11.01,
1*10^20, 900, 4.40,
1*10^20, 950, 10.20,
1*10^20, 1000, 10.58,
2*10^20, 900, 3.20,
2*10^20, 950, 9.38,
2*10^20, 1000, 10.81,
2*10^20, 900, 3.50,
2*10^20, 950, 10.02,
2*10^20, 1000, 10.60
),3,12)))

names(data1)[names(data1) == "V1"] <- "Polysilicon_doping"
names(data1)[names(data1)== "V2"] <- "annual_temperature"
names(data1)[names(data1)== "V3"] <- "y"

model<-aov(y~factor(Polysilicon_doping)*factor(annual_temperature),data=data1)
summary(model)

#b) prepare appropriate residual plots and comment on the model's adequacy
par(mfrow=c(2,2))
plot(model)

#c) Under what conditions would you operate this process?
# Which condition can we maximize the yield of the chemical process?
interaction.plot(data1$annual_temperature,data1$Polysilicon_doping,data1$y)

#b) prepare appropriate residual plots and comment on the model's adequacy
par(mfrow=c(2,2))
plot(model)

Temp2<-data1$annual_temperature^2
Poly2<-data1$Polysilicon_doping^2

data2<-cbind(Temp2, data1)
model2<-lm(y~annual_temperature*Polysilicon_doping+Temp2, data=data2)
summary(model2)

model3<-aov(y~Polysilicon_doping*annual_temperature+Temp2,data=data2)
summary(model3)

#10
data<-as.data.frame(t(matrix(c(160, 10,
171, 15,
175, 15,
182, 20,
184, 20,
181, 20,
188, 25,
193, 25,
195, 28,
200, 30,
),2,10)))

data<-as.data.frame(t(matrix(c(10,160, 
15, 171,
15, 175, 
20, 182, 
20, 184, 
20, 181, 
25, 188, 
25, 193, 
28, 195, 
30, 200, 
),2,10)))
names(data)[names(data) == "V1"] <- "Percent_hardwood"
names(data)[names(data) == "V2"] <- "y"


