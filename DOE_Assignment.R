data<-as.data.frame(t(matrix(c(900, 1,4.6,
900, 1,4.4,
900, 2, 3.2,
900, 2, 3.5,
950, 1, 10.15,
950, 1, 10.2,
950, 2, 9.38,
950, 2, 10.02,
1000, 1, 11.01,
1000, 1, 10.58,
1000, 2, 10.81,
1000, 2, 10.60),3,12)))

names(data)[names(data) == "V1"] <- "Anneal_Temperature"
names(data)[names(data) == "V2"] <- "Polysilicon_Doping"
names(data)[names(data) == "V3"] <- "y"

model<-aov(y~factor(Anneal_Temperature)*factor(Polysilicon_Doping),data=data)
summary(model)