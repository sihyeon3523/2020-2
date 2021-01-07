# ex 6.2 pp.257
# -1, +1 은 coded value of factor를 의미 
# coded value of factor A, coded value of facor B, coded value of factor C, coded value of factor D, response variable

filtr =c(-1,-1,-1,-1,45,
+1,-1,-1,-1,71,
-1,+1,-1,-1,48,
+1,+1,-1,-1,65,
-1,-1,+1,-1,68,
+1,-1,+1,-1,60,
-1,+1,+1,-1,80,
+1,+1,+1,-1,65,
-1,-1,-1,+1,43,
+1,-1,-1,+1,100,
-1,+1,-1,+1,45,
+1,+1,-1,+1,104,
-1,-1,+1,+1,75,
+1,-1,+1,+1,86,
-1,+1,+1,+1,70,
+1,+1,+1,+1,96)
# vector 를 matrix로 바꾸고 data.frame 으로 바꾸는 코드
# each column represented a observation
# each row means coded value of factor A, coded value of facor B, coded value of factor C, coded value of factor D, response variable
# t(matrix(filtr,5,16)): matrix를 transpose 
# 각각의 행이 의미하는 것: (1), a, b, ab, c, ac, bc, abc, d, ad, bd, abd, cd, acd, bcd, abcd 
# 꼭 각 행의 순서를 따라해야 한다. (중요)

matrix(filtr,5,16)
t(matrix(filtr,5,16))
ex6.2<-data.frame(t(matrix(filtr,5,16)))
ex6.2
colnames(ex6.2)<-c('A','B','C','D','rate')
ex6.2
# 이 문제에서는 only one replicate, no multiple replication
# 1. remove the higher order interaction term

# full model
fit<-aov(rate~factor(A)*factor(B)*factor(C)*factor(D),data=ex6.2)
summary(fit)
# 각 treatment 의 observation이 하나씩 밖에 없기 때문에 MSe 값을 구할 수 없음 
# So we need to remove the term that has small number of sum of square

# 2. normal probability plot을 그리기 위해 다운로드 패키지 
install.packages('unrepx')
require(unrepx)                                                                                                                                                           
? yates 
# estimate effect 각 factor 별로 effect를 estimate 한 값이 나옴 
# main effect of A
result<-yates(ex6.2$rate,label=c("A","B","C","D"))
# to make the normal probability plot use hnplot (half normal probability plot)
# x: absolute of effect : blue dot => positive를 의미하고, red dot => negative를 의미
# mean value of effect is close to zero, that means those term are not significant 
# this term very significant 
# we can specify which one is critical
hnplot(result, ID=TRUE) # A, AC, AD, D, C 가 significant함을 알 수 있음 

# refine model
# remove the factor B
fit<-aov(rate~factor(A)*factor(C)*factor(D),data=ex6.2)
# p-value 로 CD, ACD는 are not significant 함을 알 수 있음 두개는 제거 후 최종 모델 선정 
summary(fit)  
final.fit<-aov(rate~factor(A)+factor(C)+factor(D)+factor(A):factor(C)+factor(A):factor(D),data=ex6.2)
summary(final.fit)

# residual analysis 
plot(final.fit,1) # homoscedicity: fitted value가 바뀜에 따라 variance또한 바뀐다 not homo 
# non-linear function임을 알 수 있음 
plot(final.fit,2) # error term follows normal distribution
interaction.plot(ex6.2$A,ex6.2$C,ex6.2$rate) # combination of positive value A, negative value C가 highest response value
interaction.plot(ex6.2$A,ex6.2$D,ex6.2$rate) # combination of positive value A, positive value D가 highest response value

# we assume the value is numerical value 이기 때문에 factor(C)라고 적지 않음 
# to increase the etch rate, select the positive value of A, negative value of C
# to increase the etch rate, select the positive value of A, positive value D
lm(rate~A+C+D+A:C+A:D,data=ex6.2)

# all the factor is significant
anova(lm(rate~A+C+D+A:C+A:D,data=ex6.2))

#Thursday AM 10:30~PM12:00 (2공학관 422호, 423호)