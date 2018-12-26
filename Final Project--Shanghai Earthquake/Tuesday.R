library(nnet)
shanghai.out1<-nnet(z3~, zm1, size=100)
shanghai.pred1<predict(shanghai.out1, zm2)
plot(shanghai.pred1, zm2[, 5])


shanghai.data<-read.csv("shanghai.csv", header = TRUE)[-1,-1]
#plot(density(shanghai.data[[3]]))
v1<- shanghai.data[[3]]
#take out all magnitude>3
l1<- v1>3
l2<- 1*l1
v1a<-v1[l1]    
v1b<-v1a-min(v1a)
parEst(v1b,"Normal")
goodness_fit_test(v1b,"Normal",10000) 

x1<- shanghai.data[[2]][1:190]
x2<- shanghai.data[[2]][2:191]
x3<- shanghai.data[[2]][3:192]

y1<- shanghai.data[[3]][1:190]
y2<- shanghai.data[[3]][2:191]
y3<- shanghai.data[[3]][3:192]
zmat0<-cbind(x1,x2,y1,y2)
zmat0[1:5,]
z3<-1*(y3>3.05)
zm<-as.data.frame(cbind(zmat0,z3))
zm[1:5,]all()
zm1<-zm[1:70,]
zm2<-zm[71:190,]
library(nnet)
shanghai.out1<-nnet(z3~.,zm1,size=100)
shanghai.pred1<-predict(shanghai.out1,zm2)
plot(shanghai.pred1,zm2[,5])
p.pred<-exp(10*(shanghai.pred1-.5))/(1+exp(10*(shanghai.pred1-.5)))
o1<-order(shanghai.pred1)
lines(shanghai.pred1[o1],p.pred[o1])  #Error?
plot(shanghai.pred1[o1],p.pred[o1],type='l') 

y<-zm2[,5]
sum(y*log(p.pred)+(1-y)*log(1-p.pred))
length(y) #120
v1<-sample(120)
sum(y+log(p.pred[v1])+(1-y)*log(1-p.pred[v1]))

plot(x1,x2)