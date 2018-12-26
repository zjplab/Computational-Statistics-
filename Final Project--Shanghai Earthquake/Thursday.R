I1<-as.logical((x1>3.05)*(y2>=3.0))
xa<-x1[I1]
ya<-y2[I1]
shang.data<-as.data.frame(cbind(xa, ya))
shang.data[1:5,]
summary(lm(ya~xa, shang.data))



shang.str<-(lm(ya~xa, shang.data))
predict(shang.str)


shang.data1<-as.data.frame(cbind(xa[1:50], ya[1:50]))
summary(lm(ya~xa, shang.data1))

shang.str<-(lm(ya~xa, shang.data1))
predya<-predict(shang.str, shang.data[51:190, ])
plot(predya, shang.data[51,190, 2])


























x1<-shanghai.data[1:191,2]
x2<-shanghai.data[2:192, 2]
y1<-shanghai.data[1:191, 3]
y2<-shanghai.data[2:192,3]
par(mfrow=c(2,2))
plot(x1, x2)
plot(x1, y2)
plot(y1, x2)
plot(y1, y2)
I1<-as.logical( (x1>3.05)*(y2>3.05) )
plot(x1, y2)
plot(x1[I1], y2)
