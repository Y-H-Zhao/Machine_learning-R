setwd("D:/example")
wine<-read.csv("./wine.csv")
#fix(wine)
nwine<-scale(wine)  #标准化
R<-cor(nwine)  #相关系数阵
print(GR<-svd(R)$u)
print(l<-svd(R)$d)
model<-princomp(nwine)
summary(model)
screeplot(model,type = 'line',main = '碎石图',lwd=2)
#biplot(model)
model$loadings
model$scores
#model$sdev
eigen(R)
corr=GR%*%diag(l)^(1/2)

n=NULL
plot(n,xlim = c(-1.1,1.1),ylim = c(-1.1,1.1),xlab = '与第一主成分的相关系数',ylab = '与第二主成分的相关系数')
A=seq(0,2*pi,length.out =1000 )
r=1
x1=r*cos(A)
y1=r*sin(A)
points(x1,y1,col='green',lwd=0.2)
a=c("Alcohol","Malic_acid","Flavanoids","Proanthocyanins","Color_intensity","Proline")
for(i in 1:6)
{
  text(corr[i,1],corr[i,2],a[i],cex=1.5)
}
abline(h=0);abline(v=0)

library(scatterplot3d)
x<-model$scores[,1]
y<-model$scores[,2]
z<-model$scores[,3]
dt<-cbind(x,y,z)

color<-apply(dt,1,which.max)
print(color)
scatterplot3d(x,y,z,color,pch = 20,main = '得分分类图',angle=70,
              xlab="第一主成分得分", ylab="第二主成分得分", zlab="第三主成分得分",
              axis=T,col.axis='blue')
