setwd("D:/example")
#伽马和逆高斯模拟程序
set.seed(112)
n<-50
x1<-rgamma(n,2,1)
x2<-rgamma(n,2,3)
x3<-rbinom(n,1,0.4)
x4<-rbinom(n,1,0.7)
#参数
b0=7
b1=0.8
b2=-0.8
b3=0.7
b4=-0.6

eta<-b0+b1*x1+b2*x2+b3*x3+b4*x4
#对数连接函数
mu<-exp(eta)

#生成伽马
library(gamlss)
#install.packages("gamlss.tr")
library(gamlss.tr)
y<-rGA(n,mu=mu,sigma = 0.1)
dt<-data.frame(y,x1,x2,x3,x4)
#应用glm
mGA1<-glm(y~x1+x2+factor(x3)+factor(x4),data = dt,family = Gamma(link = log))
mIG1<-glm(y~x1+x2+factor(x3)+factor(x4),data = dt,family = inverse.gaussian(link = log))
#应用gamlss
mGA2<-gamlss(y~x1+x2+factor(x3)+factor(x4),data = dt,
             sigma.formula = ~x1+x2+factor(x3)+factor(x4),family = GA,mu.link = log,sigma.link=log)
mIG2<-gamlss(y~x1+x2+factor(x3)+factor(x4),data = dt,family = IG,mu.link = log,sigma.link=log)

