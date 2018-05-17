setwd("D:/example")
library(ISLR)
fix(Auto)    #mpg油耗量，horsepower马力...
names(Auto)  #

#多元回归
lm.fit0<-lm(mpg~.-name-year,data = Auto)
summary(lm.fit0)
#存在不显著的项，进行逐步回归

lm.fit0_step<-step(lm.fit0)
summary(lm.fit0_step)
#残差分析图
par(mfrow=c(2,2))
plot(lm.fit0_step)

#残差图中存在明显的几个点是与众不同的，在一元线性回归中我们把它们
#当成离群点，进行剔除，但是多元回归中，我们应该考虑是否存在交互作用，导致了
#这些点的与众不同，而且残差图显示线性拟合效果一般，所以我们
#尝试建立含有交互项的多元回归
lm.fit<-lm(mpg~origin*horsepower+origin*weight,data = Auto)
summary(lm.fit)

#残差分析
par(mfrow=c(2,2))
plot(lm.fit)

#对两模型进行比较
anova(lm.fit0_step,lm.fit)


#正态性检验
re<-rstudent(lm.fit)
shapiro.test(re)
#未通过

#取对数
mpg_log<-log(Auto$mpg)
Data<-data.frame(mpg_log,Auto[-1])
#重新建模
fix(Data) #将列名mpg_log改为mpg
lm.fit_new<-lm(mpg~origin*horsepower+origin*weight,data = Data)

par(mfrow=c(2,2))
plot(lm.fit_new)

Anova(lm.fit_new) #方差分析表 各项均是显著的。

#正态性检验
re<-rstudent(lm.fit_new)
shapiro.test(re)
#通过

#各模型拟合度比较
fit1<-fitted(lm.fit0_step)

fit2<-fitted(lm.fit)

fit3<-fitted(lm.fit_new)
#绘图比较
par(mfrow=c(1,3))
plot(fit1,Auto$mpg,xlab='无交互拟合值',ylab='真实值')
abline(0,1)

plot(fit2,Auto$mpg,xlab='有交互拟合值',ylab='真实值')
abline(0,1)

plot(fit3,mpg_log,xlab='取对数拟合值',ylab='真实值')
abline(0,1)
