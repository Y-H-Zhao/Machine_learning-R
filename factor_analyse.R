setwd("D:/example")
wine<-read.csv("./wine.csv")
nwine<-scale(wine)  #标准化
R<-cor(nwine)  #相关系数阵
install.packages("psych")
library(psych)
fa.parallel(R,n.obs=178,fa="both",main="Scree plots with parallel analysis")
#提取公共因子
fa<-fa(R,nfactors=3,n.obs=178,rotate="varimax",scores=TRUE,fm="wls")
fa
fa<-fa(R,nfactors=3,n.obs=178,rotate="varimax",scores=TRUE,fm="pa")
fa
#从解释因子结构的角度正交旋转是最容易解释的，得到的因子也是不相关的；斜交则得到的因子具有相关性，但更符合或能捕捉数据的维度
fa.varimax<-fa(R,nfactors=3,rotate="varimax",fm="pa")
fa.varimax
#斜交旋转
install.packages("GPArotation")
library(GPArotation)
fa.promax<-fa(R,nfactors=3,rotate="promax",fm="pa")
fa.promax
factor.plot(fa.promax,labels = rownames(fa.promax$loadings))
fa.diagram(fa.promax,simple=T)
fa.promax$weights
fa.diagram(fa.varimax,simple = T)
fa.varimax$weights
fa.promax$loadings
names(fa.promax)
fa.promax$R2.scores
