setwd("D:/example")
library(ISLR)
fix(Smarket)
attach(Smarket)
names(Smarket)
#lag1~5过去五个工作日的投资回报率
#Volume为前一日的股票回报量
#Today当日的投资回报率，Direction股票走向Up/Down
summary(Smarket)
cor(Smarket[,-9])
plot(Smarket$Volume) #向上

#logistic regression
#通过lag1~5和Volume来预测Direction，为什么不用Today？
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,
             family = binomial)
summary(glm.fit)
summary(glm.fit)$coef[,4] #p值
glm.prob<-predict(glm.fit,type = "response") #type = "response"表示输出Y=1的概率
glm.prob[1:10]

contrasts(Direction)

glm.pred<-rep("Down",1250)
glm.pred[glm.prob>0.5]<-"Up"
sum(glm.pred=="Up")
#混淆矩阵
table(glm.pred,Smarket$Direction)
#错误率
err.rate<-(457+141)/1250
#假阳性率：FP（假阳性值）/N（阴性值）
FP.rate<-457/(145+457)
#真阳性率：TP(真阳性值)/P（阳性值）
TP.rate<-507/(507+141)

#划分训练和测试集01~04训练，05测试
#使用条件语句产生一个布尔数据和Which不同,which返回条件语句成立的下标，
#而直接使用条件语句，不成立的取值为F，成立取值为T。
train<-(Smarket$Year<2005)
Smarket.2005<-Smarket[Year>=2005,]
Smarket.2005_1<-Smarket[!train,]
Smarket.2005_2<-Smarket[which(Smarket$Year>=2005),]
#以上三种一样的结果

Direction.2005<-Direction[Year>=2005]

#在glm 中加入subset 参数获取训练数据
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,
             family = binomial,subset = train)
glm.probs<-predict(glm.fit,Smarket.2005,type = "response")
glm.pred<-rep("Down",252)
glm.pred[glm.probs>0.5]<-"Up"
table(glm.pred,Direction.2005)

mean(glm.pred==Direction.2005)
#错误率
err.rate<-(34+97)/(34+77+97+44)
#假阳性率：FP（假阳性值）/N（阴性值）
FP.rate<-34/(77+34)
#真阳性率：TP(真阳性值)/P（阳性值）
TP.rate<-44/(97+44)
#预测的不准，很正常，这种方法不靠谱而已。
#去除影响比较小的变量p值较大的变量，重新建模
glm.fit<-glm(Direction~Lag1+Lag2,data = Smarket,
             family = binomial,subset = train)
glm.probs<-predict(glm.fit,Smarket.2005,type = "response")
glm.pred<-rep("Down",252)
glm.pred[glm.probs>0.5]<-"Up"
table(glm.pred,Direction.2005)

mean(glm.pred==Direction.2005)
#预测一个新值
predict(glm.fit,newdata = data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type = "response")

#LDA线性判别分析
#lda()函数没有family选项
library(MASS)
lda.fit<-lda(Direction~Lag1+Lag2,data = Smarket,
             subset = train)
lda.fit
#输出的项目
#1.训练集中上升和下降的比重
#2.在分类的各种情况下预测变量的均值
#3.组合判断系数
lda.pred<-predict(lda.fit,Smarket.2005)
names(lda.pred)
#class:predict result
#posterior:k列表示属于第k类的后验概率
#x：线性判别
lda.class<-lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

lda.pred$x
-0.642*-0.134-0.514*0.008
#线性判别的结果为x，转化为各类的概率为posterior，最后的分类为class

#二次判别QDA
qda.fit<-qda(Direction~Lag1+Lag2,data = Smarket,
             subset = train)
qda.fit
#没有判断系数，因为是二次关系
qda.pred<-predict(qda.fit,Smarket.2005)
names(qda.pred)
#class:predict result
#posterior:k列表示属于第k类的后验概率

qda.class<-qda.pred$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)

#K近邻，KNN只有一个简单的命令做预测，需要输入四个参数
#1)训练数据相关的预测变量矩阵，train.x
#2)测试数据相关的预测变量矩阵，test.X
#3)训练观测类标签的向量，train.Direction
#4)K的值
#下面一一构造它们
library(class)
train.X<-cbind(Lag1,Lag2)[train,]
test.X<-cbind(Lag1,Lag2)[!train,]
train.Direction<-Direction[train]

set.seed(34)
knn.pred<-knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

set.seed(4)
knn.pred<-knn(train.X,test.X,train.Direction,k=4)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

#apply--Caravan
attach(Caravan)
dim(Caravan)
summary(Purchase)
#标准化
standardized.X<-scale(Caravan[,-86])

#使用KNN，构造结构
test<-1:1000

train.X<-standardized.X[-test,]
test.X<-standardized.X[test,]
train.Y<-Purchase[-test]
test.Y<-Purchase[test]

set.seed(1)
knn.pred<-knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")

table(knn.pred,test.Y)
#预测阳性率
9/(68+9)

set.seed(1)
knn.pred<-knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
#预测阳性率
5/(21+5)

set.seed(1)
knn.pred<-knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
#预测阳性率
4/(11+4)

#逻辑回归对比
glm.fit<-glm(Purchase~.,data = Caravan,family = binomial,
             subset = -test)
glm.probs<-predict(glm.fit,Caravan[test,],type = "response")
glm.pred<-rep("No",1000)
glm.pred[glm.probs>0.5]<-"Yes"
table(glm.pred,test.Y)
#预测阳性率
0

#阈值0.25
glm.pred<-rep("No",1000)
glm.pred[glm.probs>0.25]<-"Yes"
table(glm.pred,test.Y)
#预测阳性率
11/(11+22)
