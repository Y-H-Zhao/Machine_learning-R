setwd("D:/example")
#读取disc中数据
data<-read.csv("./disc.csv")
names(data)
#disc数据分类指标三个类别：group-1代表上升，group-2代表稳定，group-3代表下降
#其他指标：企业规模（is）、服务(se)、雇员工资比例(sa)、利润增长(prr)、
#市场份额(ms)、市场份额增长(msr)、流动资金比例(cp)、资金周转速度(cs)
summary(data)
plot(data$group) #向上

#LDA线性判别分析
#lda()函数
library(MASS)
#十折交叉验证
#install.packages("caret")
library("caret")
folds<-createFolds(y=data$group,k=10)#根据data$group把数据集切分成10等份
re<-{}
for(i in 1:10){
  train_input<-data[-folds[[i]],2:9]
  train_output<-data[-folds[[i]],1]
  test_input<-data[folds[[i]],2:9]
  test_output<-data[folds[[i]],1]
  lda.fit<-lda(group~.,data = data,
               subset = -folds[[i]])
  lda.pred<-predict(lda.fit,test_input)
  lda.class<-lda.pred$class
  #mean(lda.class==test_output)
  re=c(re,mean(lda.class==test_output))
}
mean(re)#取k折交叉验证结果的均值作为评判模型准确率的结果

lda.fit
#输出的项目
#1.训练集中上升和下降的比重
#2.在分类的各种情况下预测变量的均值
#3.组合判断系数
names(lda.pred)
#class:predict result
#posterior:k列表示属于第k类的后验概率
#x：线性判别
table(lda.class,test_output)

lda.pred$x
#线性判别的结果为x，转化为各类的概率为posterior，最后的分类为class

#K近邻，KNN只有一个简单的命令做预测，需要输入四个参数
#1)训练数据相关的预测变量矩阵，train.x
#2)测试数据相关的预测变量矩阵，test.X
#3)训练观测类标签的向量，train.Direction
#4)K的值
library(class)
#10折
set.seed(34)
knn_re<-{}
for(i in 1:10){
  train_input<-data[-folds[[i]],2:9]
  train_output<-data[-folds[[i]],1]
  test_input<-data[folds[[i]],2:9]
  test_output<-data[folds[[i]],1]
  knn.pred<-knn(train_input,test_input,train_output,k=1)
  #mean(lda.class==test_output)
  knn_re=c(knn_re,mean(knn.pred==test_output))
}
mean(knn_re)#取k折交叉验证结果的均值作为评判模型准确率的结果

table(knn.pred,test_output)
mean(knn.pred==test_output)
