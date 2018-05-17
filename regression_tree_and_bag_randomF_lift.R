setwd("D:/example")
library(MASS)
set.seed(1)
train<-sample(1:nrow(Boston),nrow(Boston)/2)
tree.Boston<-tree(medv~.,data = Boston,subset = train)
summary(tree.Boston)
#使用了三个变量
plot(tree.Boston)
text(tree.Boston,pretty = 0)
#同样道理，交叉验证和剪枝
cv.Boston<-cv.tree(tree.Boston)
plot(cv.Boston$size,cv.Boston$dev,type = 'b')

prune.Boston<-prune.tree(tree.Boston,best = 8)
plot(prune.Boston)
text(prune.Boston,pretty = 0)
#对比
yhat<-predict(prune.Boston,newdata = Boston[-train,])
Boston.test<-Boston[-train,"medv"]
plot(yhat,Boston.test)
abline(0,1)
mean((yhat-Boston.test)^2)

yhat<-predict(tree.Boston,newdata = Boston[-train,])
Boston.test<-Boston[-train,"medv"]
plot(yhat,Boston.test)
abline(0,1)
mean((yhat-Boston.test)^2)

#装袋和随机森林
#randomForest()来实现装袋和随机森林，可以知道装袋法是随机森林m=p的
#一种特殊情况，所以可以用一个实现两个
#install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.Boston<-randomForest(medv~.,data=Boston,subset=train,
                         mtry=13,importance=TRUE)
#mtry,树的每一个分裂点都考虑全部13个变量
#检验效果
yhat<-predict(bag.Boston,newdata = Boston[-train,])
Boston.test<-Boston[-train,"medv"]
plot(yhat,Boston.test)
abline(0,1)
mean((yhat-Boston.test)^2)
#参数ntree设置生成树的数目
bag.Boston<-randomForest(medv~.,data=Boston,subset=train,
                         mtry=13,ntree=25)
#检验效果
yhat<-predict(bag.Boston,newdata = Boston[-train,])
Boston.test<-Boston[-train,"medv"]
mean((yhat-Boston.test)^2)

#随机森林，回归树默认mtry为p/3，分类树默认mtry为p^(1/2)
set.seed(1)
rf.Boston<-randomForest(medv~.,data=Boston,subset=train,
                         mtry=6,importance=TRUE)
yhat.rf<-predict(rf.Boston,newdata = Boston[-train,])
Boston.test<-Boston[-train,"medv"]
mean((yhat.rf-Boston.test)^2)
importance(rf.Boston) #浏览各变量的重要性
varImpPlot(rf.Boston) #看出lstat rm 最重要
#提升法
#使用gbm包中gbm（）函数建立回归树，distribution="gaussian"
#建立分类树，distribution="bernoulli".对象n.trees=5000表示
#提升法需要5000颗树，选型interaction.depth=4限制每棵树的深度
#install.packages("gbm")
library(gbm)
set.seed(1)
boost.Boston<-gbm(medv~.,data=Boston[train,],distribution="gaussian",
                  n.trees=5000,interaction.depth=4)
summary(boost.Boston) #生成一张相对影响图
#反应边际影响
#绘制两个变量的偏相关图
par(mfrow=c(1,2))
plot(boost.Boston,i="rm")
plot(boost.Boston,i="lstat")

#预测
yhat.boost<-predict(boost.Boston,newdata = Boston[-train,],n.trees = 5000)
Boston.test<-Boston[-train,"medv"]
mean((yhat.boost-Boston.test)^2) #结果更好
#选取不同的压缩参数lamada，默认值为0.001，可以修改
boost.Boston<-gbm(medv~.,data=Boston[train,],distribution="gaussian",
                  n.trees=5000,interaction.depth=4,shrinkage=0.2,
                  verbose=F)
yhat.boost<-predict(boost.Boston,newdata = Boston[-train,],n.trees = 5000)
Boston.test<-Boston[-train,"medv"]
mean((yhat.boost-Boston.test)^2)
