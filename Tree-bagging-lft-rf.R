setwd("D:/example")
#读取disc中数据
data<-read.csv("./disc.csv")
names(data)
#disc数据分类指标三个类别：group-1代表上升，group-2代表稳定，group-3代表下降
#其他指标：企业规模（is）、服务(se)、雇员工资比例(sa)、利润增长(prr)、
#市场份额(ms)、市场份额增长(msr)、流动资金比例(cp)、资金周转速度(cs)
attach(data)
states<-rep('1',90)
states[which(group==1)]<-'上升'
states[which(group==2)]<-'稳定'
states[which(group==3)]<-'下降'
detach(data)
states
tree_data<-cbind(states,data[,-1])
#加载tree包建立分类树
library(tree)
#建立模型
tree.disc<-tree(states~.,tree_data)
summary(tree.disc)
#训练错误率0%
#作图
plot(tree.disc)
text(tree.disc,pretty = 0)  #出字
tree.disc

#评价效果，进行分组
set.seed(2)
train<-sample(1:nrow(tree_data),72)
disc.test<-tree_data[-train,]
states.test<-states[-train]
tree.disc<-tree(states~.,tree_data,subset = train)
#type="class",使R返回真实的预测类别
tree.pred<-predict(tree.disc,disc.test,type = "class")
table(tree.pred,states.test)
#正确率（6+5+3）/18=0.78
#接下来，考虑剪枝是否可以改进
#使用函数cv.tree执行交叉验证来确定最优复杂树
set.seed(3)
cv.disc<-cv.tree(tree.disc,FUN = prune.misclass) #使用分类错误率
#而不是用cv.tree的默认值偏差来控制交叉验证和剪枝过程
names(cv.disc)
cv.disc
#k,成本复杂性，dev，交叉验证错误率
par(mfrow=c(1,2))
plot(cv.disc$size,cv.disc$dev,type = 'b')
plot(cv.disc$k,cv.disc$dev,type = 'b')
#可以看出size=5时，dev最小为9
#使用prune.misclass()进行剪枝
#都需要先创建一个树
prune.disc<-prune.misclass(tree.disc,best = 5)
par(mfrow=c(1,1))
plot(prune.disc)
text(prune.disc,pretty = 0)  #出字
#同上一棵树无区别

#装袋和随机森林
#randomForest()来实现装袋和随机森林，可以知道装袋法是随机森林m=p的
#一种特殊情况，所以可以用一个实现两个
#install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.disc<-randomForest(states~.,data=tree_data,subset=train,
                         mtry=8,importance=TRUE)
#mtry,树的每一个分裂点都考虑全部8个变量
#检验效果
yhat<-predict(bag.disc,newdata = tree_data[-train,])
disc.test<-tree_data[-train,1]
mean(yhat==disc.test)
#参数ntree设置生成树的数目
bag.disc<-randomForest(states~.,data=tree_data,subset=train,
                         mtry=8,ntree=25)
#检验效果
yhat<-predict(bag.disc,newdata = tree_data[-train,])
mean(yhat==disc.test)

#随机森林，回归树默认mtry为p/3，分类树默认mtry为p^(1/2)
set.seed(1)
rf.disc<-randomForest(states~.,data=tree_data,subset=train,
                        mtry=3,importance=TRUE)
yhat.rf<-predict(rf.disc,newdata = tree_data[-train,])
mean(yhat.rf==disc.test)
importance(rf.disc) #浏览各变量的重要性
varImpPlot(rf.disc) #看出lstat rm 最重要
#提升法
#使用gbm包中gbm（）函数建立回归树，distribution="gaussian"
#建立分类树，distribution="bernoulli".对象n.trees=5000表示
#提升法需要5000颗树，选型interaction.depth=4限制每棵树的深度
#install.packages("gbm")
library(gbm)
set.seed(1)
boost.disc<-gbm(states~.,data=tree_data[train,],
                  n.trees=5000,interaction.depth=4)
summary(boost.disc) #生成一张相对影响图
#反应边际影响
#绘制两个变量的偏相关图
par(mfrow=c(1,2))
plot(boost.disc,i="sa")
plot(boost.disc,i="is")

#预测
yhat.boost<-predict(boost.disc,newdata = tree_data[-train,],n.trees = 5000)
class(yhat.boost)
y<-data.frame(yhat.boost)
cla<-function(x){
  if(which.max(x)==1) return("上升")
  if(which.max(x)==2) return("稳定")
  if(which.max(x)==3) return("下降")
}
result<-apply(y,1,cla)
mean(result==disc.test)  #也全部分对了
