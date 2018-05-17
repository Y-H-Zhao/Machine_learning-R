setwd("D:/R/Graph")
#simple graph
#plot
attach(mtcars)
class(mtcars)
mtcars
mtcars<-mtcars[order(mtcars$wt),]
plot(mtcars$mpg)
plot(mtcars$wt,mtcars$mpg)
plot(x=mtcars$wt,
     y=mtcars$mpg,
     type = "o",
     main = "标题",
     sub = "子标题",
     xlab= "x轴",
     ylab = "y轴",
     asp=0.1) #y/x的比例
#pie，饼图
#使用 terrain.colors来添加颜色
x<-c(10,20,60,80)
label<-c("北京","上海","广州","深圳")
piepercent<-round(100*x/sum(x),1)
piepercent<-paste(label,piepercent,"%",sep = "")
pie(x,
    labels = piepercent,
    main = "城市饼图",
    col = terrain.colors(length(x)))
legend("bottomleft",
       label,
       cex = 0.8,
       fill = terrain.colors(length(x)))
#boxplot
x1<-c(10 ,0,33,78,90,22)
x2<-c(80.22,98.01,77.45,87.12,33.56)
boxplot(x1,x2,names = c("x1","x2"),col = c(2,3))
#条形图
require(grDevices)
tn<-table(Ni<-stats::rpois(100,lambda=8))
barplot(tn,col = rainbow(30)) #这个色彩不错
barplot(tn,space = 2.5,axisnames = FALSE,
        sub = "barplot")
barplot(tn,border = "dark blue",sub = "border dark")
barplot(tn,col = heat.colors(12),log = "y",sub = "日志") #另一种色彩
barplot(height = cbind(x=c(485,91)/485*100,
                       y=c(940,200)/940*100,
                       z=c(57,17)/57*100),
        beside = F,width = c(485,940,57),
        col = c(1,2),legend.text = c("A","B"),
        args.legend = list(x="topleft"))
#直方图，与条形图不同，它是用面积而非高度来表示数量，他的高度表示
#频数，宽度表示区间长度
hist(mtcars$mpg,breaks = 20)
#QQ图 通过画分位数来比较两个概率分布的图形方法
y<-rt(200,df=5)
qqnorm(y) #正态
qqline(y,col=2) #加线
qqplot(y,rt(300,df=5)) 比较两个分布
#qqline(y,rt(300,df=5))
#协同图coplot()
#星象图stars()
#热度图heatmp()
#密度图density()
