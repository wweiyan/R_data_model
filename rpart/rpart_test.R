library(DMwR)
library(rpart)
data(algae)
algae<-algae[-manyNAs(algae), ]
rt.a1<-rpart(a1~., data=algae[,1:12])
rt.a1
prettyTree(rt.a1)
printcp(rt.a1)


my.tree<-snip.rpart(rt.a1,c(4,7))#剪枝后的树

rt2.a1 <- prune(rt.a1,cp=0.0278)#定义cp值
rt.preditions.a1<-predict(rt.a1,algae) #模型估计
regr.eval(algae[,"a1"], rt.preditions.a1, train.y = algae[, "a1"])#评价生成树的误差


library(randomForest)
data(iris)
set.seed(71)
iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)
