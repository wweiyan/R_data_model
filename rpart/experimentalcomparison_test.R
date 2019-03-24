library(DMwR)
library(rpart)
data(algae)
algae<-algae[-manyNAs(algae), ]
clean.algae <- knnImputation(algae, k=10)

cv.rpart <- function(form, train, test, ...){
  m <- rpartXse(form, train, ...)
  p <- predict(m, test)
  mse <- mean((p-resp(form, test))^2)
  c(nmse = mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

cv.lm <- function(form, train, test, ...){
  m <- lm(form, train, ...)
  p <- predict(m, test)
  p <- ifelse(p < 0, 0, p)
  mse <- mean((p-resp(form, test))^2)
  c(nmse = mse/mean((mean(resp(form,train))-resp(form,test))^2))
}

res <- experimentalComparison(
  c(dataset(a1~., algae[,1:12],'a1')),
  c(variants('cv.lm'),
    variants('cv.rpart',se = c(0,0.5,1))),
  cvSettings(3,10,1234))

bestScores(res)

DSs <- sapply(names(algae)[12:18],
              function(x, names.attr){
                f <- as.formula(paste(x,"~."))
                dataset(f, algae[, c(names.attr, x)], x)
              },
              names(algae)[1:11]
              )


library(randomForest)
cv.rf <- function(form, train, test, ...){
  m <- randomForest(form, train, ...)#随机森林模型训练
  p <- predict(m ,test)#预测数值
  mse <- meam((p-resp(form, test))^2)#预测值与实际间方差
  c(nmse = mse/mean((mean(resp(form, test))-resp(form, test))^2))
}

res.all <- experimentalComparison(
  DSs,
  c(variants('cv.lm'),
    variants('cv.rpart',se=c(0,0.5,1)),
    variants('cv.rf',ntree=c(200,500,700))
    ),
  cvSettings(5,10,1234)
)








