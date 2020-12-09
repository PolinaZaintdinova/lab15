# renv::init() # инициализация виртуального окружения
# renv::install("e1071", "ROCR") # установка библиотеки из CRAN
# renv::snapshot() # делаем снимок версий библиотек в нашем виртуальном окружении
# фиксируем этот список в .lock-файле для возможности восстановления
# renv::restore() # команда отктиться к предыдушему удачному обновления библиотек

# ------------------- 
# Лабораторная работа №15:
# Машины опорных векторов (SVM).

library(e1071)
library(ROCR)
setwd("C:/Users/Lenovo/Desktop")


flower<- read.csv("iris.csv",header = T, sep = ",")


for(i in 1:100){ 
  flower[i,5] = 2
}
for(i in 101:150){ 
  flower[i,5] = 1
}
flower <- lapply(flower, as.numeric)

x <- data.frame(flower$sepal.length, flower$sepal.width, 
                flower$petal.length, flower$petal.width)
y <- flower$variety

set.seed(1233)

plot(x, col = (73 - y))




dat = data.frame(x = x, y = as.factor(y))

svm.fit = svm(y ~., data = dat, kernel = "linear",
              cost = 10, scale = F) 

plot(svm.fit, dat,x.flower.petal.width ~ x.flower.petal.length,
     slice = list(x.flower.sepal.width = 3, x.flower.sepal.length = 4))




svmfit$index
summary(svmfit)

set.seed(1)
tune.out = tune(svm, y~., data = dat, kernel = "linear",
                ranges = list(cost = c(0.001, 0.1, 1, 5, 10, 100)))
summary(tune.out)

bestmod = tune.out$best.model
summary(bestmod)

set.seed(1)
train = sample(150, 75)

ypred = predict(bestmod, dat[-train,])
table(predict = ypred, truth = dat[-train,"y"]) 

rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}


svmfit.opt = svm(y ~., data = dat[train,], kernel = "radial",
                 gamma = 2, cost = 1, decision.values = T)

fitted = attributes(predict(svmfit.opt, dat[train,], 
                            decision.values = T))$decision.values
par(mfrow = c(1,2))
rocplot(fitted, dat[train, "y"], main = "dateTraining")
svmfit.flex = svm(y ~., data = dat[train,], kernel = "radial",
                  gamma = 50, cost = 1, decision.values = T)

fitted = attributes(predict(svmfit.flex, dat[train,], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], main = "dateTraining", 
        add = T, col = "purple")


fitted = attributes(predict(svmfit.opt, dat[-train,], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[-train,"y"], main = "dateTest")
fitted = attributes(predict(svmfit.flex, dat[-train,], 
                            decision.values = T))$decision.values
rocplot(fitted, dat[-train,"y"], add = T, col = "purple")