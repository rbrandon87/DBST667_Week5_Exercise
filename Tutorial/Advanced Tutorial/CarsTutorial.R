library(neuralnet)


cars <- read.csv("cars.csv")

head(cars)

summary(cars)

str(cars)

cars$Hp[is.na(cars$Hp)]<-mean(cars$Hp, na.rm=TRUE)

cars$price[is.na(cars$price)]<-mean(cars$price, na.rm=TRUE)
summary (cars$price)

cars$Id <- NULL

cars$symboling<-factor(cars$symboling)

summary(cars$symboling)

set.seed(1234)


matrix.train <- model.matrix(~ ., data=cars)

View(matrix.train)

colnames(matrix.train)[colnames(matrix.train)=="symboling-1"] <-"symbolingm1"

nn <- neuralnet(symbolingm1+symboling0+symboling1+symboling2+symboling3 ~ city_mpg+hwy_mpg+Enginesize +Fueltypegas+Numofdoorsfour+Numofdoorstwo, data=matrix.train, hidden=c(3), linear.output=F)

names(nn)

nn$call

nn$response

nn$covariate

nn$model.list

nn$linear.output

nn$result.matrix

nn$net.result

nn$weights


nn$startweights

install.packages("devtools")
library(devtools)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

plot.nnet(nn)

mypredict<-compute(nn, nn$covariate)$net.result

maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(mypredict, c(1), maxidx)

prediction <- c(-1, 0, 1, 2, 3)[idx]

table(prediction, cars$symboling)
