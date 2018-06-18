#Neural network method on diabetis data
#Run once to install the packaglibrary
install.packages("neuralnet")
library("neuralnet")

#Read the diabetes data file.  Change the file location.
getwd()
diabetes <- read.csv("diabetes.csv")
View(diabetes)

#make sure that the result is reproducible
set.seed(12345)
#Build the model. If you receive a warning, rerun the command.
nn<-neuralnet(formula = class~preg+plas+pres+skin+insu+mass+pedi+age, data = diabetes, hidden=2, linear.output = T)
#names command displays the available neural network properties
names(nn)
nn
#Run the commands to display the network properties
nn$call
nn$response
nn$covariate
nn$model
#display the first 10 predicted probabilities
nn$net.result[[1]][1:10]
nn$weights
nn$startweights
nn$result.matrix
plot(nn)
#Model evaluation; Round the predicted probabilities
mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)
# confusion matrix
table(mypredict, diabetes$class)

