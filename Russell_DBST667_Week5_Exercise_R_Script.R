#Week 5 Exercise
#Brandon Russell
#Vertebral Column Dataset Neural Network

#Load Library
library(neuralnet)

#Part 2b - Read the data
VertCol <- read.csv("column.csv")

#Look for missing values
apply(VertCol, 2, function (VertCol) sum(is.na(VertCol)))

#Examine Data
View(VertCol)
summary(VertCol)
str(VertCol)

#Part 2ci - Create Seed and NeuralNet function
set.seed(12345)

#Creative way of building NN formula
allVars <-colnames(VertCol)
allVars
predictorVars <- allVars[!allVars%in%"class"]
predictorVars
predictorVars <- paste(predictorVars, collapse = "+")
predictorVars
form=as.formula(paste("class~",predictorVars,collapse = "+"))
form

#NN Model
nn<-neuralnet(formula = form, data = VertCol, hidden=2, linear.output = T)
nn

#Part 2 cii - Results Matrix
nn$result.matrix

#Part 2 ciii - Result Output
nn$net.result[[1]][1:10]

#Part 2 d - Plot the Neural Network

plot(nn)

#Part 2 ei - Eval the model

mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)
head(mypredict,10)


# Part 2 eii -confusion matrix
table(mypredict, VertCol$class)

#Part 2 fi - Report c,d,e w/ new parameters

#New NN Model
nn.new<-neuralnet(formula = form, data = VertCol, hidden=c(4,2), linear.output = T)
nn.new

#Part 2 fi - cii - Results Matrix
nn.new$result.matrix

#Part 2 fi - ciii - Result Output
nn.new$net.result[[1]][1:10]

#Part 2 fi - d - Plot the Neural Network

plot(nn.new)

#Part 2 fi - ei - Eval the model

mypredict<-compute(nn.new, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)
head(mypredict,10)


# Part 2 fi - eii -confusion matrix
table(mypredict, VertCol$class)


