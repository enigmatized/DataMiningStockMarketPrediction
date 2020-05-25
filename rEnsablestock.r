realDataSet <- read.csv(file = 'C:/Users/garre/Desktop/DataPredictionModel2015to2019/2015to2020NoNews/Name.csv')
precentage<- read.csv(file = 'C:/Users/garre/Desktop/DataPredictionModel2015to2019/2015to2020NoNews/NamePrecent.csv')

library(mlbench)




#####Creates if there was a change in SPY
###1 above zero, 0 for below zerp
##Used for categeorical Prediction
#precentage$change=ifelse(precentage$spyClose>0,1,0)

size<-nrow(precentage)
precentage<-precentage[2:size,]#getting rid of first row, we are using "yesterday's data", for tomorrow's spy change
precentage$change=ifelse(precentage$spyClose>0,1,0)
#Scaling data
newPrecentScaled=as.data.frame(scale(precentage[,c(-1,-50)]))
newPrecentScaled$change= precentage$change
#head(newPrecentScaled)


Up=subset(newPrecentScaled,change==1)
Down=subset(newPrecentScaled,change==0)
rows4fiftyfity1=sample(1:nrow(Up),510,replace=FALSE)
rows4fiftyfity2=sample(1:nrow(Down),510,replace=FALSE)
#rows1=sample(1:nrow(Up),0.8*nrow(Up),replace=FALSE)
#rows2=sample(1:nrow(Down),0.8*nrow(Down),replace=FALSE)
trainV2=rbind(Up[rows4fiftyfity1,],Down[rows4fiftyfity2,])
testV2=rbind(Up[-rows4fiftyfity1,],Down[-rows4fiftyfity2,])
validation=0.5*nrow(testV2)
testSet=rbind(testV2[-validation,])





rows=sample(1:nrow(precentage),1150,replace=FALSE)
trainzz=precentage[rows,]
testzz=precentage[-rows,]




#Splitting data into Train, Validation And Test
#Doing a 70/15/15 Split
#1200*0.7=840
#420 for up and for 420 for down
Up=subset(newPrecentScaled,change==1)
Down=subset(newPrecentScaled,change==0)
rows4fiftyfity1=sample(1:nrow(Up),420,replace=FALSE)
rows4fiftyfity2=sample(1:nrow(Down),420,replace=FALSE)

train=rbind(Up[rows4fiftyfity1,],Down[rows4fiftyfity2,])

####Splitting the rest of validation and training data
remainingData=rbind(Up[-rows4fiftyfity1,],Down[-rows4fiftyfity2,])

#validation <- remainingData[sample(1:nrow(remainingData), 180, replace=FALSE),]
#testSet=rbind(remainingData[-validation,])


validate_rows = sample(1:nrow(remainingData),180,replace=FALSE)

validation = remainingData[validate_rows,]

test=remainingData[-validate_rows,]

class(test)
class(validate_rows)
#head(validation)
print(train$change)







library(caret)


class(newPrecentScaled$change) 

newPrecentScaled$change = as.factor(newPrecentScaled$change)


set.seed(123456)

rows= sample(1:nrow(newPrecentScaled),size = 0.7*nrow(newPrecentScaled), replace=FALSE) 

train = newPrecentScaled[rows,]
test = newPrecentScaled[-rows,]

train<-train[,c(6, 16, 18, 22, 35, 40, 49)]
test<-test[,c(6, 16, 18, 22, 35, 40, 49)]
#head(train)

trainX<-train[,-5]
trainY<-train[,5]
head(train)

seed<-7




library(caretEnsemble)

mycontrol = trainControl(method="cv", number=10, classProbs=TRUE)

model_list = c("rf", "treebag", "qda", "lda", "glmnet", "stepLDA" , "knn", "nb", "glm", "treebag", "svmRadial", "svmPoly", "svmLinear", "gbm") # list of algorithms

levels(train$change)=c("Up","Down")
View(train$change)
set.seed(seed)
models = caretList(change~., data=train, trControl=mycontrol, methodList=model_list)
results = resamples(models)

modelCor(results)

library(corrplot)
library(ggvis)

corrplot(cor(summary(results)$values),method="circle")
splom(results)



model_list = c("knn", "stepLDA", "rf", "svmRadial", "svmPoly", "svmLinear", "gbm" ) # list of algorithms
set.seed(seed)
models = caretList(change~., data=train, trControl=mycontrol, methodList=model_list)
results = resamples(models)
modelCor(results)
summary(results)
# stack using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.glm <- caretStack(models, method="svmRadial", metric="Accuracy", trControl=stackControl)
print(stack.glm)
cf<-predict(stack.glm, test)
levels(cf)=c("Up","Down")
levels(test$change)=c("Up","Down")
confusionMatrix(cf, test$change)



set.seed(7)

rows= sample(1:nrow(newPrecentScaled),size = 0.7*nrow(newPrecentScaled), replace=FALSE) 

train = newPrecentScaled[rows,]
test = newPrecentScaled[-rows,]
names(train)
train<-train[,c(6, 16, 18, 22, 35, 40, 49)]
test<-test[,c(6, 16, 18, 22, 35, 40, 49)]

names(train)


mytrain=trainControl(method="repeatedcv",number=10, repeats=10)
set.seed(7)
model_logistic= train(change~., data=train,
                      method="glm",
                      family=binomial(link="logit"),
                      preProcess = c("center", "scale"),
                      trControl=mytrain) 


predict_regularizedLOGISITC  = predict ( model_logistic, test) 

confusionMatrix(regularizedLOGISITC, test$change )


fit.logit <- glm(y~gl, )












