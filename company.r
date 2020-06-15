library(randomForest)
library(caret)
company <- read.csv("E:\\Assignment\\random forest\\Company_Data.csv")
View(company)
sum(is.na(company))
class(company$Sales)
attach(company)
company$Sales<-as.factor(company$Sales)
## Data Partioning###
companytrain <- createDataPartition(company$Sales,p=0.65,list = F)
training<-company[companytrain, ]
testing<-company[-companytrain, ]
# Building a random forest model on training data 
fit.forest<-randomForest(Sales~.,data = training,na.action=na.contiguous,importance=TRUE)
fit.forest$ntree
### Training Accuracy###
pre<-predict(fit.forest,training)

mean(training$Sales==predict(fit.forest,training)) # 100% accuracy 
x<-predict(fit.forest,training)
x
# Predicting test data###
pred<-predict(fit.forest,newdata=testing)
 mean(testing$Sales==pred)
library(gmodels)
plot(fit.forest)
plot(pred)
# Cross table 
rf<-CrossTable(testing$Sales,pred)
##or##
rf_perf<-CrossTable(testing$Sales, pred,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual default', 'predicted default'))
