library(randomForest)
library(caret)
fraud<- read.csv(file.choose())
View(fraud)
sum(is.na(fraud))
summary(fraud)
class(fraud)
hist(fraud$Taxable.Income)
# if Taxable Income is less than or equal to 30000 then Risky else Good.
Risky_Good = ifelse(fraud$Taxable.Income<= 30000, "Risky", "Good")
temp1=data.frame(fraud,Risky_Good)
View(temp1)
temp1<-temp1[,c(1:7)]
### Data Partitioning ####

temp<-createDataPartition(temp1$Taxable.Income,p=0.70,list = F)
train<-temp1[temp, ]
test<-temp1[-temp, ]
### Model Building ######
set.seed(213)
rf <- randomForest(Risky_Good~., data=train)
rf 
attributes(rf)
head(train$Risky_Good)
#### Prediction ####
pred1 <- predict(rf, train)
head(pred1)
## Accuracy##
confusionMatrix(pred1,train$Risky_Good) 
## prediction on test data##
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$Risky_Good) 
plot(rf)
 
