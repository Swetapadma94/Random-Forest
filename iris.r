library(caret)
library(randomForest)
library(gplots)
data('iris')
View(iris)
training<-createDataPartition(iris$Species,p=0.75,list=F)
train<-iris[training, ]
test<-iris[-training, ]
dim(train)
dim(test)
# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 
iris_setosa<-iris[iris$Species=="setosa",] # 50
iris_versicolor <- iris[iris$Species=="versicolor",] # 50
iris_virginica <- iris[iris$Species=="virginica",] # 50
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])

# Building a random forest model on training data 
fit.forest <- randomForest(Species~.,data=train, na.action=na.roughfix,importance=TRUE)
fit.forest                          
fit.forest$ntree 
pre<-predict(fit.forest,train)
plot(fit.forest)
## Training Accuracy##
mean(pre==train$iSpecies)
# Predicting test data ###
pred<-predict(fit.forest,newdata = test)
pred
mean(pred==test$Species)
#########CrossTable#####
rf<-CrossTable(test$Species,pred)
##or #####
ref<-CrossTable(test$Species,pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                dnn = c('actual default', 'predicted default'))
