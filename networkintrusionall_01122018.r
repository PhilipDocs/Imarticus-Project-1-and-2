#=========remove all from R environment
rm(list=ls(all=TRUE))
require("rattle")
require("caret")
#===============Network Intrusion Case Study=============
#==============Build a model in training and test in validate
#========Load the data 
train<-read.csv('file:///C:/Users/128564/Desktop/New folder (2)/Networkintrusion/Python_Module_Day_16.2_Network_Intrusion_Train_data (1).csv')
validate<-read.csv('file:///C:/Users/128564/Desktop/New folder (2)/Networkintrusion/Python_Module_Day_16.4_Network_Intrusion_Validate_data (1).csv')
#===========================================================
#========================================================
table(train$class);table(validate$class);
#=========50% of data is classified as anomaly===========
#========Basic plots to understand about the data ====
require("ggplot2")
ggplot(train, aes(class,duration)) + geom_point()
#Duration is higher for anomoly cases
ggplot(train, aes(duration)) + geom_histogram()
ggplot(train, aes(protocol_type,duration)) + geom_boxplot()
#================================
ggplot(train, aes(flag,protocol_type)) + geom_boxplot()
table(train$protocol_type)
#==================================
ggplot(train, aes(flag,service)) + geom_boxplot()
unique(train$service)
#=============
colnames(train)
ggplot(train, aes(protocol_type, fill = as.factor(class))) + geom_bar()
#==#================
ggplot(train, aes(protocol_type, class, col = flag)) + geom_point(alpha = 0.4) + theme_minimal()
#===============================================================
source("file:///C:/Users/128564/Desktop/New folder (2)/Networkintrusion/datasplit.r")
##################################Call data split R
require("caTools")
modeldata<-train
out<-datasplit(modeldata,0.8,100)
class(out)
length(out)
train<-out[[1]]
test<-out[[2]]
#=======================================================
dim(train);
dim(test);
#===============Take all factor variables out and relevel to avoid issues======================================
catr <- unlist(lapply(train, is.factor))
x2<-train[ , catr]
colnames(x2)
#==========================================================================
setdiff(unique(test$service),unique(train$service))
setdiff(unique(test$flag),unique(train$flag))

levels(test$service)<-levels(train$service)
levels(test$protocol_type)<-levels(train$protocol_type)
levels(test$flag)<-levels(train$flag)
colnames(train)
#==========================Build a decision tree using rpart======================
#================Recursive partitioning and regression trees======================
class(train$class)
require("rpart")
m1<-rpart(class~.,data=train)
m1
#===========plot will not be clear if more variables ===================
plot(m1,compress=TRUE,margin=TRUE); text(m1)
asRules(m1)
#=================Predict in train data======================================
Pred_Train_Class <- predict(m1,type='class')
Pred_Train_Prob <- predict(m1,type='prob')
#=================Add predictions to train data ===============================
train$y_class<-ifelse(train$class=="anomaly",1,0)
train$y_class<-as.factor(train$y_class)
train$y_pred_Class<-Pred_Train_Class
train$y_pred_prob<-Pred_Train_Prob[2]
#===================Predict in test data ===================================
Pred_Test_Class<-predict(m1,test,type="class")
Pred_Test_Prob<-predict(m1,test,type="prob")
#===========================================================================
test$y_class<-ifelse(test$class=="anomaly",1,0)
test$y_class<-as.factor(test$y_class)
test$y_pred_Class<-Pred_Test_Class
test$y_pred_prob<-Pred_Test_Prob[2]
#==========================================================================
m1_conf_train<-confusionMatrix(train$class,train$y_pred_Class)
m1_conf_test<-confusionMatrix(test$class,test$y_pred_Class)
m1_conf_train
m1_conf_test
#=====================Model Looks good but interpretability is not that good==================
train<-train[,c(-43,-44,-45,-46)]
test<-test[,c(-43,-44,-45,-46)]
m2<-rpart(class~.,data=train,minsplit=301,cp=0.11,minbucket=100,maxdepth=1)
m3<-prune(m1,cp=0.05)
m4<-prune(m3,cp=0.02)
m5<-prune(m4,minbucket=9,maxdepth=1,cp=0.05)
summary(m5)
plot(m5)
text(m5)
m1<-m5
#===========plot will not be clear if more variables ===================
plot(m1,compress=TRUE,margin=TRUE); text(m1)
#=====================================================================
#=================Predict in train data======================================
Pred_Train_Class <- predict(m1,type='class')
Pred_Train_Prob <- predict(m1,type='prob')
#=================Add predictions to train data ===============================
train$y_class<-ifelse(train$class=="anomaly",1,0)
train$y_class<-as.factor(train$y_class)
train$y_pred_Class<-Pred_Train_Class
train$y_pred_prob<-Pred_Train_Prob[2]
#===================Predict in test data ===================================
Pred_Test_Class<-predict(m1,test,type="class")
Pred_Test_Prob<-predict(m1,test,type="prob")
#===========================================================================
test$y_class<-ifelse(test$class=="anomaly",1,0)
test$y_class<-as.factor(test$y_class)
test$y_pred_Class<-Pred_Test_Class
test$y_pred_prob<-Pred_Test_Prob[2]
str(test)
#==========================================================================
m1_conf_train<-confusionMatrix(train$class,train$y_pred_Class)
m1_conf_test<-confusionMatrix(test$class,test$y_pred_Class)
#=============================================================
train<-train[,c(-43,-44,-45,-46)]
test<-test[,c(-43,-44,-45,-46)]
#==========================Grid search option==============================
#==========================================================================
require("caret")
train.control <- trainControl(
  method = "repeatedcv",
  number = 10, ## 10-fold CV
  repeats = 3,## repeated three times
  # USE AUC
  summaryFunction = twoClassSummary, 
  classProbs = TRUE
)
require("e1071")
#========================================================================
m3 <- tune.rpart(class~., data = train, minsplit = c(5,10),maxdepth=1:2,cp = c(0.002,0.03),minbucket=c(5,15))
summary(m3)
#=========================================================================

system.time (rpartFit1 <- train(class~., data=train, 
                                method = "rpart", 
                                tuneLength = 6,
                                trControl = train.control,
                                metric = "ROC",minsplit=5,minbucket=5,cp=0.002,maxdepth=2
))
rpartFit1
plot(rpartFit1)
rpartFit1$finalModel
require("rattle")
fancyRpartPlot(rpartFit1$finalModel)
#================================================================================
fm<-rpartFit1$finalModel
#=============Build model
colnames(train)
train<-train[,c(1:42)]
require("rpart")
fm1<-rpart(class~.,data=train,minsplit=5,minbucket=5,cp=0.002,maxdepth=2)

#fm1<-rpart(class~.,data=train,minsplit=3,minbucket=3,cp=0.002,maxdepth=30)
# fm2<-rpart(class~.,data=train)
# fm1<-fm2
#=================================================================================
train$pred_class<-predict(fm1,type="class")
pred_prob<-predict(fm1,type="prob")
train$pred_prob<-pred_prob[,1]
train$y_prob<-ifelse(train$class=="anomaly",1,0)
#===============
summary(train$pred_prob)
#=========Calculate ROC
require("pROC")
roc(train$y_prob~train$pred_prob)
plot(roc(train$y_prob~train$pred_prob))
#=================================================================
#====================test data set 
table(test$protocol_type)
table(train$protocol_type)
test$pred_class<-predict(fm1,test,type="class")
pred_prob<-predict(fm1,test,type="prob")
test$pred_prob<-pred_prob[,1]
test$y_prob<-ifelse(test$class=="anomaly",1,0)
#=========Calculate ROC
roc(test$y_prob~test$pred_prob)
plot(roc(test$y_prob~test$pred_prob))
#===========================Predict in Validate===================================

#validate$pred_class<-predict(fm1,validate,type="class")
#===========This throws up an error======================================
levels(validate$service)<-levels(train$service)
levels(validate$protocol_type)<-levels(train$protocol_type)
levels(validate$flag)<-levels(train$flag)
#====================================================================
validate$pred_class<-predict(fm1,validate,type="class")
pred_prob<-predict(fm1,validate,type="prob")
pred_prob[1:2,]
validate$pred_prob<-pred_prob[,1]

validate$y_prob<-ifelse(validate$class=="anomaly",1,0)
roc(validate$y_prob~validate$pred_prob)
plot(roc(validate$y_prob~validate$pred_prob))
#================Derive rank tables and close off ===================================
dim(train);dim(test);dim(validate);
#====================================================================================
colnames(train)
train$Pred<-train$pred_prob
AU<-train
lft_gn_dt<- AU[order(-AU$Pred),]         
#============rank 1to total rows of a dataset============
lft_gn_dt <-cbind(lft_gn_dt, rank = seq(1: nrow(lft_gn_dt)))
#=======================Split into 10 deciles===============
lft_gn_dt<-cbind(lft_gn_dt,group=cut(lft_gn_dt$rank, breaks=10,labels = FALSE,ordered_result = TRUE))
#==================
t1<-lft_gn_dt
unique(t1$group)
table(t1$group)
require("plyr")
str(train$class)
str(train)
#=================
ranktable<-ddply(lft_gn_dt,~group, summarise,
                 Count=length(group),
                 limit =max(rank),
                 minp=min(Pred),
                 maxp=max(Pred),
                 predicted_resolution=mean(Pred),
                 error=sum(y_prob),
                 not_error=length(group)-sum(y_prob),
                 actual_resolution=sum(y_prob)/length(group)
)
ranktable$cut1<-ranktable$not_error*1.0/(ranktable$error+ranktable$not_error)
ranktable$hit<-ranktable$error*1.0/sum(ranktable$error)
ranktable_train<-ranktable
trainp<-lft_gn_dt
cutoff <- ranktable$maxp
cutoff[1]<-1000
corr<-function(x)
{return(x*(sample(100000002:100000011,10,replace=F)/100000000))}
cutoff1<-corr(cutoff)
cutoff<-cutoff1
cutoff
write.csv(cutoff,'cutoff.csv')
rm(cutoff)
cutoff<-read.csv('cutoff.csv')
cutoff<-as.numeric(cutoff$x)
cutoff
ranktable_train
#l1<-lft_gn_dt[lft_gn_dt$rank>=15000,] 
#=====================Ranktables for test dataset
test$Pred<-test$pred_prob
lft_gn_dt<-test
lft_gn_dt <- arrange(lft_gn_dt,desc(Pred))
lft_gn_dt<-cbind(lft_gn_dt,group=cut(lft_gn_dt$Pred, breaks=cutoff,labels = FALSE,ordered_result = TRUE))

if(sum(is.na(lft_gn_dt$group)>0))
{
  
  lft_gn_dt[is.na(lft_gn_dt$group),]$group = 10
}

ranktable<-ddply(lft_gn_dt,~group, summarise,
                 Count=length(group),
                 limit =max(group),
                 minp=min(Pred),
                 maxp=max(Pred),
                 predicted_resolution=mean(Pred),
                 error=sum(y_prob),
                 not_error=length(group)-sum(y_prob),
                 actual_resolution=sum(y_prob)/length(group)
)

ranktable <- arrange(ranktable,desc(group))
aa1<-c(9:1,10)
aa2<-1:10
aa3<-cbind(aa1,aa2)
ranktable<-ranktable[,-1]
colnames(aa3)<-c("group","limit")
aa3<-as.data.frame(aa3)
ranktable<-join(ranktable,aa3)
require("sqldf")
ranktable<-sqldf("select * from ranktable order by maxp desc")
ranktable_test<-ranktable
ranktable_test <- arrange(ranktable_test,-desc(group))
testp<-lft_gn_dt
v1<-ncol(ranktable_test)
ranktable_test<-ranktable_test[,c(v1,1:v1-1)]
testp<-lft_gn_dt
ranktable_test
dim(test)
ranktable_train

#==================OOP dataset
lft_gn_dt<-validate
lft_gn_dt$Pred<-validate$pred_prob
lft_gn_dt <- arrange(lft_gn_dt,desc(Pred))
lft_gn_dt<-cbind(lft_gn_dt,group=cut(lft_gn_dt$Pred, breaks=cutoff,labels = FALSE,ordered_result = TRUE))
unique(lft_gn_dt$group)
if(sum(is.na(lft_gn_dt$group)>0))
{
  
  lft_gn_dt[is.na(lft_gn_dt$group),]$group = 10
}
ranktable<-ddply(lft_gn_dt,~group)
ranktable<-ddply(lft_gn_dt,~group, summarise,
                 Count=length(group),
                 limit =max(group),
                 minp=min(Pred),
                 maxp=max(Pred),
                 predicted_resolution=mean(Pred),
                 error=sum(y_prob),
                 not_error=length(group)-sum(y_prob),
                 actual_resolution=sum(y_prob)/length(group)
)


ranktable <- arrange(ranktable,desc(group))
aa1<-c(9:1,10)
aa2<-1:10
aa3<-cbind(aa1,aa2)
ranktable<-ranktable[,-1]
colnames(aa3)<-c("group","limit")
aa3<-as.data.frame(aa3)
ranktable<-join(ranktable,aa3)
ranktable<-sqldf("select * from ranktable order by maxp desc")
ranktable_OOP<-ranktable
k1<-colnames(ranktable_test)
ranktable_OOP<-ranktable_OOP[,k1]
ranktable_OOP
ranktable_test
ranktable_train
#===================Cutoff and Confusion Matrix============================
#==================Tuning is required for better ranktables==========
#============randomforest handles these cases=========================
#=========Cutoff===============================================
cutoff<-0.5
unique(train$pred_prob)

train$dclass<-ifelse(train$pred_prob>=cutoff,1,0)
test$dclass<-ifelse(test$pred_prob>=cutoff,1,0)
validate$dclass<-ifelse(validate$pred_prob>=cutoff,1,0)
#==============Confusion matrix=============================
train$dclass<-as.factor(train$dclass)
test$dclass<-as.factor(test$dclass)
validate$dclass<-as.factor(validate$dclass)
train$y_prob<-as.factor(train$y_prob)
test$y_prob<-as.factor(test$y_prob)
validate$y_prob<-as.factor(validate$y_prob)
a1<-confusionMatrix(train$y_prob,train$dclass,positive=levels(train$y_prob)[2])
a2<-confusionMatrix(test$y_prob,test$dclass,positive=levels(test$y_prob)[2])
a3<-confusionMatrix(validate$y_prob,validate$dclass,positive=levels(validate$y_prob)[2])
a1
a2
a3
#=================================================================
fancyRpartPlot(fm1)
#========Random forest model======================================
colnames(train);colnames(validate);
train<-train[,c(1:42)]
test<-test[,c(1:42)]
validate<-validate[,c(1:42)]
train$y_prob<-ifelse(train$class=="anomaly",1,0)
test$y_prob<-ifelse(test$class=="anomaly",1,0)
validate$y_prob<-ifelse(validate$class=="anomaly",1,0)
train[1:2,]
#===========Simple random forest model===================================
sqtmtry<- round(sqrt(ncol(train) - 1))
rfGrid <- expand.grid(mtry = c(round(sqtmtry / 2), sqtmtry, 2 * sqtmtry))
rfGrid
sqtmtry
require("caret")
ctrl <- trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary, number = 3) 
set.seed(2)
trained2<- train(class ~ . , data = train[,-43], method = "rf", ntree = 5, tuneGrid = rfGrid, metric = "ROC",
                 trControl = ctrl, importance = TRUE)
print(trained2)
plot(trained2)
#===============================================================================
require("randomForest")
model1<-randomForest(as.factor(class)~.,data=train,mtry=12,ntree=5)
#==============model will not work as more than 53 categories
apply(train[ , catr], 2, function(x) length(unique(x)))
#======Only one variable causing the issue ==Convert to dummy 
results<-fastDummies::dummy_cols(train$service)
train1<-cbind(train,results)
#================find best parameters===========================================
sqtmtry<- round(sqrt(ncol(train1) - 1))
rfGrid <- expand.grid(mtry = c(round(sqtmtry / 2), sqtmtry, 2 * sqtmtry))
rfGrid
sqtmtry
require("caret")
ctrl <- trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary, number = 3) 
set.seed(2)
trained2<- train(class ~ . , data = train1[,c(-3,-43)], method = "rf", ntree = 5, tuneGrid = rfGrid, metric = "ROC",
                 trControl = ctrl, importance = TRUE)
print(trained2)
plot(trained2)
#================================================================================
str(train)
colnames(train)
unique(train$y_prob)
model2<-randomForest(y_prob~.,data=train[,c(-3,-42)],mtry=20,ntree=500)

#===============================================================================
model2

varImpPlot(model2)

#==================================================================================
unique(train$pred_class)
train$pred_class<-predict(model2,type="response")
#pred_prob<-predict(model2,type="response")
pred_prob
#===============
summary(train$pred_class)
train$pred_class1<-ifelse(train$pred_class>=0.5,1,0)
table(train$y_prob,train$pred_class1)
#=================================================================================
test$pred_class<-predict(model2,test,type="class")
# pred_prob<-predict(model2,test)
# test$pred_prob<-pred_prob

test$pred_class1<-ifelse(test$pred_class>=0.5,1,0)
table(test$y_prob,test$pred_class1)

#===================Valdate=====================================================
validate$pred_class<-predict(model2,validate,type="response")
#===========This throws up an error======================================
levels(validate$service)<-levels(train$service)
levels(validate$protocol_type)<-levels(train$protocol_type)
levels(validate$flag)<-levels(train$flag)
#====================================================================
validate$pred_class<-predict(model2,validate,type="response")
validate$pred_class1<-ifelse(validate$pred_class>=0.3,1,0)

table(validate$y_prob,validate$pred_class1)
str(validate)
a3
a4<-confusionMatrix(as.factor(validate$y_prob),as.factor(validate$pred_class1),positive=levels(as.factor(validate$y_prob))[2])
#==========================================================================
a3
a4

#===================================================================
saveRDS(model2,"model2.rda")
#=====================
model2
rm(model2)
getwd()
model_rf<-readRDS("model2.rda")
#==========save pmml
pmml_rf<-pmml(model_rf,"pmml.rda")
#==============decision tree model
saveRDS(fm1,"dt.rda")
model_dt<-readRDS("dt.rda")
model_dt
require("pmml")
pmml_dt<-pmml(model_dt,'dt.rda')
pmml_dt
#===================================================
==============================xgboost model=============================
  colnames(train);colnames(validate);
train<-train[,c(1:42)]
test<-test[,c(1:42)]
validate<-validate[,c(1:42)]
train$y_prob<-ifelse(train$class=="anomaly",1,0)
test$y_prob<-ifelse(test$class=="anomaly",1,0)
validate$y_prob<-ifelse(validate$class=="anomaly",1,0)
train[1:2,]
colnames(train)
str(train)
#============Take 5 imp variables and try out====================
#==========take only numeric variables=============
numr <- unlist(lapply(train, is.numeric))
train1<-train[ , numr]
test1<-test[,numr]
validate1<-validate[,numr]
#=============================================================
labels <- train1$y_prob
unique(train1$y_prob)
ts_label <- test1$y_prob
str(train1$y_prob)
#=============================================================
library(xgboost)
str(train1)
#=============convert all variables to numeric from integer r
train2 <- data.frame(lapply(train1, function(x) as.numeric(as.character(x))))

test2<-data.frame(lapply(test1, function(x) as.numeric(as.character(x))))
#=============================================
train2<-train2[,-ncol(train2)]
test2<-test2[,-ncol(test2)]
train2<-as.matrix(train2)
test2<-as.matrix(test2)
dtrain <- xgb.DMatrix(data = train2,label = labels) 
dtest <- xgb.DMatrix(data = test2,label=ts_label)
#==============
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, 
               colsample_bytree=1)
set.seed(200)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
##best iteration = 75
min(xgbcv$test_error_mean)
#==========================================================
xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 750, watchlist = list(val=dtest,train=dtrain), print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")
#==============================================
xgbpred <- predict (xgb1,dtest)

xgbpred <- ifelse (xgbpred > 0.5,1,0)

confusionMatrix (as.factor(xgbpred), as.factor(ts_label))
mat <- xgb.importance (feature_names = colnames(train2),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20]) 
#=======================================Naive bayes=======
train<-train[,c(1:42)]
test<-test[,c(1:42)]
validate<-validate[,c(1:42)]
train$y_prob<-ifelse(train$class=="anomaly",1,0)
test$y_prob<-ifelse(test$class=="anomaly",1,0)
validate$y_prob<-ifelse(validate$class=="anomaly",1,0)
train[1:2,]
colnames(train)
str(train)
#============Take 5 imp variables and try out====================
#==========take only numeric variables=============
numr <- unlist(lapply(train, is.factor))
train1<-train[ , numr]
test1<-test[,numr]
validate1<-validate[,numr]
#===============
colnames(train1)
Classify = naiveBayes(train1[,c(1:3)],train1[,4])
Classify
test1$pred = predict(Classify,test1[,c(1:3)])
table(test1$pred,test1$class)
#==================
train1$srcvar<-ifelse(train$src_bytes>=28,"Yes","No")
train1$srcvar<-as.factor(train1$srcvar)
test1$srcvar<-ifelse(test$src_bytes>=28,"Yes","No")
test1$srcvar<-as.factor(test1$srcvar)
str(train1)
str(test1)
train[1:2,]
unique(train$srv_count)
train1$srvcount<-train$srv_count
test1$srvcount<-test$srv_count
#===============
colnames(train1)
Classify = naiveBayes(train1[,c(1:3,5,6)],train1[,4])
out = predict(Classify,test1)
table(out,test1$class)
#===================
colnames(train)

Classify = naiveBayes(train[,c(1:41)],train[,42])
out = predict(Classify,test[,c(1:41)])
unique(out)
table(out,test$class)
#===============================================