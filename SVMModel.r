#====================Initialization=================
#=========remove all from R environment
rm(list=ls(all=TRUE))
#install.packages("rattle")
require("rattle")
require("caret")
setwd("C:\\Users\\BIS\\Desktop\\SVM_CreditRisk\\")
#------------------------------------Load data--------
train<-read.csv('Train.csv')
validate<-read.csv('Validate.csv')
#==========================
summary(is.na(train))
sum(is.na(train))
#========================================================
#====================================
require("Hmisc")
describe(train)
summary(train)
sum(is.na(train))
#==================================
str(train$LoanAmount)
loan_avg<-mean(train$LoanAmount,na.rm=TRUE)
cre_his<-mean(train$Credit_History,na.rm=TRUE)
#=====================================
sum(is.na(train))
train$LoanAmount[is.na(train$LoanAmount)]<-loan_avg
train$Credit_History[is.na(train$Credit_History)]<-cre_his
#==========================================
summary(train)
ln_term<-mean(train$Loan_Amount_Term,na.rm=TRUE)
train$Loan_Amount_Term[is.na(train$Loan_Amount_Term)]<-ln_term
#============================================
source("datasplit.r")
##################################Call data split R
require("caTools")
modeldata<-train
set.seed(100)
out<-datasplit(modeldata,0.8,100)
class(out)
length(out)
train<-out[[1]]
test<-out[[2]]
#=======================================================
dim(train);
dim(test);
#===============================


#================================
levels(test$Gender)<-levels(train$Gender)
levels(test$Married)<-levels(train$Married)
levels(test$Dependents)<-levels(train$Dependents)
levels(test$Education)<-levels(train$Education)
levels(test$Self_Employed)<-levels(train$Self_Employed)
levels(test$Property_Area)<-levels(train$Property_Area)
#============Crude model check==========================
# colnames(train)
# m1<-randomForest(Loan_Status~.,data=train[,-1],mtry=4,ntree=500)
# test$pred<-predict(m1,test,type="class")
# table(test$pred,test$Loan_Status)
#======================================================
catr <- unlist(lapply(train, is.factor))
x2<-train[ , catr]
x2<-x2[,-1]
results<-fastDummies::dummy_cols(x2[,-ncol(x2)])
colnames(results)
results1<-results[,c(-1,-2,-3,-4,-5,-6)]
traindata<-cbind(train,results1)
catr1<-as.data.frame(catr)
catr1$col<-rownames(catr1)
catr2<-catr1[(catr1$catr==TRUE),]
f1<-rownames(catr2)
traindata1<-traindata[,f1]
colnames(traindata1)
f2<-setdiff(colnames(traindata),colnames(traindata1))
traindata2<-traindata[,f2]
traindata2$Loan_Status<-traindata$Loan_Status  
colnames(traindata2)
#===================================================
set.seed(100)
colnames(traindata2)
str(traindata2)
# #=================Crudemodel2
# colnames(traindata2)[15]<-"Dependent_3"
# colnames(traindata2)[18]<-"Education_nc"
# m2<-randomForest(Loan_Status~.,data=traindata2,ntree=500,mtry=7)
# traindata2$pred<-predict(m2,type="class")
# table(traindata2$Loan_Status,traindata2$pred)
#=================svm model
# m1<-svm(Loan_Status~.,data=traindata2)
# traindata2$pred<-predict(m1,type="class")
# table(traindata2$Loan_Status,traindata2$pred)
# m1$kernel
# m1$cost
# m1$degree
# m1$gamma
# m1$coef0
# m1$nu
#================PREPARE TSEST DATA
x2<-test[ , catr]
colnames(x2)
x2<-x2[,-1]
colnames(x2)
results<-fastDummies::dummy_cols(x2[,-ncol(x2)])
colnames(results)
unique(test$Loan_Status)
#=======================
sum(is.na(test))
test[1:10,]
test1<-cbind(test,results)
# test1$Married_<-0
# gg1<-predict(svm.tune,test1,type="prob")[2]
# gg1<-gg1$Y
# 
# test1$svm_pred<-gg1
test1$Loan_Status_n<-ifelse(test1$Loan_Status=="Y",1,0)
#=================================================
unique(test1$Loan_Status_n)
#==================Build a svm model with cv for polynomial svm ##############3
validlist<-colnames(traindata2)
colnames(test1)
#
# setdiff(colnames(test1),validlist)
# test1<-test1[,validlist]

#=====================================
set.seed(12345)
TrainingParameters <- trainControl(method = "repeatedcv", number = 10, repeats=10)
SVModel <- train(Loan_Status ~ ., data = traindata2,
                 method = "svmPoly",
                 trControl= TrainingParameters,
                 tuneGrid = data.frame(degree = 3,
                                       scale = .04,
                                       C = 1),
                 preProcess = c("pca","scale","center"),
                 na.action = na.omit
)
SVModel$results
mm1<-SVModel$finalModel
SVMPredictions <-predict(SVModel, traindata2)
table(SVMPredictions,traindata2$Loan_Status)

confusionMatrix(SVMPredictions,traindata2$Loan_Status,positive="Y")

setdiff(colnames(traindata2),colnames(test1))
test1$Married_<-0
polypredicttest<-predict(SVModel,test1)
table(polypredicttest,test1$Loan_Status)
confusionMatrix(test1$Loan_Status,polypredicttest,positive="Y")

#======================================================================
x2<-validate[ , catr]
colnames(x2)
x2<-x2[,-1]
colnames(x2)
results<-fastDummies::dummy_cols(x2[,-ncol(x2)])
colnames(results)
colnames(validate)
unique(validate$outcome)
#=======================
sum(is.na(validate))
validate1<-cbind(validate,results)
#==========================
colnames(validate1)[13]<-"Loan_Status"
validate1$Married_<-0
setdiff(colnames(test1),colnames(validate1))
validate2<-validate1[,validlist]
#=============================
sum(is.na(validate))
validate2[is.na(validate2)]<-0
polypredicttest<-predict(SVModel,validate2)
dim(validate2)
table(polypredicttest,validate2$Loan_Status)
confusionMatrix(validate2$Loan_Status,polypredicttest,positive="Y")



#=====================================save model as pmml ==========
SVModel$coefnames
colnames(traindata2)
set.seed(12345)
require("kernlab")
require("e1071")
svfit=svm(Loan_Status ~ ., data = traindata2,kernel="polynomial",gama=.04,cost=1,degree=3,preProcess = c("pca","scale","center"))
# svfit=svm(Loan_Status ~ ., data = traindata2,kernel="linear",preProcess = c("pca","scale","center"))

polypredicttest<-predict(svfit,validate2)
dim(validate2)
table(polypredicttest,validate2$Loan_Status)
confusionMatrix(validate2$Loan_Status,polypredicttest,positive="Y")


# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #==============================================
# #============Build a svm model ===========
# # Setup for cross validation
# set.seed(129883)
# ctrl <- trainControl(method="cv",
#                      number = 10,
#                      summaryFunction=twoClassSummary,
#                      classProbs=TRUE)
# 
# # Grid search to fine tune SVM
# grid <- expand.grid(sigma = c(1,1.1),
#                     C = c(1,1.1)
# )
# 
# #Train SVM
# colnames(traindata2)
# str(traindata2)
# traindata2$Loan_Status_n<-ifelse(traindata2$Loan_Status=="Y",1,0)
# traindata2$Loan_Status_n<-as.factor(traindata2$Loan_Status_n)
# unique(traindata2$Loan_Status)
# colnames(traindata2)
# svm.tune <- train(Loan_Status~.,
#                   data=traindata2[,c(7:25)],
#                   method = "svmRadial",
#                   metric="ROC",
#                   tuneGrid = grid,
#                   trControl=ctrl,preProcess = c("pca","scale","center"))
# 
# svm.tune
# class(svm.tune)
# SVM_predictions<-predict(svm.tune, traindata2)
# table(SVMPredictions,traindata2$Loan_Status)
# unique(traindata2$Loan_Status)
# confusionMatrix(traindata2$Loan_Status,SVMPredictions,positive="Y")
# 
# SVM_predictions_prob<-predict(svm.tune, traindata2,type="prob")[2]
# class(SVM_predictions_prob)
# 
# roc(traindata2$Loan_Status_n~SVM_predictions_prob$Y)
# plot(roc(traindata2$Loan_Status_n~SVM_predictions_prob$Y))
# # #=====================================================
# # gg1<-predict(svm.tune,  type="prob")[2]
# # gg1<-gg1$Y
# # traindata2$svm_pred <- as.numeric(gg1)
# # traindata2$svm_pred<-as.numeric(traindata2$svm_pred)
# # str(traindata2)
# # roc(traindata2$Loan_Status_n~traindata2$svm_pred)
# # plot(roc(traindata2$Loan_Status_n~traindata2$svm_pred))
# # #=============================Testdata=============
# x2<-test[ , catr]
# colnames(x2)
# x2<-x2[,-1]
# colnames(x2)
# results<-fastDummies::dummy_cols(x2[,-ncol(x2)])
# colnames(results)
# unique(test$Loan_Status)
# #=======================
# sum(is.na(test))
# test[1:10,]
# test1<-cbind(test,results)
# test1$Married_<-0
# gg1<-predict(svm.tune,test1,type="prob")[2]
# gg1<-gg1$Y
# 
# test1$svm_pred<-gg1
# test1$Loan_Status_n<-ifelse(test1$Loan_Status=="Y",1,0)
# #=================================================
# unique(test1$Loan_Status_n)
# roc(test1$Loan_Status_n~test1$svm_pred)
# plot(roc(test1$Loan_Status_n~test1$svm_pred))
# #=========================calss
# testpredictions<-predict(svm.tune,test1,type="raw")
# testpredictions
# table(test1$Loan_Status,testpredictions)
# #===================================Overfitting model======lets try a linear kernel instead==
# set.seed(129883)
# ctrl <- trainControl(method="cv",
#                      number = 2,
#                      summaryFunction=twoClassSummary,
#                      classProbs=TRUE)
# 
# # Grid search to fine tune SVM
# grid <- expand.grid(C = c(1,4,6,7,8,0.1,0.3,0.5,.002,.005,.009,100)
# )
# 
# #Train SVM
# colnames(traindata2)
# str(traindata2)
# traindata2$Loan_Status_n<-as.factor(traindata2$Loan_Status_n)
# unique(traindata2$Loan_Status)
# svm.tune <- train(Loan_Status~.,
#                   data=traindata2[,c(7:25)],
#                   method = "svmLinear",
#                   metric="ROC",
#                   tuneGrid = grid,
#                   trControl=ctrl)
# 
# svm.tune
# class(svm.tune)
# 
# #=====================================================
# traindata2$Loan_Status_n<-ifelse(traindata2$Loan_Status=="Y",1,0)
# gg1<-predict(svm.tune,  type="prob")[2]
# gg1<-gg1$Y
# traindata2$svm_pred <- as.numeric(gg1)
# traindata2$svm_pred<-as.numeric(traindata2$svm_pred)
# str(traindata2)
# roc(traindata2$Loan_Status_n~traindata2$svm_pred)
# plot(roc(traindata2$Loan_Status_n~traindata2$svm_pred))
# #=============================Testdata=============
# x2<-test[ , catr]
# colnames(x2)
# x2<-x2[,-1]
# colnames(x2)
# results<-fastDummies::dummy_cols(x2[,-ncol(x2)])
# colnames(results)
# unique(test$Loan_Status)
# #=======================
# sum(is.na(test))
# test[1:10,]
# test1<-cbind(test,results)
# test1$Married_<-0
# gg1<-predict(svm.tune,test1,type="prob")[2]
# gg1<-gg1$Y
# 
# test1$svm_pred<-gg1
# test1$Loan_Status_n<-ifelse(test1$Loan_Status=="Y",1,0)
# 
# #=================================================
# unique(test1$Loan_Status_n)
# roc(test1$Loan_Status_n~test1$svm_pred)
# plot(roc(test1$Loan_Status_n~test1$svm_pred))
# #============================================================
# #==ploy===========================
# set.seed(129883)
# ctrl <- trainControl(method="cv",
#                      number = 2,
#                      summaryFunction=twoClassSummary,
#                      classProbs=TRUE)
# 
# # Grid search to fine tune SVM
# grid <- expand.grid(C = c(1,4,6,7,8,0.1,0.3,0.5,.002,.005,.009,100),scale = 1,degree=1)
# 
# #Train SVM
# colnames(traindata2)
# str(traindata2)
# traindata2$Loan_Status_n<-as.factor(traindata2$Loan_Status_n)
# unique(traindata2$Loan_Status)
# svm.tune <- train(Loan_Status~.,
#                   data=traindata2[,c(7:25)],
#                   method = "svmPoly",
#                   metric="ROC",
#                   tuneGrid = grid,
#                   trControl=ctrl,preProcess = c("pca","scale","center"))
# 
# svm.tune
# class(svm.tune)
# #============================================================
# traindata3<-traindata2
# colnames(traindata3)
# colnames(traindata3)[15]<-"Dependent_3"
# colnames(traindata3)[18]<-"Education_ng"
# m1<-randomForest(Loan_Status_n~.,data=traindata3[,c(7:24,26)],mtry=5,ntree=500)
# m1
# #================================================
# table(traindata3$Loan_Status)
# traindata3$pred<-predict(m1,traindata3,type="class")
# table(traindata3$pred,traindata3$Loan_Status_n)
# #==============================================test=
# levels(test2$Gender_Female)<-levels(traindata3$Gender_Female)
# levels(test2$Gender_)<-levels(traindata3$Gender_)
# levels(test2$Married_No)<-levels(traindata3$Married_No)
# levels(test2$Married_Yes)<-levels(traindata3$Married_Yes)
# levels(test2$Married_)<-levels(traindata3$Married_)
# levels(test2$Dependents_0)<-levels(traindata3$Dependents_0)
# levels(test2$Dependents_1)<-levels(traindata3$Dependents_1)
# levels(test2$Dependents_2)<-levels(traindata3$Dependents_2)
# 
# levels(test2$Dependent_3)<-levels(traindata3$Dependent_3)
# levels(test2$Dependents_)<-levels(traindata3$Dependents_)
# levels(test2$Education_Graduate)<-levels(traindata3$Education_Graduate)
# levels(test2$Education_ng)<-levels(traindata3$Education_ng)
# levels(test2$Self_Employed_No)<-levels(traindata3$Self_Employed_No)
# levels(test2$Self_Employed_Yes)<-levels(traindata3$Self_Employed_Yes)
# levels(test2$Self_Employed_)<-levels(traindata3$Self_Employed_)
# levels(test2$Property_Area_Urban)<-levels(traindata3$Property_Area_Urban)
# levels(test2$Property_Area_Rural)<-levels(traindata3$Property_Area_Rural)
# levels(test2$Property_Area_Semiurban)<-levels(traindata3$Property_Area_Semiurban)
# 
# #=====================================
# test2<-test1
# colnames(test2)[15]<-"Dependent_3"
# colnames(test2)[18]<-"Education_ng"
# test2$rf_pred<-predict(m1,test2,type="class")
# colnames(traindata3[,c(7:24,26)])
# 


