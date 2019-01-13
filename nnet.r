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
colnames(train1)
#install.packages("nnet")
require("nnet")
m1<-nnet(y_prob~.,data=train1,maxit=100,size=10)


#=============================================
predict(m1,validate)
validate$out<-as.numeric(predict(m1,validate))
validate$class1<-ifelse(validate$class=="anomaly",1,0)
str(validate)
validate$out1<-ifelse(validate$out>=0.5,1,0)
roc(validate$out1,validate$class1)