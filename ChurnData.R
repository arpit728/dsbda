churn=read.csv("churn_Data.csv")
churn=churn[,-1]
churn=churn[,-4]
phone$churn=as.factor(churn$Phone)
library("caret")
barplot(table(churn$Churn.),color="blue")
apply(churn,2,function(x)sum(is.na(x)))
trainIndex=createDataPartition(churn$Churn.,p=.7,list = F,times = 1)
trainSet=churn[trainIndex,]
testSet=churn[-trainIndex,]
fit=glm(Churn.~.,data=trainSet,family = "binomial")
summary(fit)
pred=predict(fit,testSet,type="response")
pred=ifelse(pred>-0.5,"1","0")
confusionMatrix(pred,testSet$Churn.)

#tb=table(testSet$Churn.,Class)
#tb
#churn.mod=ifelse(testSet$Churn.="yes",1,0)
#pred_class=churn.mod
#pred_class=churn.mod
#pred_class[pred<-.5]=1-pred
#head(pred)


