#1.25.20

#Logistic Regressions (with penalties) for High Cost Patients 
#Train and Test set derived from Preprocess.R

library(caret)
library(pROC)
library(tidyverse)

# Set formula 

x_form = "gender+ethnicity+age_grp+sponservice+index"
y_form = "perc_95_total"
form = as.formula(paste(y_form, "~", x_form))

#Base Model 

base_1<-glm(form,data=pts,family=binomial)
summary(base_1)

base_1_pred<-predict(base_1,test,type="response")
table(as.numeric(base_1_pred>0.5))

confusionMatrix(data = as.numeric(base_1_pred>0.5), test$perc_95_total)

test$prob <- base_1_pred

g <- roc(perc_95_total ~ prob, data = test)
g
plot(g)  


#Penalized Models

library(glmnet)
library(foreach)

x<-model.matrix(as.formula(paste("~", x_form)),train)[,-1]
y<-factor(train$perc_95_total)

x.x<-model.matrix(as.formula(paste( "~", x_form)),test)[,-1]
y.y<-factor(test$perc_95_total)

#lasso
lasscv<-cv.glmnet(x, y, family = "binomial", nfold = 10, type.measure = "deviance", paralle = TRUE, alpha = 1)
lass<-glmnet(x, y, family = "binomial", lambda = lasscv$lambda.1se, alpha = 1)
roc(y.y,as.numeric(predict(lass,x.x,type="response")))

#ridge
rdcv<-cv.glmnet(x, y, family = "binomial", nfold = 10, type.measure = "deviance", paralle = TRUE, alpha = 0)
rd<-glmnet(x, y, family = "binomial", lambda = rdcv$lambda.1se, alpha = 1)
roc(y.y,as.numeric(predict(rd,x.x,type="response")))

#elastic net 
#Not currently converging - need to adapt 
a <- seq(0.1, 0.9, 0.05)
search<-foreach(i = a, .combine = rbind) %dopar% {
  cv<-cv.glmnet(x, y, family = "binomial", nfold = 10, type.measure = "deviance", paralle = TRUE, alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}

cvb <- search[search$cvm == min(search$cvm), ]
en <- glmnet(x, y, family = "binomial", lambda = cvb$lambda.1se, alpha = cvb$alpha)
en <- glmnet(x, y, family = "binomial", lambda = cvb$lambda.1se, alpha = 0.5)

roc(y.y, as.numeric(predict(en, x.x, type = "response")))


gm<-glmnet(x,y,family="binomial",alpha=0.5)
plot(gm,xvar="lambda")
plot(gm,xvar="dev")




