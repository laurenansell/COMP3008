library(tidyverse)

WIMS_data<-read.csv("./Lecture Code/Up_Stream.csv")

WIMS_data<-WIMS_data[,-c(1,2)]
WIMS_data<-na.omit(WIMS_data)

real<-WIMS_data$pH

library(rpart)
library(rpart.plot)

WIMS_tree<-rpart(pH~.-pH,data = WIMS_data)

rpart.plot(WIMS_tree,box.palette = "RdBu",shadow.col = "grey",nn=TRUE)

summary(WIMS_tree)

library(tree)

tree.WIMS<-tree(pH~.-pH,data = WIMS_data)
plot(tree.WIMS)
text(tree.WIMS,pretty = 0)

summary(tree.WIMS)

tree.WIMS

library(randomForest)

rf_WIMS<-randomForest(pH~.-pH,WIMS_data,mtry=7)

rf_WIMS.pred<-predict(rf_WIMS,WIMS_data,type = "response")

library(gbm)

GBTree_WIMS<-gbm(pH~.-pH,data = WIMS_data,n.trees = 103)

GBTree_WIMS.pred<-predict(GBTree_WIMS,n.trees = 5,type = "response")
GBTree_WIMS.pred1<-predict(GBTree_WIMS,n.trees = 103,type = "response")

library(xgboost)

data_matrix<-xgb.DMatrix(data = as.matrix(WIMS_data),label = WIMS_data$pH)

params<-list(objective="reg:squarederror",max_depth=5,eta=0.1,nrounds=200)

xgb_model<-xgb.train(params,data_matrix,nrounds = 200)

xgb.pred<-predict(xgb_model,data_matrix)

## plotting

plot(real)
plot(rf_WIMS.pred)


library(scales)
library(neuralnet)
library(MASS)


nn_model<-neuralnet(pH~.-pH,data = WIMS_data,
                    hidden = c(6),
                    linear.output = TRUE)

pr.nn<-compute(nn_model,WIMS_data)

plot(nn_model)

plot(WIMS_data$pH,pr.nn$net.result)
