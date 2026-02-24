library(tidyverse)
library(ISLR2)
library(mdsr)
library(tidymodels)
library(yardstick)
library(glmnet)


## Regression
pokemon_data<-read.csv("./Lecture Code/pokemon_stats_2025.csv")

pokemon_data<-pokemon_data %>% select(height,weight,base_experience,hp,
                                      speed,special_attack,special_defense,
                                      attack,defense)
base_experience_model<-lm(base_experience~.-base_experience,
                          data = pokemon_data)

summary(base_experience_model)

## Back elimination

pokemon_data <- pokemon_data %>% select(weight,base_experience,hp,
                                        speed,special_attack,special_defense,
                                        attack,defense)

base_experience_model<-lm(base_experience~.-base_experience,
                          data = pokemon_data)

summary(base_experience_model)

pokemon_data <- pokemon_data %>% select(base_experience,hp,
                                        speed,special_attack,special_defense,
                                        attack,defense)

base_experience_model<-lm(base_experience~.-base_experience,
                          data = pokemon_data)

summary(base_experience_model)

## Lasso regression

hitter<-na.omit(Hitters)

x<-model.matrix(Salary~.,hitter)[,-1]
y<-hitter$Salary

set.seed(2)

train<-sample(1:nrow(x),nrow(x)/2)
test<-(-train)
y.test<-y[test]

lasso_model<-glmnet(x[train,],y[train],alpha=1)

plot(lasso_model)

cv.out<-cv.glmnet(x[train,],y[train],alpha=1)

plot(cv.out)

best_lambda<-cv.out$lambda.min

lasso.pred<-predict(lasso_model,s=best_lambda,newx = x[test,])

mean((lasso.pred-y.test)^2)

lasso.coeff<-predict(lasso_model,type = "coefficients",s=best_lambda)[1:20,]

lasso.coeff


## logistic regression

url<-"http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"


census<-read_csv(url,col_names = c("age","workclass","fnlwgy","education",
                                   "education_1","martial_status","occupation",
                                   "relationship","race","sex","capital_gains",
                                   "capital_loss","hours_per_week","native_country",
                                   "income"))

glimpse(census)

census$income<-as.factor(census$income)

census_parts<-census %>% initial_split(prop=0.8)

train<-census_parts %>% training()
test<-census_parts %>% testing()

logistic_model<-logistic_reg(mode = "classification") %>% set_engine("glm") %>% 
  fit(income~capital_gains,data=train)

train_plus<-train %>% mutate(high_earners=as.integer(income==">50K"))

ggplot(train_plus,aes(x=capital_gains,y=high_earners))+
  geom_count(position = position_jitter(width=0,height=0.05),alpha=0.5)+
  geom_smooth(method = "glm",method.args=list(family="binomial"),
              color="skyblue",lty=2,se=FALSE)+
  geom_hline(aes(yintercept = 0.5),linetype=3)+scale_x_log10(labels=scales::dollar)
