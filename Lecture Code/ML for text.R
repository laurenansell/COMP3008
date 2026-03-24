devtools::install_github("EmilHvitfeld/scotus")

library(tidyverse)
library(scotus)
library(tidymodels)
library(textrecipes)
library(discrim)
library(naivebayes)
library(keras3)
library(tensorflow)
library(pbapply)

scotus_filtered %>% as_tibble()

scotus_filtered %>% mutate(year=as.numeric(year),
                           year=10*(year %/% 10)) %>% 
  count(year) %>% 
  ggplot(aes(x = year,y=n))+geom_col()

set.seed(2)

scotus_split<-scotus_filtered %>% mutate(year=as.numeric(year),
                                         text=str_remove_all(text,"'")) %>% 
  initial_split()

scotus_training<-training(scotus_split)
scotus_test<-testing(scotus_split)

scotus_reg<-recipe(year~text,data = scotus_training) %>% 
  step_tokenize(text) %>% 
  step_tokenfilter(text,max_tokens = 1e3) %>% 
  step_tfidf(text) %>% 
  step_normalize(all_predictors())

scotus_reg

scotus_prep<-prep(scotus_reg)
scotus_bake<-bake(scotus_prep,new_data = NULL)

dim(scotus_bake)

scotus_wf<-workflow() %>% add_recipe(scotus_reg)

scotus_wf

svm_spec<-svm_linear() %>% 
  set_mode("regression") %>% 
  set_engine("LiblineaR")


svm_fit<-scotus_wf %>% 
  add_model(svm_spec) %>% 
  fit(data = scotus_training)


svm_fit %>% extract_fit_parsnip() %>% tidy() %>% 
  arrange(-estimate)

svm_fit$pre

## Classification

complaints<-read.csv("./Lecture Code/complaints_subset.csv")
  
complaints$Consumer.complaint.narrative %>% 
  str_extract_all("\\{\\$[0-9\\.]*\\}") %>% 
  compact() %>% head()


complaints2class<-complaints %>% 
  mutate(Product=factor(ifelse(
    Product==paste("Credit reporting, credit repair services,","or other personal consumer reports"),
    "Credit","Other"
  )))

complaints_split<-initial_split(complaints2class,strata = Product)

complaints_train<-training(complaints_split)
complaints_test<-testing(complaints_split)

complaints_rec<-recipe(Product~Consumer.complaint.narrative,data = complaints_train)

complaints_rec<-complaints_rec %>% 
  step_tokenize(Consumer.complaint.narrative) %>% 
  step_tokenfilter(Consumer.complaint.narrative, max_tokens = 1e3) %>% 
  step_tfidf(Consumer.complaint.narrative)


complaint_wf<-workflow() %>% 
  add_recipe(complaints_rec)

nb_spec<-naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("naivebayes")

nb_spec

nb_fit<-complaint_wf %>% 
  add_model(nb_spec) %>% 
  fit(data = complaints_train)

nb_fit$pre

## NN

data<-read.csv("./Lecture Code/neural network data.csv")


data %>% ggplot(aes(nchar(Text)))+geom_histogram()

max_words<-30000
max_length<-30

data_rec<-recipe(Book~Text, data=data) %>% 
  step_tokenize(Text) %>%
  step_tokenfilter(Text,max_tokens=max_words) %>% 
  step_sequence_onehot(Text,sequence_length=max_length)
  
data_rec

data_prep<-prep(data_rec)  

data_bake<-bake(data_prep,new_data = NULL,composition = "matrix")  

dim(data_bake)  

install_keras()

reticulate::use_condaenv("r-tensorflow")

dense_model<-keras_model_sequential() %>% 
  layer_embedding(input_dim = max_words+1,
                  output_dim = 12) %>%
  layer_flatten() %>% 
  layer_dense(units = 16,activation = "relu") %>% 
  layer_dense(units = 1,activation = "sigmoid")

dense_model

dense_model %>% compile(
  optimizer="adam",
  loss="binary_crossentropy",
  metrics=c("accuracy")
)

dense_history<-dense_model %>% fit(x=data_bake,y=data_bake$Book,
                                   batch_size(512),epochs(10),
                                   validation_split(0.125),verbose=FALSE)

plot(dense_history)


predicitions<-dense_model %>% predict(data_bake) %>% '>(0.5)' %>% k_cast("int32")











  








  
















