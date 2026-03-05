library(tidyverse)
library(factoextra)

credit_data<-read.csv("./Lecture Code/creditcard.csv")

## K-means

kmeans_model<-kmeans(credit_data,50,iter.max = 20,nstart = 1)

kmeans_model$size

kmeans_model$centers

fviz_cluster(kmeans_model,credit_data,ellipse.type = "norm")


## hierarchical

credit_data_sample<-credit_data %>% slice_sample(prop = 0.05)
credit_data_sample<-credit_data_sample[,1:10]

clusters<-hclust(dist(credit_data_sample),method = "complete")

plot(clusters)

abline(h=50000,col="purple")

fit<-cutree(clusters,k=5)

plot(clusters)

rect.hclust(clusters,k=5,border="darkorange")

cities_data<-read.csv("./Lecture Code/cities_data.csv")

cities_pca<-princomp(cities_data[,-1],cor = TRUE)

summary(cities_pca)

plot(cities_pca)

scores<-cities_pca$scores
new_var<-cities_pca$scores

new_var_df<-data.frame(new_var)

cities_data_2<-separate(cities_data,"City_Name",c("Name","Other_Info"),
                        extra = "merge")

new_var_df$City_Short<-cities_data_2$Name

ggplot(new_var_df,aes(x=Comp.1,y=Comp.2,label=City_Short))+
  geom_text(col="blue",size=3)+
  coord_fixed(ratio=1)

biplot(cities_pca)


## association rule mining

library(arules)
library(arulesViz)
library(igraph)
library(visNetwork)

data<-read.csv("./Lecture Code/Groceries-data.csv",stringsAsFactors = FALSE)

transaction_list<-split(data$itemDescription,data$Member_number)
transactions<-as(transaction_list,"transactions")

summary(transactions)

itemFrequencyPlot(transactions,topN=10,type="absolute",col="steelblue")

rules<-apriori(transactions,parameter = list(supp=0.001,conf=0.3))

cat("Number of rule generated:",length(rules),"\n")

if(length(rules)>0){inspect(rules[1:min(10,length(rules))])} else{cat("No rules were generated. Try lowering support or confidence.\n")}







