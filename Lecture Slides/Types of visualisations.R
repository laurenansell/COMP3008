library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(ISLR)
library(plot3D)
library(glue)
library(datasauRus)

## Barchart

arrest<-gather(USArrests, "Crime", "Value", c(1,2,4)) %>% group_by(Crime) %>% 
  summarise(Total=sum(Value))

ggplot(arrest,aes(x=Crime,y=Total))+geom_bar(stat="identity",fill="red")+
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 14))

## Histogram

##  Simulate data:
set.seed(2)
x <- rnorm(1000)
y <- rnorm(1000)

##  Create cuts:
x_c <- cut(x, 20)
y_c <- cut(y, 20)

##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)

##  Plot as a 3D histogram:
hist3D(z=z, border="black")


ggplot(airquality,aes(x=Temp))+geom_histogram(bins=15,col="black",fill="darkgreen")+
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 14))


## Line graph

airquality<-airquality %>% mutate(Date=make_date(year = 1973,month = Month,day=Day))


ggplot()+geom_line(data=airquality,aes(x=Date,y=Wind),col="blue",linewidth=1.5)+
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 14))

## Heatmap

## usng the same data as the 3D histogram
image2D(z=z, border="black")

##  Simulate data:
x <- c(rep(1,1000))
y <- c(rep(1,1000))

##  Create cuts:
x_c <- cut(x, 20)
y_c <- cut(y, 20)

##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)

image2D(z=z, border="black")

## Boxplots

box_data<-datasaurus_dozen %>% filter(dataset==c("slant_up","slant_down"))


ggplot()+geom_boxplot(data = box_data,aes(x=x, fill = dataset))+
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 14),legend.position = "none")

ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot()+
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 14),legend.position = "none")

## Word cloud

wordcloud_good_data<-read.delim("./Lecture Slides/book_1.txt",header = FALSE)

wordcloud_good_data <- wordcloud_good_data %>%
  select(V1) %>% 
  unnest_tokens(word, V1)

wordcloud_good_data <- wordcloud_good_data %>% count(word, sort=TRUE)

set.seed(2)

wordcloud(words = wordcloud_good_data$word, freq = wordcloud_good_data$n, 
          min.freq = 1,          
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))


wordcloud_bad_data<-read.csv("./Lecture Slides/likert_example.csv")

wordcloud_bad_data <- wordcloud_bad_data %>%
  select(Q1) %>% 
  unnest_tokens(word, Q1)

wordcloud_bad_data <- wordcloud_bad_data %>% count(word, sort=TRUE)

set.seed(2)

wordcloud(words = wordcloud_bad_data$word, freq = wordcloud_bad_data$n, 
          min.freq = 1,          
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))



## Table

norm<-rnorm(100)
unif<-runif(100)
exp<-rexp(100)
gamma<-rgamma(100,1)

mean(norm)
sd(nrom)

mean(unif)
sd(unif)

mean(exp)
sd(exp)

mean(gamma)
sd(gamma)

datasaurus_dozen %>% 
  group_by(dataset) %>% 
  summarize(
    mean_x    = mean(x),
    mean_y    = mean(y),
    std_dev_x = sd(x),
    std_dev_y = sd(y),
    corr_x_y  = cor(x, y)
  )


